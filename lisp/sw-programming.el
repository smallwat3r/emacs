;;; sw-programming.el --- Programming language support -*- lexical-binding: t -*-

;;; Commentary:
;; Language modes, formatting, and syntax support.

;;; Code:

;; Enable ANSI colors in compilation buffers
(add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)

;;; Tree-sitter

(use-package treesit
  :ensure nil
  :demand t
  :custom
  (treesit-font-lock-level 2)
  :config
  (setq major-mode-remap-alist
        '((c-mode . c-ts-mode)
          (c++-mode . c++-ts-mode)
          (cmake-mode . cmake-ts-mode)
          (css-mode . css-ts-mode)
          (go-mode . go-ts-mode)
          (html-mode . html-ts-mode)
          (java-mode . java-ts-mode)
          (js-mode . js-ts-mode)
          (json-mode . json-ts-mode)
          (python-mode . python-ts-mode)
          (ruby-mode . ruby-ts-mode)
          (rust-mode . rust-ts-mode)
          (sh-mode . bash-ts-mode)
          (toml-mode . toml-ts-mode)
          (yaml-mode . yaml-ts-mode))))

;;; Formatting

(use-package apheleia
  :ensure (:wait t)
  :demand t
  :config
  ;; Python formatters (black/isort read pyproject.toml automatically)
  (setf (alist-get 'black apheleia-formatters) '("black" "--quiet" "-"))
  (setf (alist-get 'isort apheleia-formatters) '("isort" "--profile" "black" "-"))

  ;; Other formatters
  (setf (alist-get 'shfmt apheleia-formatters)
        '("shfmt" "-i" "2" "-ci" "-bn" "-"))
  (setf (alist-get 'prettier-js apheleia-formatters)
        '("prettier" "--stdin-filepath" filepath "--print-width" "120"))

  ;; Mode associations
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(isort black))
  (setf (alist-get 'python-mode apheleia-mode-alist) '(isort black))
  (setf (alist-get 'bash-ts-mode apheleia-mode-alist) 'shfmt)
  (setf (alist-get 'sh-mode apheleia-mode-alist) 'shfmt))

;; Region formatters by mode (mirrors apheleia config where applicable)
(defvar sw-region-formatters
  (let (alist)
    (dolist (entry
             '(((python-mode python-ts-mode)
                "black" "--quiet" "-")
               ((go-mode go-ts-mode)
                "gofmt")
               ((sh-mode bash-ts-mode)
                "shfmt" "-i" "2" "-ci" "-bn" "-")
               ((js-mode js-ts-mode)
                "prettier" "--stdin-filepath" "file.js")
               ((typescript-ts-mode)
                "prettier" "--stdin-filepath" "file.ts")
               ((tsx-ts-mode)
                "prettier" "--stdin-filepath" "file.tsx")
               ((c-mode c-ts-mode)
                "clang-format")
               ((html-mode html-ts-mode web-mode)
                "prettier" "--stdin-filepath" "file.html")))
      (let ((cmd (cdr entry)))
        (dolist (mode (car entry))
          (push (cons mode cmd) alist))))
    (nreverse alist))
  "Alist mapping major modes to region formatter commands.
Each value is a list where car is the command and cdr is the arguments.")

(defun sw--string-min-indent (str)
  "Return minimum indentation (spaces) of non-blank lines in STR."
  (let ((min most-positive-fixnum))
    (dolist (line (split-string str "\n"))
      (when (string-match "^\\( *\\)[^ \t\n]" line)
        (setq min (min min (length (match-string 1 line))))))
    (if (= min most-positive-fixnum) 0 min)))

(defun sw--string-reindent (str old-indent new-indent)
  "Change indentation of STR from OLD-INDENT to NEW-INDENT spaces."
  (let ((prefix (make-string new-indent ?\s))
        (re (concat "^" (make-string old-indent ?\s))))
    (mapconcat (lambda (line)
                 (if (string-match-p "^[ \t]*$" line)
                     line
                   (concat prefix (replace-regexp-in-string re "" line t t))))
               (split-string str "\n" nil)
               "\n")))

(defun sw-format-region ()
  "Format the current region using language-specific tools or eglot.
For Python, handles indented code by dedenting before formatting."
  (interactive)
  (unless (use-region-p)
    (user-error "No region selected"))
  (let* ((beg (region-beginning))
         (end (region-end))
         (formatter (alist-get major-mode sw-region-formatters)))
    (cond
     ((derived-mode-p 'emacs-lisp-mode 'lisp-mode)
      (indent-region beg end)
      (message "Formatted region (indent-region)"))
     (formatter
      (let* ((cmd (car formatter))
             (args (cdr formatter))
             (input (buffer-substring-no-properties beg end))
             (indent (sw--string-min-indent input))
             (dedented (sw--string-reindent input indent 0))
             (err-file (make-temp-file "fmt-err"))
             (exit-code nil)
             output err-msg)
        (unwind-protect
            (progn
              (setq output
                    (with-temp-buffer
                      (insert dedented)
                      (setq exit-code
                            (apply #'call-process-region
                                   (point-min) (point-max)
                                   cmd t
                                   (list t err-file) nil args))
                      (when (zerop exit-code)
                        (buffer-string))))
              (setq err-msg
                    (unless (zerop exit-code)
                      (with-temp-buffer
                        (insert-file-contents err-file)
                        (string-trim (buffer-string))))))
          (ignore-errors (delete-file err-file)))
        (if output
            (save-excursion
              (delete-region beg end)
              (goto-char beg)
              (insert (sw--string-reindent output 0 indent))
              (message "Formatted region (%s)" cmd))
          (message "%s failed: %s" cmd (or err-msg "unknown error")))))
     ((and (fboundp 'eglot-managed-p) (eglot-managed-p))
      (eglot-format beg end)
      (message "Formatted region (eglot)"))
     (t
      (message "No region formatter available")))))

;;; Python

(use-package python
  :ensure nil
  :custom
  (python-indent-offset 4)
  (python-shell-interpreter "python3")
  (python-shell-completion-native-enable nil)
  :config
  (defun sw-python-toggle-fstring ()
    "Toggle f-string prefix on the current Python string literal.
When point is inside a string, adds or removes the `f' prefix.
Handles combined prefixes like `rf' or `fr' correctly."
    (interactive)
    (let* ((ppss (syntax-ppss))
           (in-string (nth 3 ppss))
           (string-start (nth 8 ppss)))
      (when in-string
        (save-excursion
          (goto-char string-start)
          ;; For triple-quoted strings, syntax-ppss reports
          ;; string-start at the last quote of the opening
          ;; delimiter. Move back to the first quote.
          (let ((q (char-after)))
            (when (and (memq q '(?\" ?'))
                       (> (point) 1)
                       (eq (char-before) q)
                       (eq (char-before (1- (point))) q))
              (backward-char 2)))
          (let ((start (point)))
            (cond
             ;; Immediate prefix char is f, remove it
             ((eq (char-before start) ?f)
              (delete-char -1))
             ;; Combined prefix fr (raw f-string)
             ((and (> start 1)
                   (eq (char-before (1- start)) ?f)
                   (memq (char-before start) '(?r ?R)))
              (goto-char (1- start))
              (delete-char -1))
             ;; No f-prefix, add it
             (t
              (insert "f"))))))))

  (defun sw-python-repl-toggle ()
    "Toggle a project-scoped Python REPL using the venv."
    (interactive)
    (let* ((python
            (or (and (fboundp 'pet-executable-find)
                     (pet-executable-find "python"))
                python-shell-interpreter))
           (python-shell-interpreter python)
           (proc-name
            (python-shell-get-process-name 'project))
           (buf (get-buffer
                 (format "*%s*" proc-name))))
      (if-let ((win (and buf (get-buffer-window buf))))
          (delete-window win)
        (run-python nil 'project t))))

  (defun sw-python-isort ()
    "Run isort on the current buffer."
    (interactive)
    (let ((isort (or (executable-find "isort")
                     (and (fboundp 'pet-executable-find)
                          (pet-executable-find "isort")))))
      (unless isort
        (user-error "isort not found"))
      (apheleia-format-buffer 'isort))))

;; Pytest integration
(use-package python-pytest
  :commands (python-pytest
             python-pytest-file
             python-pytest-file-dwim
             python-pytest-run-def-or-class-at-point
             python-pytest-repeat
             python-pytest-dispatch))

;; Virtual environment detection
(use-package pet
  :hook (python-base-mode . pet-mode)
  :config
  (add-hook 'python-base-mode-hook
            (lambda ()
              (when-let ((python (pet-executable-find "python")))
                ;; Eglot/basedpyright
                (setq-local eglot-workspace-configuration
                            `(:basedpyright (:pythonPath ,python)))))))

;;; Go

(use-package go-ts-mode
  :ensure nil
  :custom
  (go-ts-mode-indent-offset 4)
  :hook (go-ts-mode . (lambda () (setq-local indent-tabs-mode t))))

;; Go test integration
(use-package gotest
  :commands (go-test-current-test
             go-test-current-file
             go-test-current-project
             go-run))

;;; Rust

(use-package rust-ts-mode
  :ensure nil
  :mode "\\.rs\\'")

(use-package cargo
  :hook (rust-ts-mode . cargo-minor-mode))

;;; JavaScript/TypeScript

(use-package js
  :ensure nil
  :custom
  (js-indent-level 2))

(use-package typescript-ts-mode
  :ensure nil
  :mode ("\\.ts\\'" "\\.tsx\\'")
  :custom
  (typescript-ts-mode-indent-offset 2))

;;; Web

(use-package web-mode
  :mode ("\\.html?\\'" "\\.vue\\'" "\\.svelte\\'")
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-script-padding 2)
  (web-mode-style-padding 2)
  (web-mode-enable-auto-pairing nil))

;;; JSON/YAML

(use-package json-ts-mode
  :ensure nil
  :mode "\\.json\\'"
  :hook (json-ts-mode . (lambda () (setq-local tab-width 2))))

(use-package yaml-ts-mode
  :ensure nil
  :mode "\\.ya?ml\\'")

;;; Markdown

(use-package markdown-mode
  :mode ("\\.md\\'" "\\.markdown\\'")
  :custom
  (markdown-fontify-code-blocks-natively t)
  (markdown-command "pandoc"))

;;; Shell

(use-package sh-script
  :ensure nil
  :custom
  (sh-basic-offset 2)
  (sh-indentation 2)
  :hook (sh-mode . (lambda ()
                     (setq-local indent-tabs-mode nil))))

;;; Docker

(use-package dockerfile-mode
  :mode "Dockerfile\\'")

(use-package docker
  :commands docker)

;;; Terraform

(use-package terraform-mode
  :mode "\\.tf\\'")

;;; SQL

(use-package sql
  :ensure nil
  :mode (("\\.sql\\'" . sql-mode)
         ("\\.mysql\\'" . sql-mode)
         ("\\.pgsql\\'" . sql-mode))
  :custom
  (sql-mysql-options '("--ssl-mode=DISABLED"))
  (sql-mysql-login-params '((user :default "root")
                            password database
                            (server :default "127.0.0.1")
                            (port :default 3306)))
  (sql-postgres-login-params '((user :default "postgres")
                               password database
                               (server :default "127.0.0.1")
                               (port :default 5432)))
  :config
  (defun sw-sql-repl-toggle ()
    "Toggle a SQL REPL window."
    (interactive)
    (let ((buf (sql-find-sqli-buffer)))
      (if-let ((win (and buf
                         (get-buffer-window buf))))
          (delete-window win)
        (sql-product-interactive)))))

;;; Emacs Lisp

(defun sw-ielm-toggle ()
  "Toggle an IELM (Emacs Lisp REPL) window."
  (interactive)
  (let ((buf (get-buffer "*ielm*")))
    (if-let ((win (and buf (get-buffer-window buf))))
        (delete-window win)
      (ielm))))

(use-package package-lint
  :commands package-lint-current-buffer)

;;; Misc

(use-package lua-mode
  :mode "\\.lua\\'")

(use-package nginx-mode
  :mode ("nginx\\.conf\\'" "/nginx/.+\\.conf\\'"))

;;; Editorconfig

(use-package editorconfig
  :ensure (:wait t)
  :demand t
  :config
  (setq editorconfig-exclude-modes '(tramp-mode))
  (editorconfig-mode 1))

(provide 'sw-programming)
;;; sw-programming.el ends here
