;;; sw-programming.el --- Programming language support -*- lexical-binding: t -*-

;;; Commentary:
;; Language modes, formatting, and syntax support.

;;; Code:

;;; Tree-sitter (built-in since Emacs 29)

(use-package treesit
  :ensure nil
  :demand t
  :custom
  (treesit-font-lock-level 4))

;; Auto-install and remap to tree-sitter modes
(use-package treesit-auto
  :ensure (:wait t)
  :demand t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode 1))

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
  '((python-mode    . ("black" "--quiet" "-"))
    (python-ts-mode . ("black" "--quiet" "-"))
    (go-mode        . ("gofmt"))
    (go-ts-mode     . ("gofmt"))
    (sh-mode        . ("shfmt" "-i" "2" "-ci" "-bn" "-"))
    (bash-ts-mode   . ("shfmt" "-i" "2" "-ci" "-bn" "-"))
    (js-mode        . ("prettier" "--stdin-filepath" "file.js"))
    (js-ts-mode     . ("prettier" "--stdin-filepath" "file.js"))
    (typescript-ts-mode . ("prettier" "--stdin-filepath" "file.ts"))
    (tsx-ts-mode    . ("prettier" "--stdin-filepath" "file.tsx"))
    (c-mode         . ("clang-format"))
    (c-ts-mode      . ("clang-format"))
    (html-mode      . ("prettier" "--stdin-filepath" "file.html"))
    (html-ts-mode   . ("prettier" "--stdin-filepath" "file.html"))
    (web-mode       . ("prettier" "--stdin-filepath" "file.html")))
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
     (formatter
      (let* ((cmd (car formatter))
             (args (cdr formatter))
             (input (buffer-substring-no-properties beg end))
             (indent (sw--string-min-indent input))
             (dedented (sw--string-reindent input indent 0))
             (output (with-temp-buffer
                       (insert dedented)
                       (when (zerop (apply #'call-process-region
                                           (point-min) (point-max) cmd t t nil args))
                         (buffer-string)))))
        (if output
            (save-excursion
              (delete-region beg end)
              (goto-char beg)
              (insert (sw--string-reindent output 0 indent))
              (message "Formatted region (%s)" cmd))
          (message "%s formatting failed" cmd))))
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
          (cond
           ;; Immediate prefix char is f/F, remove it
           ((memq (char-before string-start) '(?f ?F))
            (delete-char -1))
           ;; Combined prefix like rf/fr
           ((and (> string-start 1)
                 (memq (char-before (1- string-start)) '(?f ?F))
                 (memq (char-before string-start) '(?r ?R ?b ?B ?u ?U)))
            (goto-char (1- string-start))
            (delete-char -1))
           ;; No f-prefix, add it
           (t
            (goto-char string-start)
            (insert "f"))))))

  (defun sw-python-isort ()
    "Run isort on the current buffer."
    (interactive)
    (let ((isort (or (executable-find "isort")
                     (and (fboundp 'pet-executable-find)
                          (pet-executable-find "isort")))))
      (unless isort
        (user-error "isort not found"))
      (apheleia-format-buffer 'isort)))))

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

;;; Misc modes

(use-package lua-mode
  :mode "\\.lua\\'")

(use-package nginx-mode
  :mode ("nginx\\.conf\\'" "/nginx/.+\\.conf\\'"))

;;; Editorconfig

(use-package editorconfig
  :ensure (:wait t)
  :demand t
  :config
  (editorconfig-mode 1))

(provide 'sw-programming)
;;; sw-programming.el ends here
