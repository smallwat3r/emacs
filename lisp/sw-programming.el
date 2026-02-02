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
            (insert "f"))))))))

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
