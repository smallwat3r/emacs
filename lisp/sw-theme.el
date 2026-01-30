;;; sw-theme.el --- Theme configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Theme and appearance settings.

;;; Code:

;;; Fonts (primary font set in early-init.el)

(setq-default line-spacing 2)

;;; Frame appearance
(setq-default left-fringe-width 8
              right-fringe-width 8)

;; Window dividers
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(window-divider-mode 1)

;; Themes
(use-package creamy-theme
  :demand t
  :config
  (load-theme 'creamy t))

;; Highlight TODO/FIXME keywords
(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :custom
  (hl-todo-keyword-faces
   '(("TODO" . "#ff6c6b")
     ("FIXME" . "#ff6c6b")
     ("HACK" . "#b8860b")
     ("NOTE" . "#4682b4")
     ("DEPRECATED" . "#2e8b57"))))

;; Rainbow delimiters for better paren visibility
(use-package rainbow-delimiters
  :hook ((emacs-lisp-mode lisp-mode) . rainbow-delimiters-mode))

;; Highlight numbers in code
(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode))

;; Symbol overlay for manual highlighting (SPC c h)
(use-package symbol-overlay
  :commands (symbol-overlay-put symbol-overlay-remove-all))

(provide 'sw-theme)
;;; sw-theme.el ends here
