;;; sw-theme.el --- Theme configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Theme and appearance settings.

;;; Code:

;; Frame appearance
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
   '(("TODO" . "#cc9393")
     ("FIXME" . "#cc9393")
     ("HACK" . "#d0bf8f")
     ("NOTE" . "#7cb8bb")
     ("DEPRECATED" . "#afd8af"))))

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
