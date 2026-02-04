;;; sw-theme.el --- Theme configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Theme and appearance settings.

;;; Code:

;;; Fonts (font variables defined in early-init.el)

;; Monospace faces
(set-face-attribute 'fixed-pitch nil :family sw-font-family :height (* sw-font-size 10))
(set-face-attribute 'fixed-pitch-serif nil :family sw-font-serif :height (* sw-font-size 10))

;; Variable-pitch face
(set-face-attribute 'variable-pitch nil
                    :family sw-font-variable-pitch
                    :height (* sw-font-variable-pitch-size 10))

;; Symbol and emoji fontsets
(defun sw-setup-fontsets ()
  "Configure fontsets for symbols and emoji."
  ;; Resolve fonts from fallback lists now that display is available
  (setq sw-font-symbol (sw-first-available-font sw-font-symbol-fallbacks))
  (setq sw-font-emoji (sw-first-available-font sw-font-emoji-fallbacks))
  (when sw-font-symbol
    (set-fontset-font t 'symbol sw-font-symbol nil 'prepend)
    (set-fontset-font t 'mathematical sw-font-symbol nil 'prepend))
  (when sw-font-emoji
    (set-fontset-font t 'emoji sw-font-emoji nil 'prepend)))

(if (daemonp)
    (add-hook 'server-after-make-frame-hook #'sw-setup-fontsets)
  (add-hook 'after-init-hook #'sw-setup-fontsets))

(setq-default line-spacing 2)

;;; Frame appearance
(setq-default left-fringe-width 0
              right-fringe-width 0)

;; Window dividers
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(window-divider-mode 1)

;; Themes
(use-package creamy-theme
  :ensure (:wait t)
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

;; Warm colors for nighttime coding
(use-package warm-mode
  :ensure (:host github :repo "smallwat3r/emacs-warm-mode")
  :init
  (setq warm-mode-refresh-packages
        '(magit org org-modern diredfl diff-hl corfu embark marginalia consult
          vertico markdown-mode web-mode evil-goggles symbol-overlay pdf-tools))
  :custom
  (warm-mode-warmth 0.25)
  (warm-mode-dim 0.9))

(provide 'sw-theme)
;;; sw-theme.el ends here
