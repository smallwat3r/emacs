;;; sw-evil.el --- Evil mode configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Vim emulation with Evil mode and related packages.

;;; Code:

(use-package evil
  :ensure (:wait t)
  :demand t
  :custom
  (evil-want-integration t)
  (evil-want-keybinding nil)
  (evil-want-C-u-scroll t)
  (evil-want-C-i-jump t)
  (evil-want-Y-yank-to-eol t)
  (evil-want-fine-undo t)
  (evil-undo-system 'undo-redo)
  (evil-symbol-word-search t)
  (evil-split-window-below t)
  (evil-vsplit-window-right t)
  (evil-respect-visual-line-mode t)
  (evil-search-module 'evil-search)
  (evil-kbd-macro-suppress-motion-error t)  ; don't abort macros on motion errors
  (evil-ex-interactive-search-highlight 'selected-window)  ; only highlight in current window
  :config
  (evil-mode 1)

  ;; Don't copy visual selection to clipboard on every movement (perf on Wayland)
  (setq evil-visual-update-x-selection-p nil)

  ;; Slow down search highlighting in large/folded buffers to prevent lag
  (defun sw/slow-down-evil-highlighting ()
    (setq-local evil-ex-hl-update-delay 0.25))
  (dolist (hook '(magit-mode-hook so-long-minor-mode-hook))
    (add-hook hook #'sw/slow-down-evil-highlighting))

  ;; Cursor colors by state
  (setq evil-default-state-cursor  '(box "cyan3")
        evil-normal-state-cursor   '(box "cyan3")
        evil-insert-state-cursor   '(bar "green3")
        evil-visual-state-cursor   '(box "OrangeRed2")
        evil-replace-state-cursor  '(hbar "red2")
        evil-operator-state-cursor '(box "red2")))

;; Evil keybindings for many modes
(use-package evil-collection
  :ensure (:wait t)
  :after evil
  :demand t
  :custom
  (evil-collection-magit-want-horizontal-movement t)
  :config
  (evil-collection-init))

;; Visual hints for operations
(use-package evil-goggles
  :after evil
  :hook (sw/first-input . evil-goggles-mode)
  :custom
  (evil-goggles-duration 0.15)
  (evil-goggles-pulse nil)
  :config
  (evil-goggles-use-diff-refine-faces))

;; Two-character search
(use-package evil-snipe
  :after evil
  :hook ((sw/first-input . evil-snipe-mode)
         (sw/first-input . evil-snipe-override-mode))
  :custom
  (evil-snipe-scope 'visible)
  (evil-snipe-repeat-scope 'visible)
  (evil-snipe-spillover-scope nil)
  :config
  ;; Use n/N to repeat snipes (in addition to ;/,)
  (define-key evil-snipe-parent-transient-map "n" #'evil-snipe-repeat)
  (define-key evil-snipe-parent-transient-map "N" #'evil-snipe-repeat-reverse))

;; Comment with gc
(use-package evil-nerd-commenter
  :ensure (:wait t)
  :after evil
  :demand t)

;; Surround text objects
(use-package evil-surround
  :ensure (:wait t)
  :after evil
  :demand t
  :config
  (global-evil-surround-mode 1))

;; Search for visual selection with * and #
(use-package evil-visualstar
  :ensure (:wait t)
  :after evil
  :demand t
  :config
  (evil-define-key* 'visual 'global
    "*" #'evil-visualstar/begin-search-forward
    "#" #'evil-visualstar/begin-search-backward))

;; Align text with gl/gL operator (e.g., glip= to align paragraph by =)
(use-package evil-lion
  :ensure (:wait t)
  :after evil
  :demand t)

;; Leader key support
(use-package general
  :ensure (:wait t)
  :demand t
  :config
  (general-create-definer sw/leader
    :states '(normal visual motion)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")

  (general-create-definer sw/local-leader
    :states '(normal visual motion)
    :prefix "SPC m"
    :global-prefix "C-SPC m"))

;; Which-key for discoverability (built-in since Emacs 30)
(use-package which-key
  :ensure nil
  :hook (sw/first-input . which-key-mode)
  :custom
  (which-key-idle-delay 0.3)
  (which-key-idle-secondary-delay 0.05)
  (which-key-add-column-padding 0))

(provide 'sw-evil)
;;; sw-evil.el ends here
