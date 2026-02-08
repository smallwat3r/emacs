;;; sw-evil.el --- Evil mode configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Vim emulation with Evil mode and related packages.

;;; Code:

(use-package evil
  :ensure (:wait t)
  :demand t
  :init
  ;; These must be set BEFORE evil loads
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-C-u-scroll t
        evil-want-C-i-jump t
        evil-want-Y-yank-to-eol t)
  :custom
  (evil-want-fine-undo t)
  (evil-undo-system 'undo-redo)
  (evil-symbol-word-search t)
  (evil-split-window-below t)
  (evil-vsplit-window-right t)
  (evil-respect-visual-line-mode t)
  (evil-search-module 'evil-search)
  (evil-kbd-macro-suppress-motion-error t)
  (evil-ex-interactive-search-highlight 'selected-window)
  (evil-ex-search-highlight-all t)
  :config
  (evil-mode 1)

  ;; Don't copy visual selection to clipboard on every movement (perf on Wayland)
  (setq evil-visual-update-x-selection-p nil)

  ;; Slow down search highlighting in large/folded buffers to prevent lag
  (defun sw-slow-down-evil-highlighting ()
    (setq-local evil-ex-hl-update-delay 0.25))
  (dolist (hook '(magit-mode-hook so-long-minor-mode-hook))
    (add-hook hook #'sw-slow-down-evil-highlighting)))

;; Evil keybindings for many modes
(use-package evil-collection
  :ensure (:wait t)
  :after evil
  :demand t
  :custom
  (evil-collection-magit-want-horizontal-movement t)
  ;; Prevent evil-collection from binding SPC in modes it manages
  ;; (e.g. magit, dired), so our SPC leader always takes priority.
  (evil-collection-key-blacklist '("SPC"))
  :config
  (evil-collection-init))

;; Visual hints for operations
(use-package evil-goggles
  :after evil
  :hook (sw-first-input . evil-goggles-mode)
  :custom
  (evil-goggles-duration 0.15)
  (evil-goggles-pulse nil)
  :config
  (evil-goggles-use-diff-refine-faces))

;; Two-character search
(use-package evil-snipe
  :after evil
  :hook ((sw-first-input . evil-snipe-mode)
         (sw-first-input . evil-snipe-override-mode))
  :custom
  (evil-snipe-scope 'visible)
  (evil-snipe-repeat-scope 'visible)
  (evil-snipe-spillover-scope nil)
  :config
  (defvar sw--last-search-was-snipe nil
    "Non-nil if the last search action was an evil-snipe.")

  (defun sw--snipe-set-flag (&rest _)
    "Record that the last search was a snipe."
    (setq sw--last-search-was-snipe t))

  (defun sw--search-clear-flag (&rest _)
    "Record that the last search was an evil-search."
    (setq sw--last-search-was-snipe nil))

  (advice-add 'evil-snipe-s :after #'sw--snipe-set-flag)
  (advice-add 'evil-snipe-S :after #'sw--snipe-set-flag)
  (advice-add 'evil-ex-search-forward
              :after #'sw--search-clear-flag)
  (advice-add 'evil-ex-search-backward
              :after #'sw--search-clear-flag)

  (defun sw-search-next ()
    "Repeat last search: snipe or evil-search."
    (interactive)
    (if sw--last-search-was-snipe
        (evil-snipe-repeat)
      (evil-ex-search-next)))

  (defun sw-search-previous ()
    "Repeat last search in reverse: snipe or evil-search."
    (interactive)
    (if sw--last-search-was-snipe
        (evil-snipe-repeat-reverse)
      (evil-ex-search-previous))))

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
  (global-evil-surround-mode 1)
  (push '(?< . ("<" . ">")) evil-surround-pairs-alist)
  (push '(?> . ("<" . ">")) evil-surround-pairs-alist))

;; Highlight symbol under cursor
(defun sw-highlight-symbol-at-point ()
  "Highlight symbol at point without moving. Use n/N to jump."
  (interactive)
  (let* ((symbol (or (thing-at-point 'symbol t)
                     (user-error "No symbol at point")))
         (pattern (format "\\_<%s\\_>" (regexp-quote symbol))))
    (setq evil-ex-search-pattern (evil-ex-make-search-pattern pattern)
          evil-ex-search-direction 'forward)
    (evil-push-search-history pattern t)
    (evil-ex-delete-hl 'evil-ex-search)
    (evil-ex-make-hl 'evil-ex-search)
    (evil-ex-hl-change 'evil-ex-search evil-ex-search-pattern)
    (evil-ex-hl-update-highlights)))

;; Tree-sitter text objects (e.g., vaf, dif, vic, dia)
(use-package evil-textobj-tree-sitter
  :after evil
  :demand t
  :config
  (define-key evil-outer-text-objects-map "f"
    (evil-textobj-tree-sitter-get-textobj
      "function.outer"))
  (define-key evil-inner-text-objects-map "f"
    (evil-textobj-tree-sitter-get-textobj
      "function.inner"))
  (define-key evil-outer-text-objects-map "c"
    (evil-textobj-tree-sitter-get-textobj
      "class.outer"))
  (define-key evil-inner-text-objects-map "c"
    (evil-textobj-tree-sitter-get-textobj
      "class.inner"))
  (define-key evil-outer-text-objects-map "a"
    (evil-textobj-tree-sitter-get-textobj
      "parameter.outer"))
  (define-key evil-inner-text-objects-map "a"
    (evil-textobj-tree-sitter-get-textobj
      "parameter.inner"))
  (define-key evil-outer-text-objects-map "i"
    (evil-textobj-tree-sitter-get-textobj
      "conditional.outer"))
  (define-key evil-inner-text-objects-map "i"
    (evil-textobj-tree-sitter-get-textobj
      "conditional.inner"))
  (define-key evil-outer-text-objects-map "l"
    (evil-textobj-tree-sitter-get-textobj
      "loop.outer"))
  (define-key evil-inner-text-objects-map "l"
    (evil-textobj-tree-sitter-get-textobj
      "loop.inner")))

;; Align text with gl/gL operator (e.g., glip= to align paragraph by =)
(use-package evil-lion
  :ensure (:wait t)
  :after evil
  :demand t)

(provide 'sw-evil)
;;; sw-evil.el ends here
