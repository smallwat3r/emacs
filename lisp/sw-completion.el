;;; sw-completion.el --- Completion framework -*- lexical-binding: t -*-

;;; Commentary:
;; Vertico, Corfu, Consult, and related completion packages.

;;; Code:

;;; Minibuffer completion with Vertico

(use-package vertico
  :hook (sw-first-input . vertico-mode)
  :custom
  (vertico-count 10)
  (vertico-resize nil)
  (vertico-cycle t))

;; Save minibuffer history
(use-package savehist
  :ensure nil
  :hook (sw-first-input . savehist-mode))

;; Marginalia - rich annotations in minibuffer
(use-package marginalia
  :hook (sw-first-input . marginalia-mode)
  :config
  ;; Remove underline from annotation faces
  (dolist (face '(marginalia-documentation marginalia-value marginalia-key))
    (set-face-attribute face nil :underline nil)))

;; Orderless - flexible completion style with fuzzy matching
(use-package orderless
  :ensure (:wait t)
  :demand t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion))))
  :config
  ;; Define orderless matching styles
  (setq orderless-matching-styles '(orderless-literal
                                    orderless-regexp
                                    orderless-flex))  ; flex = fuzzy
  ;; Use fuzzy for consult-ripgrep and similar async commands
  (setq orderless-component-separator #'orderless-escapable-split-on-space))

;; Consult - enhanced search and navigation
(use-package consult
  :custom
  (consult-narrow-key "<")
  (consult-line-numbers-widen t)
  (consult-async-min-input 2)
  (consult-async-refresh-delay 0.15)
  (consult-async-input-throttle 0.2)
  (consult-async-input-debounce 0.1)
  :config
  ;; Use ripgrep for grep
  (setq consult-ripgrep-args
        (concat "rg --null --line-buffered --color=never --max-columns=1000 "
                "--path-separator / --smart-case --no-heading --line-number "
                "--hidden --glob !.git ."))
  ;; Optimized fd args (fdfind on Debian/Ubuntu, fd elsewhere)
  (setq consult-fd-args
        `(,(if (executable-find "fdfind") "fdfind" "fd")
          "--color=never" "--hidden" "--exclude" ".git"))
  (setq consult-async-split-style 'semicolon))

;; Embark - contextual actions
(use-package embark
  :custom
  (prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Embark + Consult integration
(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;;; In-buffer completion with Corfu

(use-package corfu
  :hook (sw-first-input . global-corfu-mode)
  :custom
  (corfu-count 5)
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 2)
  (corfu-cycle t)
  (corfu-preselect 'prompt)
  (corfu-quit-no-match 'separator)
  (corfu-quit-at-boundary 'separator)
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous)))

;; Cape - completion at point extensions
(use-package cape
  :after corfu
  :config
  ;; Add in reverse order since add-to-list pushes to front
  ;; Final order: dabbrev, file, keyword
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

;; Yasnippet
(use-package yasnippet
  :hook ((prog-mode text-mode) . yas-minor-mode)
  :commands (yas-insert-snippet yas-expand)
  :config
  (yas-reload-all))

(use-package yasnippet-snippets
  :after yasnippet)

(provide 'sw-completion)
;;; sw-completion.el ends here
