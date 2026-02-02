;;; sw-eglot.el --- Eglot configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Eglot LSP client configuration (built-in since Emacs 29).

;;; Code:

(use-package eglot
  :ensure nil
  :demand t
  :hook (python-ts-mode . eglot-ensure)
  :hook (go-ts-mode . eglot-ensure)
  :hook (rust-ts-mode . eglot-ensure)
  :hook (typescript-ts-mode . eglot-ensure)
  :hook (js-ts-mode . eglot-ensure)
  :hook (terraform-mode . eglot-ensure)
  :init
  ;; Prevent eglot from managing flymake
  (setq eglot-stay-out-of '(flymake))
  :custom
  ;; Performance
  (eglot-autoshutdown t)
  (eglot-events-buffer-size 0)
  (eglot-sync-connect nil)
  (eglot-connect-timeout 10)

  ;; Features
  (eglot-extend-to-xref t)
  (eglot-ignored-server-capabilities '(:inlayHintProvider))

  :config
  ;; Server configurations
  (dolist (server '(((python-mode python-ts-mode)
                     "basedpyright-langserver" "--stdio")
                    ((rust-mode rust-ts-mode)
                     "rust-analyzer")
                    ((go-mode go-ts-mode)
                     "gopls")
                    ((typescript-ts-mode js-ts-mode tsx-ts-mode)
                     "typescript-language-server" "--stdio")
                    (terraform-mode
                     "terraform-ls" "serve")))
    (add-to-list 'eglot-server-programs
                 (cons (car server) (cdr server)))))

;; Consult integration for eglot
(use-package consult-eglot
  :after (consult eglot)
  :commands consult-eglot-symbols)

;; Eglot booster - IO buffering for better performance
(use-package eglot-booster
  :ensure (:host github :repo "jdtsmith/eglot-booster")
  :after eglot
  :when (executable-find "emacs-lsp-booster")
  :init
  ;; On Emacs 30+, only use IO buffering (native JSON is faster)
  (setq eglot-booster-io-only (> emacs-major-version 29))
  :config
  (eglot-booster-mode 1))

(provide 'sw-eglot)
;;; sw-eglot.el ends here
