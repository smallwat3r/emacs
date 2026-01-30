;;; sw-dired.el --- Dired configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Dired file manager enhancements.

;;; Code:

(use-package dired
  :ensure nil
  :custom
  (dired-listing-switches "-alh --group-directories-first")
  (dired-dwim-target t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-auto-revert-buffer t)
  :config
  ;; Enable extra features
  (put 'dired-find-alternate-file 'disabled nil))

(use-package dired-x
  :ensure nil
  :after dired
  :config
  (setq dired-omit-files
        (concat dired-omit-files "\\|^\\..+$")))

;; Icons in dired
(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

;; Subtree expansion
(use-package dired-subtree
  :after dired
  :custom
  (dired-subtree-use-backgrounds nil)
  :config
  (with-eval-after-load 'evil
    (evil-define-key 'normal dired-mode-map
      (kbd "TAB") #'dired-subtree-toggle
      (kbd "<backtab>") #'dired-subtree-remove)))

;; Narrow dired buffer
(use-package dired-narrow
  :after dired
  :config
  (with-eval-after-load 'evil
    (evil-define-key 'normal dired-mode-map
      "/" #'dired-narrow-fuzzy)))

(provide 'sw-dired)
;;; sw-dired.el ends here
