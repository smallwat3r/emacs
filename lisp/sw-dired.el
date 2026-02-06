;;; sw-dired.el --- Dired configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Dired file manager enhancements.

;;; Code:

(use-package dired
  :ensure nil
  :custom
  (dired-listing-switches (if sw-is-mac "-alh"
                           "-alh --group-directories-first"))
  (dired-dwim-target t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-auto-revert-buffer t)
  :config
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

;; Colorful file names by type
(use-package diredfl
  :hook (dired-mode . diredfl-mode))

;; Subtree expansion
(use-package dired-subtree
  :after dired
  :demand t
  :custom
  (dired-subtree-use-backgrounds nil))

;; Narrow dired buffer
(use-package dired-narrow
  :after dired)

(provide 'sw-dired)
;;; sw-dired.el ends here
