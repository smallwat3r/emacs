;;; sw-org.el --- Org mode configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Org mode and related packages.

;;; Code:

(defvar sw-org-directory "~/org"
  "Directory for org files.")

(use-package org
  :ensure nil
  :custom
  ;; Directories
  (org-directory sw-org-directory)
  (org-default-notes-file (expand-file-name "notes.org" sw-org-directory))

  ;; Display
  (org-startup-indented t)
  (org-startup-folded 'content)
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-ellipsis " ▼")

  ;; Source blocks
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-src-preserve-indentation t)
  (org-edit-src-content-indentation 0)

  ;; Export
  (org-export-with-smart-quotes t)
  (org-export-preserve-breaks nil)

  ;; TODO settings
  (org-todo-keywords
   '((sequence "TODO(t)" "INPROGRESS(i)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))

  :config
  ;; Babel languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (shell . t))))

;; Modern Org appearance
(use-package org-modern
  :after org
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda)))

;; Org journal
(use-package org-journal
  :custom
  (org-journal-dir (expand-file-name "journal" sw-org-directory))
  (org-journal-date-format "%A, %d %B %Y")
  (org-journal-file-format "%Y-%m-%d.org")
  (org-journal-file-type 'daily))

;; Table of contents
(use-package toc-org
  :hook (org-mode . toc-org-mode))

;; Deft - quick note browsing
(use-package deft
  :commands deft
  :custom
  (deft-directory sw-org-directory)
  (deft-extensions '("org" "md" "txt"))
  (deft-default-extension "txt")
  (deft-recursive t)
  (deft-use-filename-as-title t)
  (deft-use-filter-string-for-filename t)
  (deft-auto-save-interval -1.0)
  (deft-file-naming-rules '((noslash . "-")
                            (nospace . "-")
                            (case-fn . downcase))))

(provide 'sw-org)
;;; sw-org.el ends here
