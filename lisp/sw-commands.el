;;; sw-commands.el --- Interactive commands -*- lexical-binding: t -*-

;;; Commentary:
;; General interactive commands used by keybindings.

;;; Code:

(require 'sw-workspaces)

;;; Buffer/File commands

(defun sw/show-buffer-path ()
  "Show current buffer file path or buffer name."
  (interactive)
  (message "%s" (or (buffer-file-name) (buffer-name))))

(defun sw/copy-file-path ()
  "Copy current buffer file path to clipboard."
  (interactive)
  (when-let ((path (buffer-file-name)))
    (kill-new path)
    (message "Copied: %s" path)))

(defun sw/delete-this-file ()
  "Delete the current file and kill the buffer."
  (interactive)
  (let ((file (buffer-file-name)))
    (if (not file)
        (user-error "Buffer is not visiting a file")
      (when (y-or-n-p (format "Delete %s? " file))
        (delete-file file)
        (kill-buffer)
        (message "Deleted %s" file)))))

;;; Navigation commands

(defun sw/find-in-directory (dir name)
  "Find file in DIR, switching to or creating workspace NAME."
  (let ((existing (member name (sw/workspace--get-names))))
    (if existing
        (tab-bar-switch-to-tab name)
      (let ((tab-bar-new-tab-choice #'sw/fallback-buffer))
        (tab-bar-new-tab)
        (tab-bar-rename-tab name)
        (delete-other-windows)))
    (let ((default-directory dir))
      (project-find-file))))

(defun sw/find-dotfiles ()
  "Find file in dotfiles directory, with dedicated workspace."
  (interactive)
  (sw/find-in-directory "~/dotfiles/" "dotfiles"))

(defun sw/find-emacs-config ()
  "Find file in Emacs config directory, with dedicated workspace."
  (interactive)
  (sw/find-in-directory user-emacs-directory ".emacs.d"))

;;; Formatting

(defun sw/format-buffer-or-region ()
  "Format the current region if active, otherwise format the buffer.
Uses eglot for region formatting when available, apheleia for buffer."
  (interactive)
  (if (and (use-region-p)
           (fboundp 'eglot-managed-p)
           (eglot-managed-p))
      (eglot-format (region-beginning) (region-end))
    (require 'apheleia)
    (call-interactively #'apheleia-format-buffer)))

;;; Insert commands

(defun sw/insert-date ()
  "Insert current date."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defun sw/insert-datetime ()
  "Insert current datetime."
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%M:%S")))

(defun sw/insert-email ()
  "Insert email address, prompting if multiple available."
  (interactive)
  (insert (if (cdr sw/email-addresses)
              (completing-read "Email: " sw/email-addresses nil t)
            sw/email)))

;;; Project commands

(defun sw/project-make ()
  "Run make in project root."
  (interactive)
  (let ((default-directory (if (project-current)
                               (project-root (project-current))
                             default-directory)))
    (call-interactively #'compile)))

(provide 'sw-commands)
;;; sw-commands.el ends here
