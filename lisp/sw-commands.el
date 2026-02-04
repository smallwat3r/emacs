;;; sw-commands.el --- Interactive commands -*- lexical-binding: t -*-

;;; Commentary:
;; General interactive commands used by keybindings.

;;; Code:

;;; Buffer/File commands

(defun sw-show-buffer-path ()
  "Show current buffer file path or buffer name."
  (interactive)
  (message "%s" (or (buffer-file-name) (buffer-name))))

(defun sw-copy-file-path ()
  "Copy current buffer file path to clipboard."
  (interactive)
  (when-let ((path (buffer-file-name)))
    (kill-new path)
    (message "Copied: %s" path)))

(defun sw-delete-this-file ()
  "Delete the current file and kill the buffer."
  (interactive)
  (let ((file (buffer-file-name)))
    (if (not file)
        (user-error "Buffer is not visiting a file")
      (when (y-or-n-p (format "Delete %s? " file))
        (delete-file file)
        (kill-buffer)
        (message "Deleted %s" file)))))

(defun sw-kill-all-projects-and-buffers ()
  "Kill all buffers and close all workspaces except current.
Preserves *scratch* and *Messages* buffers."
  (interactive)
  (when (y-or-n-p "Kill all projects and buffers? ")
    ;; Close all tabs except the current one
    (while (> (length (tab-bar-tabs)) 1)
      (tab-bar-close-other-tabs))
    (tab-bar-rename-tab "main")
    ;; Kill all buffers except essential ones
    (dolist (buf (buffer-list))
      (unless (member (buffer-name buf) '("*scratch*" "*Messages*"))
        (kill-buffer buf)))
    (switch-to-buffer "*scratch*")
    (delete-other-windows)
    (message "All projects and buffers killed")))

;;; Insert commands

(defun sw-insert-date ()
  "Insert current date."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defun sw-insert-datetime ()
  "Insert current datetime."
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%M:%S")))

(defun sw-insert-email ()
  "Insert email address, prompting if multiple available."
  (interactive)
  (insert (if (cdr sw-email-addresses)
              (completing-read "Email: " sw-email-addresses nil t)
            sw-email)))

;;; Editing commands

(defun sw-backward-kill-word ()
  "Kill backward more gradually than `backward-kill-word'.
Stops at word boundaries including underscores and hyphens."
  (interactive)
  (let* ((start (point))
        (end (save-excursion
               (cond
                ((bobp) (point))
                ;; Whitespace: delete it, and following word if single space
                ((looking-back "[ \t\n]" (1- (point)))
                 (skip-chars-backward " \t\n")
                 (when (and (= (- start (point)) 1)
                            (looking-back "[[:alnum:]]" (1- (point))))
                   (skip-chars-backward "[:alnum:]"))
                 (point))
                ;; Underscores/hyphens: delete them
                ((looking-back "[_-]+" (line-beginning-position))
                 (skip-chars-backward "_-")
                 (point))
                ;; Punctuation: delete sequence
                ((looking-back "[^[:alnum:][:space:]_-]" (1- (point)))
                 (skip-chars-backward "^[:alnum:][:space:]_-")
                 (point))
                ;; Word: stop at separators
                (t
                 (skip-chars-backward "[:alnum:]")
                 (point))))))
    (kill-region end start)))

;;; Window commands

(defun sw-split-window-right ()
  "Split window right and select the new window."
  (interactive)
  (select-window (split-window-right)))

(defun sw-split-window-below ()
  "Split window below and select the new window."
  (interactive)
  (select-window (split-window-below)))

;;; Text scaling

(defvar sw--current-font-size nil
  "Current font size in points. Initialized from sw-font-size.")

(defun sw-text-scale-increase ()
  "Increase font size globally by 1pt."
  (interactive)
  (unless sw--current-font-size
    (setq sw--current-font-size sw-font-size))
  (setq sw--current-font-size (1+ sw--current-font-size))
  (set-face-attribute 'default nil :height (* sw--current-font-size 10))
  (message "Font size: %dpt" sw--current-font-size))

(defun sw-text-scale-decrease ()
  "Decrease font size globally by 1pt."
  (interactive)
  (unless sw--current-font-size
    (setq sw--current-font-size sw-font-size))
  (setq sw--current-font-size (max 8 (1- sw--current-font-size)))
  (set-face-attribute 'default nil :height (* sw--current-font-size 10))
  (message "Font size: %dpt" sw--current-font-size))

(defun sw-text-scale-reset ()
  "Reset font size to default."
  (interactive)
  (setq sw--current-font-size sw-font-size)
  (set-face-attribute 'default nil :height (* sw-font-size 10))
  (message "Font size: %dpt" sw-font-size))

(provide 'sw-commands)
;;; sw-commands.el ends here
