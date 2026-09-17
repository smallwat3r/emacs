;;; sw-commands.el --- Interactive commands -*- lexical-binding: t -*-

;;; Commentary:
;; General interactive commands used by keybindings.

;;; Code:

(require 'sw-lib)

;;; Buffer/File commands

(defun sw-show-buffer-path ()
  "Show current buffer file path or buffer name."
  (interactive)
  (message "%s" (or (buffer-file-name) (buffer-name))))

(defun sw-copy-file-path ()
  "Copy current buffer file path to clipboard."
  (interactive)
  (when-let* ((path (buffer-file-name)))
    (kill-new path)
    (message "Copied: %s" path)))

(defun sw-kill-buffer ()
  "Kill the current buffer with confirmation message."
  (interactive)
  (let ((name (buffer-name)))
    (kill-current-buffer)
    (message "Killed buffer: %s" name)))

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
    (tab-bar-close-other-tabs)
    (tab-bar-rename-tab "main")
    ;; Kill all buffers except essential ones. Suppress query functions
    ;; so live-process buffers (REPLs, eglot) don't prompt mid-loop.
    (let ((kill-buffer-query-functions nil))
      (dolist (buf (buffer-list))
        (unless (member (buffer-name buf) '("*scratch*" "*Messages*"))
          (kill-buffer buf))))
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

(defun sw--skip-backward-word ()
  "Skip backward over a word if one precedes point."
  (when (looking-back "[[:alnum:]]" (1- (point)))
    (skip-chars-backward "[:alnum:]")))

(defun sw--skip-backward-symbol-and-word ()
  "Skip backward over a symbol sequence then a word."
  (when (looking-back "[^[:alnum:][:space:]]" (1- (point)))
    (skip-chars-backward "^[:alnum:][:space:]"))
  (sw--skip-backward-word))

(defun sw--backward-kill-word-boundary (start)
  "Return position of the backward kill boundary from START."
  (save-excursion
    (cond
     ((bobp) (point))
     ((looking-back "[ \t\n]" (1- (point)))
      (skip-chars-backward " \t\n")
      (when (= (- start (point)) 1)
        (sw--skip-backward-symbol-and-word))
      (point))
     ((looking-back "[_-]+" (line-beginning-position))
      (skip-chars-backward "_-")
      (sw--skip-backward-word)
      (point))
     ((looking-back "[^[:alnum:][:space:]_-]" (1- (point)))
      (skip-chars-backward "^[:alnum:][:space:]_-")
      (sw--skip-backward-word)
      (point))
     (t
      (skip-chars-backward "[:alnum:]")
      (point)))))

(defun sw-backward-kill-word ()
  "Kill backward more gradually than `backward-kill-word'.
Stops at word boundaries including underscores and hyphens.
When deleting single space, also deletes trailing symbol and word."
  (interactive)
  (let ((start (point)))
    (kill-region
     (sw--backward-kill-word-boundary start) start)))

(defun sw-copy-dedented (beg end)
  "Copy region between BEG and END with common indentation removed."
  (interactive "r")
  (let ((text (buffer-substring-no-properties beg end)))
    (kill-new (sw-string-reindent text (sw-string-min-indent text) 0))
    (deactivate-mark)
    (message "Copied dedented text")))

;;; Search commands

(defun sw-consult-line-symbol ()
  "Search for symbol at point in current buffer."
  (interactive)
  (require 'consult)
  (consult-line (thing-at-point 'symbol t)))

;;; Find file commands

(defun sw-find-file-in (dir)
  "Pick a file under DIR, all files listed up front, matched with orderless."
  (require 'consult)
  (let* ((default-directory (file-name-as-directory (expand-file-name dir)))
         ;; synchronous fd, fine for project sized trees, use consult-fd for huge ones
         (files (apply #'process-lines (append consult-fd-args '("--type" "f")))))
    (find-file (completing-read "File: " files nil t))))

(defun sw-find-file-here ()
  "Pick a file under the current directory."
  (interactive)
  (sw-find-file-in default-directory))

;;; Notes commands
;; Plain files in ~/notes, txt by default, found with fd and rg
(defvar sw-notes-directory "~/notes"
  "Directory holding notes.")

(defun sw-notes-find ()
  "Pick a note by file name."
  (interactive)
  (sw-find-file-in sw-notes-directory))

(defun sw-notes-search ()
  "Search note contents."
  (interactive)
  (require 'consult)
  (consult-ripgrep sw-notes-directory))

(defun sw-notes-new (title)
  "Create a new txt note named after TITLE."
  (interactive "sNote title: ")
  (let ((slug (string-trim (replace-regexp-in-string "[^a-z0-9]+" "-" (downcase title)) "-" "-")))
    (when (string-empty-p slug)
      (user-error "Empty note title"))
    (find-file (expand-file-name (concat slug ".txt") sw-notes-directory))))

;;; Window commands

(defun sw-split-window-right ()
  "Split window right and select the new window."
  (interactive)
  (select-window (split-window-right)))

(defun sw-split-window-below ()
  "Split window below and select the new window."
  (interactive)
  (select-window (split-window-below)))

;;; OS commands

(defun sw-open-in-file-manager ()
  "Open the current directory in the OS file manager."
  (interactive)
  (let ((dir (or (and (derived-mode-p 'dired-mode)
                      (dired-current-directory))
                 default-directory)))
    (pcase system-type
      ('darwin (call-process "open" nil 0 nil dir))
      ('gnu/linux (call-process "xdg-open" nil 0 nil dir))
      (_ (user-error "Unsupported system: %s"
                     system-type)))))

(provide 'sw-commands)
;;; sw-commands.el ends here
