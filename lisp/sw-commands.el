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
  (let* ((text (buffer-substring-no-properties beg end))
         (min-indent (sw--string-min-indent text))
         (lines (split-string text "\n")))
    (kill-new
     (mapconcat
      (lambda (l)
        (if (>= (length l) min-indent)
            (substring l min-indent)
          l))
      lines "\n"))
    (deactivate-mark)
    (message "Copied dedented text")))

;;; Search commands

(defun sw-consult-line-symbol ()
  "Search for symbol at point in current buffer."
  (interactive)
  (require 'consult)
  (consult-line (thing-at-point 'symbol t)))

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

(defvar sw-font-size-step 1
  "Number of points to adjust when scaling font size.")

(defvar sw-font-size-min 6
  "Minimum font size in points.")

(defvar sw-font-size-max 26
  "Maximum font size in points.")

(defvar sw--current-font-size nil
  "Current font size in points. Initialized from sw-font-size.")

(defun sw--ensure-font-size ()
  "Initialize `sw--current-font-size' from `sw-font-size' if unset."
  (unless sw--current-font-size
    (setq sw--current-font-size sw-font-size)))

(defun sw--set-all-font-sizes (size)
  "Set SIZE (in points) on all text faces."
  (let ((height (* size 10)))
    (dolist (face '(default fixed-pitch fixed-pitch-serif
                            variable-pitch))
      (set-face-attribute face nil :height height))))

(defun sw-text-scale-increase ()
  "Increase font size globally by `sw-font-size-step'."
  (interactive)
  (sw--ensure-font-size)
  (setq sw--current-font-size
        (min sw-font-size-max
             (+ sw--current-font-size sw-font-size-step)))
  (sw--set-all-font-sizes sw--current-font-size)
  (message "Font size: %dpt" sw--current-font-size))

(defun sw-text-scale-decrease ()
  "Decrease font size globally by `sw-font-size-step'."
  (interactive)
  (sw--ensure-font-size)
  (setq sw--current-font-size
        (max sw-font-size-min
             (- sw--current-font-size sw-font-size-step)))
  (sw--set-all-font-sizes sw--current-font-size)
  (message "Font size: %dpt" sw--current-font-size))

(defun sw-text-scale-reset ()
  "Reset font size to default."
  (interactive)
  (setq sw--current-font-size sw-font-size)
  (sw--set-all-font-sizes sw-font-size)
  (message "Font size: %dpt" sw-font-size))

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
