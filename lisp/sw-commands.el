;;; sw-commands.el --- Interactive commands -*- lexical-binding: t -*-

;;; Commentary:
;; General interactive commands used by keybindings.

;;; Code:

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

(defun sw/kill-all-projects-and-buffers ()
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

;;; Formatting

(defun sw/format-region-with-command (beg end cmd args msg)
  "Format region BEG to END with CMD and ARGS. MSG names the formatter."
  (let ((input (buffer-substring-no-properties beg end)))
    (if-let ((output (with-temp-buffer
                       (insert input)
                       (when (zerop (apply #'call-process-region
                                           (point-min) (point-max) cmd t t nil args))
                         (buffer-string)))))
        (save-excursion
          (delete-region beg end)
          (goto-char beg)
          (insert output)
          (message "Formatted region (%s)" msg))
      (message "%s formatting failed" msg))))

(defun sw/format-region-with-black (beg end)
  "Format region between BEG and END using black."
  (sw/format-region-with-command beg end "black" '("--quiet" "-") "black"))

(defun sw/format-region-with-gofmt (beg end)
  "Format region between BEG and END using gofmt."
  (sw/format-region-with-command beg end "gofmt" nil "gofmt"))

(defun sw/format-region-with-shfmt (beg end)
  "Format region between BEG and END using shfmt."
  (sw/format-region-with-command beg end "shfmt" '("-i" "2" "-ci" "-bn" "-") "shfmt"))

(defun sw/format-region ()
  "Format the current region using language-specific tools or eglot."
  (interactive)
  (unless (use-region-p)
    (user-error "No region selected"))
  (let ((beg (region-beginning))
        (end (region-end)))
    (cond
     ((derived-mode-p 'python-mode 'python-ts-mode)
      (sw/format-region-with-black beg end))
     ((derived-mode-p 'go-mode 'go-ts-mode)
      (sw/format-region-with-gofmt beg end))
     ((derived-mode-p 'sh-mode 'bash-ts-mode)
      (sw/format-region-with-shfmt beg end))
     ((and (fboundp 'eglot-managed-p) (eglot-managed-p))
      (eglot-format beg end)
      (message "Formatted region (eglot)"))
     (t
      (message "No region formatter available")))))

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

(provide 'sw-commands)
;;; sw-commands.el ends here
