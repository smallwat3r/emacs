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

;; Region formatters by mode. Each entry is (MODE . (CMD . ARGS)).
;; Args should match apheleia config in sw-programming.el where applicable.
(defvar sw/region-formatters
  '((python-mode    . ("black" "--quiet" "-"))
    (python-ts-mode . ("black" "--quiet" "-"))
    (go-mode        . ("gofmt"))
    (go-ts-mode     . ("gofmt"))
    (sh-mode        . ("shfmt" "-i" "2" "-ci" "-bn" "-"))
    (bash-ts-mode   . ("shfmt" "-i" "2" "-ci" "-bn" "-")))
  "Alist mapping major modes to region formatter commands.
Each value is a list where car is the command and cdr is the arguments.")

(defun sw/format-region ()
  "Format the current region using language-specific tools or eglot."
  (interactive)
  (unless (use-region-p)
    (user-error "No region selected"))
  (let* ((beg (region-beginning))
         (end (region-end))
         (formatter (alist-get major-mode sw/region-formatters)))
    (cond
     (formatter
      (let* ((cmd (car formatter))
             (args (cdr formatter))
             (input (buffer-substring-no-properties beg end))
             (output (with-temp-buffer
                       (insert input)
                       (when (zerop (apply #'call-process-region
                                           (point-min) (point-max) cmd t t nil args))
                         (buffer-string)))))
        (if output
            (save-excursion
              (delete-region beg end)
              (goto-char beg)
              (insert output)
              (message "Formatted region (%s)" cmd))
          (message "%s formatting failed" cmd))))
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
