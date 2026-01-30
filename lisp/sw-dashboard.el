;;; sw-dashboard.el --- Simple startup dashboard -*- lexical-binding: t -*-

;; Copyright (C) 2026 Matthieu Petiteau
;; Author: Matthieu Petiteau <mpetiteau.pro@gmail.com>

;;; Commentary:
;; A minimal dashboard displayed on Emacs startup.

;;; Code:

(defconst sw/dashboard-buffer-name "*dashboard*"
  "Name of the dashboard buffer.")

(defun sw/dashboard-protect-buffer ()
  "Prevent the dashboard buffer from being killed."
  (not (string= (buffer-name) sw/dashboard-buffer-name)))

(defun sw/dashboard--os-version ()
  "Return the OS version string."
  (pcase system-type
    ('gnu/linux
     (or (when-let ((release (ignore-errors
                               (with-temp-buffer
                                 (insert-file-contents "/etc/os-release")
                                 (buffer-string)))))
           (when (string-match "^PRETTY_NAME=\"?\\([^\"]+\\)\"?" release)
             (match-string 1 release)))
         (string-trim (shell-command-to-string "uname -sr"))))
    ('darwin
     (format "macOS %s"
             (string-trim (shell-command-to-string "sw_vers -productVersion"))))
    (_ (capitalize (symbol-name system-type)))))

(defun sw/dashboard-display ()
  "Create and display the dashboard buffer."
  (let ((buf (get-buffer-create sw/dashboard-buffer-name)))
    (with-current-buffer buf
      (with-silent-modifications
        (erase-buffer)
        (let* ((width (window-width))
               (height (window-height))
               (content-lines 7)
               (top-padding (max 0 (/ (- height content-lines) 2)))
               (center (lambda (s)
                         (insert (make-string (max 0 (/ (- width (length s)) 2)) ?\s)
                                 s "\n")))
               (load-time (float-time
                           (time-subtract after-init-time before-init-time))))
          (insert (make-string top-padding ?\n))
          (funcall center "smallwat3r's Emacs")
          (insert "\n\n")
          (funcall center (format "Emacs %s" emacs-version))
          (funcall center (sw/dashboard--os-version))
          (funcall center (if (daemonp) "Mode: Daemon" "Mode: Standalone"))
          (insert "\n")
          (funcall center (format "Loaded in %.2fs, %d GCs" load-time gcs-done)))
        (goto-char (point-min)))
      (setq-local buffer-read-only t
                  cursor-type nil
                  mode-line-format nil
                  truncate-lines t))
    (switch-to-buffer buf)))

(defun sw/dashboard-refresh ()
  "Refresh the dashboard buffer."
  (interactive)
  (sw/dashboard-display))

(defun sw/dashboard-setup ()
  "Set up the dashboard to display on startup."
  (add-hook 'kill-buffer-query-functions #'sw/dashboard-protect-buffer)
  (add-hook 'emacs-startup-hook #'sw/dashboard-display 100))

(provide 'sw-dashboard)

;;; sw-dashboard.el ends here
