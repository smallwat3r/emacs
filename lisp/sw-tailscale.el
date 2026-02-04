;;; sw-tailscale.el --- Tailscale integration -*- lexical-binding: t -*-

;;; Commentary:
;; Tailscale VPN helpers for account switching and device access.

;;; Code:

(defvar sw-tailscale-accounts
  '(("work" . "d52c")
    ("home" . "a195"))
  "Alist mapping account aliases to Tailscale account IDs.")

(defmacro sw-tailscale--with-cli (&rest body)
  "Execute BODY if tailscale CLI is available, else signal error."
  (declare (indent 0))
  `(if (executable-find "tailscale")
       (progn ,@body)
     (user-error "tailscale not found")))

(defun sw-tailscale--devices ()
  "Return alist of Tailscale devices as (name . ip)."
  (sw-tailscale--with-cli
    (condition-case err
        (let* ((json (shell-command-to-string "tailscale status --json 2>/dev/null"))
               (data (json-parse-string json :object-type 'alist))
               (peers (alist-get 'Peer data)))
          (mapcar (lambda (peer)
                    (let* ((info (cdr peer))
                           (dns (alist-get 'DNSName info))
                           (name (car (split-string dns "\\.")))
                           (ip (aref (alist-get 'TailscaleIPs info) 0)))
                      (cons name ip)))
                  peers))
      (error
       (message "Tailscale: %s" (error-message-string err))
       nil))))

(defun sw-tailscale-switch (account)
  "Switch Tailscale to ACCOUNT (alias from `sw-tailscale-accounts')."
  (interactive
   (list (completing-read "Tailscale account: "
                          (mapcar #'car sw-tailscale-accounts)
                          nil t)))
  (sw-tailscale--with-cli
    (if-let ((id (alist-get account sw-tailscale-accounts nil nil #'equal)))
        (let ((cmd (format "sudo tailscale switch %s" id)))
          (message "Switching to %s..." account)
          (if (zerop (shell-command cmd))
              (message "Switched to %s" account)
            (message "Failed to switch to %s" account)))
      (user-error "Unknown account: %s" account))))

(defun sw-tailscale-status ()
  "Show Tailscale status."
  (interactive)
  (sw-tailscale--with-cli
    (with-current-buffer (get-buffer-create "*tailscale-status*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (shell-command-to-string "tailscale status")))
      (goto-char (point-min))
      (special-mode)
      (pop-to-buffer (current-buffer)))))

(defun sw-tailscale-ssh ()
  "Select a Tailscale device and connect via TRAMP."
  (interactive)
  (sw-tailscale--with-cli
    (if-let ((devices (sw-tailscale--devices)))
        (let* ((names (mapcar #'car devices))
               (choice (completing-read "Tailscale device: " names nil t))
               (path (format "/ssh:%s:" choice)))
          (find-file path))
      (user-error "No Tailscale devices found"))))

(provide 'sw-tailscale)
;;; sw-tailscale.el ends here
