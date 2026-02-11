;;; sw-tailscale.el --- Tailscale integration -*- lexical-binding: t -*-

;;; Commentary:
;; Tailscale VPN helpers for account switching and device access.

;;; Code:

(defmacro sw-tailscale--with-cli (&rest body)
  "Execute BODY if tailscale CLI is available, else signal error."
  (declare (indent 0))
  `(if (executable-find "tailscale")
       (progn ,@body)
     (user-error "tailscale not found")))

(defun sw-tailscale--call (&rest args)
  "Run tailscale with ARGS. Return (EXIT-CODE . OUTPUT)."
  (with-temp-buffer
    (cons (apply #'call-process "tailscale" nil t nil args)
          (buffer-string))))

(defun sw-tailscale--parse-accounts (output)
  "Parse account list OUTPUT into alist of (DISPLAY . ID)."
  (let ((lines (cdr (split-string output "\n" t)))
        accounts)
    (dolist (line lines)
      (when (string-match
             "^\\([a-f0-9]+\\)\\s-+\\([^ ]+\\)\\s-+\\([^ *]+\\)"
             line)
        (let ((id (match-string 1 line))
              (tailnet (match-string 2 line))
              (account (match-string 3 line)))
          (push (cons (format "%s (%s)" account tailnet) id)
                accounts))))
    (nreverse accounts)))

(defun sw-tailscale--devices ()
  "Return alist of Tailscale devices as (name . ip)."
  (sw-tailscale--with-cli
    (condition-case err
        (let* ((result (sw-tailscale--call "status" "--json"))
               (data (json-parse-string (cdr result)
                                        :object-type 'alist))
               (peers (alist-get 'Peer data)))
          (mapcar (lambda (peer)
                    (let* ((info (cdr peer))
                           (dns (alist-get 'DNSName info))
                           (name (car (split-string dns "\\.")))
                           (ip (aref
                                (alist-get 'TailscaleIPs info)
                                0)))
                      (cons name ip)))
                  peers))
      (error
       (message "Tailscale: %s" (error-message-string err))
       nil))))

(defun sw-tailscale-switch ()
  "Switch Tailscale account, parsed from CLI."
  (interactive)
  (sw-tailscale--with-cli
    (let* ((result (sw-tailscale--call "switch" "--list"))
           (_ (unless (zerop (car result))
                (user-error "Failed to list accounts")))
           (accounts (sw-tailscale--parse-accounts
                      (cdr result)))
           (_ (unless accounts
                (user-error "No Tailscale accounts found")))
           (choice (completing-read
                    "Tailscale account: "
                    (mapcar #'car accounts) nil t))
           (id (alist-get choice accounts nil nil #'equal)))
      (message "Switching to %s..." choice)
      (let ((switch (sw-tailscale--call "switch" id)))
        (if (zerop (car switch))
            (message "Switched to %s" choice)
          (message "Failed to switch to %s" choice))))))

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
