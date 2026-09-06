;;; sw-tailscale.el --- Tailscale integration -*- lexical-binding: t -*-

;;; Commentary:
;; Tailscale VPN helpers for account switching and device access.

;;; Code:

(require 'sw-lib)

(defun sw-tailscale--ensure-cli ()
  "Signal a user error if the tailscale CLI is not available."
  (unless (executable-find "tailscale")
    (user-error "tailscale not found")))

(defun sw-tailscale--call (&rest args)
  "Run tailscale with ARGS. Return (EXIT-CODE . OUTPUT)."
  (with-temp-buffer
    (cons (apply #'call-process "tailscale" nil t nil args)
          (buffer-string))))

(defun sw-tailscale--run (&rest args)
  "Run tailscale with ARGS asynchronously."
  (sw-tailscale--ensure-cli)
  (apply #'sw-run-async "tailscale" args))

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
  (sw-tailscale--ensure-cli)
  (condition-case err
      (let* ((result (sw-tailscale--call "status" "--json"))
             (data (json-parse-string (cdr result)
                                      :object-type 'alist))
             (peers (alist-get 'Peer data)))
        (delq nil
              (mapcar (lambda (peer)
                        (let* ((info (cdr peer))
                               (dns (alist-get 'DNSName info))
                               (ips (alist-get 'TailscaleIPs info)))
                          ;; Some peers legitimately have no DNS name or
                          ;; no IPs, skip them rather than erroring out.
                          (when (and (stringp dns)
                                     (> (length dns) 0)
                                     (vectorp ips)
                                     (> (length ips) 0))
                            (cons (car (split-string dns "\\."))
                                  (aref ips 0)))))
                      peers)))
    (error
     (message "Tailscale: %s" (error-message-string err))
     nil)))

(defun sw-tailscale-switch ()
  "Switch Tailscale account, parsed from CLI."
  (interactive)
  (sw-tailscale--ensure-cli)
  (let* ((result (sw-tailscale--call "switch" "--list"))
         (_ (unless (zerop (car result))
              (user-error "Failed to list accounts")))
         (accounts (or (sw-tailscale--parse-accounts (cdr result))
                       (user-error "No Tailscale accounts found")))
         (choice (completing-read
                  "Tailscale account: "
                  (mapcar #'car accounts) nil t)))
    (sw-tailscale--run "switch" (alist-get choice accounts nil nil #'equal))))

(defun sw-tailscale-up ()
  "Bring Tailscale up."
  (interactive)
  (sw-tailscale--run "up"))

(defun sw-tailscale-down ()
  "Bring Tailscale down.
Note this flushes the tailnet routes the IVPN drop-in adds, run
iv-tailscale-routes from a shell after the next up when IVPN is
active."
  (interactive)
  (sw-tailscale--run "down"))

(defun sw-tailscale-status ()
  "Show Tailscale status."
  (interactive)
  (sw-tailscale--ensure-cli)
  (with-current-buffer (get-buffer-create "*tailscale-status*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (shell-command-to-string "tailscale status")))
    (goto-char (point-min))
    (special-mode)
    (pop-to-buffer (current-buffer))))

(defun sw-tailscale-ssh ()
  "Select a Tailscale device and connect via TRAMP."
  (interactive)
  (sw-tailscale--ensure-cli)
  (if-let* ((devices (sw-tailscale--devices)))
      (let* ((names (mapcar #'car devices))
             (choice (completing-read "Tailscale device: " names nil t))
             (path (format "/scp:%s:" choice)))
        (find-file path))
    (user-error "No Tailscale devices found")))

(provide 'sw-tailscale)
;;; sw-tailscale.el ends here
