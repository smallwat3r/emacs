;;; sw-tailscale.el --- Tailscale integration -*- lexical-binding: t -*-

;;; Commentary:
;; Tailscale VPN helpers for account switching and device access.

;;; Code:

(require 'sw-lib)

(defun sw-tailscale--call (&rest args)
  "Run tailscale with ARGS. Return (EXIT-CODE . OUTPUT)."
  (with-temp-buffer
    (cons (apply #'call-process "tailscale" nil t nil args)
          (buffer-string))))

(defun sw-tailscale--run (&rest args)
  "Run tailscale with ARGS asynchronously."
  (sw-ensure-cli "tailscale")
  (sw-run-async (cons "tailscale" args)))

(defun sw-tailscale--parse-accounts (output)
  "Parse account list OUTPUT into alist of (DISPLAY . ID).
The active account is marked with a trailing `*' by the CLI and
gets a (current) suffix in DISPLAY."
  (let ((lines (cdr (split-string output "\n" t)))
        accounts)
    (dolist (line lines)
      (when (string-match
             "^\\([a-f0-9]+\\)\\s-+\\([^ ]+\\)\\s-+\\([^ *]+\\)\\(\\*\\)?"
             line)
        (let ((id (match-string 1 line))
              (tailnet (match-string 2 line))
              (account (match-string 3 line))
              (current (if (match-string 4 line) " (current)" "")))
          (push (cons (format "%s (%s)%s" account tailnet current) id)
                accounts))))
    (nreverse accounts)))

(defun sw-tailscale--peers ()
  "Return the peer alists from `tailscale status --json', or nil on error."
  (sw-ensure-cli "tailscale")
  (condition-case err
      (let ((data (json-parse-string (cdr (sw-tailscale--call "status" "--json"))
                                     :object-type 'alist)))
        (mapcar #'cdr (alist-get 'Peer data)))
    (error
     (message "Tailscale: %s" (error-message-string err))
     nil)))

(defun sw-tailscale--peer-name (info)
  "Return the short DNS name of peer INFO, or nil if it has none."
  (let ((dns (alist-get 'DNSName info)))
    ;; Some peers legitimately have no DNS name, skip them rather
    ;; than erroring out.
    (when (and (stringp dns) (> (length dns) 0))
      (car (split-string dns "\\.")))))

(defun sw-tailscale--devices ()
  "Return alist of Tailscale devices as (DISPLAY . NAME).
Online devices come first, offline ones are marked as such."
  (let (devices)
    (dolist (info (sw-tailscale--peers))
      (when-let* ((name (sw-tailscale--peer-name info)))
        (let ((online (eq (alist-get 'Online info) t)))
          (push (list (if online name (format "%s (offline)" name))
                      name online)
                devices))))
    (mapcar (lambda (d) (cons (car d) (cadr d)))
            (sort devices (lambda (a b) (and (caddr a) (not (caddr b))))))))

(defun sw-tailscale--exit-nodes ()
  "Return alist of exit nodes as (DISPLAY . NAME).
The active exit node gets a (current) suffix in DISPLAY."
  (let (nodes)
    (dolist (info (sw-tailscale--peers))
      (when-let* (((eq (alist-get 'ExitNodeOption info) t))
                  (name (sw-tailscale--peer-name info)))
        (push (cons (if (eq (alist-get 'ExitNode info) t)
                        (format "%s (current)" name)
                      name)
                    name)
              nodes)))
    (nreverse nodes)))

(defun sw-tailscale-exit-node ()
  "Select a Tailscale exit node, or None to route directly."
  (interactive)
  (let* ((nodes (or (sw-tailscale--exit-nodes)
                    (user-error "No Tailscale exit nodes found")))
         (choice (completing-read "Tailscale exit node: "
                                  (cons "None" (mapcar #'car nodes)) nil t))
         (name (alist-get choice nodes "" nil #'equal)))
    (sw-tailscale--run "set" (concat "--exit-node=" name))))

(defun sw-tailscale-switch ()
  "Switch Tailscale account, parsed from CLI."
  (interactive)
  (sw-ensure-cli "tailscale")
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
  (sw-ensure-cli "tailscale")
  (sw-command-buffer "*tailscale-status*" '("tailscale" "status")))

(defun sw-tailscale-ssh ()
  "Select a Tailscale device and connect via TRAMP."
  (interactive)
  (if-let* ((devices (sw-tailscale--devices)))
      (let* ((choice (completing-read "Tailscale device: "
                                      (mapcar #'car devices) nil t))
             (name (alist-get choice devices nil nil #'equal)))
        (find-file (format "/scp:%s:" name)))
    (user-error "No Tailscale devices found")))

(provide 'sw-tailscale)
;;; sw-tailscale.el ends here
