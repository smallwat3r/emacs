;;; sw-ivpn.el --- IVPN integration -*- lexical-binding: t -*-

;;; Commentary:
;; IVPN helpers for connecting, disconnecting and checking status.

;;; Code:

(require 'sw-lib)

(defun sw-ivpn--ensure-cli ()
  "Signal a user error if the ivpn CLI is not available."
  (unless (executable-find "ivpn")
    (user-error "ivpn not found")))

(defun sw-ivpn--run (&rest args)
  "Run ivpn with ARGS asynchronously."
  (sw-ivpn--ensure-cli)
  (apply #'sw-run-async "ivpn" args))

(defun sw-ivpn--servers ()
  "Return alist of WireGuard servers as (DISPLAY . HOST).
Parses the pipe separated table from `ivpn servers -p wg', the
first column after the protocol is the host, the following ones
city, country and ISP."
  (sw-ivpn--ensure-cli)
  (let (servers)
    (dolist (line (cdr (split-string
                        (shell-command-to-string "ivpn servers -p wg")
                        "\n" t)))
      (let ((cols (mapcar #'string-trim (split-string line "|"))))
        (when (> (length cols) 4)
          (push (cons (string-join (seq-subseq cols 1 5) "  ")
                      (nth 1 cols))
                servers))))
    (nreverse servers)))

(defun sw-ivpn-connect ()
  "Pick a WireGuard server and connect to it."
  (interactive)
  (let* ((servers (or (sw-ivpn--servers)
                      (user-error "No IVPN servers found")))
         (choice (completing-read "IVPN server: " (mapcar #'car servers)
                                  nil t)))
    (sw-ivpn--run "connect" "-p" "wg"
                  (alist-get choice servers nil nil #'equal))))

(defun sw-ivpn-connect-last ()
  "Reconnect to the last used server."
  (interactive)
  (sw-ivpn--run "connect" "-last"))

(defun sw-ivpn-disconnect ()
  "Disconnect from IVPN."
  (interactive)
  (sw-ivpn--run "disconnect"))

(defun sw-ivpn-status ()
  "Show IVPN status."
  (interactive)
  (sw-ivpn--ensure-cli)
  (with-current-buffer (get-buffer-create "*ivpn-status*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (shell-command-to-string "ivpn status")))
    (goto-char (point-min))
    (special-mode)
    (pop-to-buffer (current-buffer))))

(provide 'sw-ivpn)
;;; sw-ivpn.el ends here
