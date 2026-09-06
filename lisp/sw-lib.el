;;; sw-lib.el --- Shared utilities -*- lexical-binding: t -*-

;;; Commentary:
;; Small helper functions shared across modules.

;;; Code:

(defun sw-string-min-indent (str)
  "Return minimum indentation (spaces) of non-blank lines in STR."
  (let ((min most-positive-fixnum))
    (dolist (line (split-string str "\n"))
      (when (string-match "^\\( *\\)[^ \t\n]" line)
        (setq min (min min (length (match-string 1 line))))))
    (if (= min most-positive-fixnum) 0 min)))

(defun sw-string-reindent (str old-indent new-indent)
  "Change indentation of STR from OLD-INDENT to NEW-INDENT spaces."
  (let ((prefix (make-string new-indent ?\s))
        (re (concat "^" (make-string old-indent ?\s))))
    (mapconcat (lambda (line)
                 (if (string-match-p "^[ \t]*$" line)
                     line
                   (concat prefix
                           (replace-regexp-in-string
                            re "" line t t))))
               (split-string str "\n" nil)
               "\n")))

(defun sw-ensure-cli (program)
  "Signal a user error unless PROGRAM is found in PATH."
  (unless (executable-find program)
    (user-error "%s not found in PATH" program)))

(defun sw-run-async (command &optional on-success)
  "Run COMMAND (a list of program and arguments) asynchronously.
Report the outcome in the echo area. Call ON-SUCCESS with no
arguments once the process exits successfully."
  (let ((cmd (string-join command " ")))
    (message "%s..." cmd)
    (make-process
     :name (car command)
     :buffer (generate-new-buffer (format " *%s*" (car command)))
     :command command
     :sentinel
     (lambda (proc _event)
       (unless (process-live-p proc)
         (if (zerop (process-exit-status proc))
             (progn
               (message "%s: done" cmd)
               (when on-success (funcall on-success)))
           (message "%s failed: %s" cmd
                    (with-current-buffer (process-buffer proc)
                      (string-trim (buffer-string)))))
         (kill-buffer (process-buffer proc)))))))

(defun sw-command-buffer (name command)
  "Run COMMAND (a list of program and arguments) and show its output.
The output streams into a read-only buffer called NAME, which is
displayed right away so slow commands do not block Emacs."
  (let ((buffer (get-buffer-create name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer))
      (special-mode))
    (pop-to-buffer buffer)
    (make-process
     :name (car command)
     :buffer buffer
     :command command
     :sentinel
     (lambda (proc _event)
       (unless (process-live-p proc)
         (when-let* ((win (get-buffer-window (process-buffer proc))))
           (set-window-point win 1)))))))

(defun sw-toggle-window (buffer start)
  "Delete the window showing BUFFER if there is one, otherwise call START.
BUFFER may be nil when it does not exist yet."
  (if-let* ((win (and buffer (get-buffer-window buffer))))
      (delete-window win)
    (funcall start)))

(provide 'sw-lib)
;;; sw-lib.el ends here
