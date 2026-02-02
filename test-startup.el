#!/usr/bin/env -S emacs --script
;;; test-startup.el --- Test Emacs startup time -*- lexical-binding: t -*-

;;; Commentary:
;; Run with: emacs --script test-startup.el [threshold]
;; Or just: ./test-startup.el [threshold]

;;; Code:

(defvar sw-test-threshold (if (car command-line-args-left)
                              (string-to-number (car command-line-args-left))
                            1.0)
  "Maximum allowed startup time in seconds.")

(defvar sw-test-times nil
  "Alist of module names to load times.")

(defvar sw-test-start (current-time)
  "Start time for total measurement.")

(defmacro sw-time (name &rest body)
  "Measure time to execute BODY and record under NAME."
  `(let ((start (current-time)))
     ,@body
     (push (cons ,name (float-time (time-subtract (current-time) start)))
           sw-test-times)))

;; Load config
(sw-time "early-init" (load (expand-file-name "early-init.el" user-emacs-directory) nil t))
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(sw-time "sw-elpaca" (require 'sw-elpaca))

(sw-time "sw-theme" (require 'sw-theme))
(sw-time "sw-evil" (require 'sw-evil))
(sw-time "sw-modeline" (require 'sw-modeline))
(sw-time "sw-completion" (require 'sw-completion))
(sw-time "sw-project" (require 'sw-project))
(sw-time "sw-git" (require 'sw-git))
(sw-time "sw-terminal" (require 'sw-terminal))
(sw-time "sw-dired" (require 'sw-dired))
(sw-time "sw-filetypes" (require 'sw-filetypes))
(sw-time "sw-org" (require 'sw-org))
(sw-time "sw-programming" (require 'sw-programming))
(sw-time "sw-eglot" (require 'sw-eglot))
(sw-time "sw-tools" (require 'sw-tools))
(sw-time "sw-claude" (require 'sw-claude))
(sw-time "sw-workspaces" (require 'sw-workspaces))
(sw-time "sw-keybindings" (require 'sw-keybindings))
(sw-time "sw-dashboard" (require 'sw-dashboard))

;; Results
(let ((total (float-time (time-subtract (current-time) sw-test-start))))
  (princ "\nModule breakdown:\n")
  (princ "------------------------------\n")
  (dolist (item (sort sw-test-times (lambda (a b) (> (cdr a) (cdr b)))))
    (princ (format "%-20s %6.3fs\n" (car item) (cdr item))))
  (princ "------------------------------\n")
  (princ (format "%-20s %6.3fs\n" "TOTAL" total))
  (princ "\n")
  (if (< total sw-test-threshold)
      (progn
        (princ (format "PASS (%.2fs < %.2fs)\n" total sw-test-threshold))
        (kill-emacs 0))
    (princ (format "FAIL: %.2fs exceeds %.2fs threshold\n" total sw-test-threshold))
    (kill-emacs 1)))

;;; test-startup.el ends here
