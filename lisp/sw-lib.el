;;; sw-lib.el --- Shared utilities -*- lexical-binding: t -*-

;;; Commentary:
;; Small helper functions shared across modules.

;;; Code:

(defun sw--string-min-indent (str)
  "Return minimum indentation (spaces) of non-blank lines in STR."
  (let ((min most-positive-fixnum))
    (dolist (line (split-string str "\n"))
      (when (string-match "^\\( *\\)[^ \t\n]" line)
        (setq min (min min (length (match-string 1 line))))))
    (if (= min most-positive-fixnum) 0 min)))

(defun sw--string-reindent (str old-indent new-indent)
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

(provide 'sw-lib)
;;; sw-lib.el ends here
