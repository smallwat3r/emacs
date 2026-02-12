;;; sw-center.el --- Center buffer content -*- lexical-binding: t -*-

;;; Commentary:
;; Global minor mode that centers buffer content using window margins.
;; Automatically removes margins when side-by-side windows exist.

;;; Code:

(defvar sw-center-width 100
  "Text width for centering. When nil, uses `fill-column'.")

(defun sw-center--has-hsplit-p (node)
  "Return non-nil if window tree NODE contains a horizontal split."
  (when (listp node)
    (or (not (car node))
        (cl-some #'sw-center--has-hsplit-p
                 (cddr node)))))

(defun sw-center--update (&rest _)
  "Update centering margins on all windows."
  (let ((squash (sw-center--has-hsplit-p
                 (car (window-tree)))))
    (walk-windows
     (lambda (w)
       (set-window-margins w 0 0)
       (let* ((target (or sw-center-width fill-column 100))
              (available (window-body-width w))
              (margin (if squash 0
                        (max 0 (/ (- available target)
                                  2)))))
         (set-window-margins w margin margin)))
     'no-mini)))

(defun sw-center--clear ()
  "Remove centering margins from all windows."
  (walk-windows
   (lambda (w) (set-window-margins w 0 0))
   'no-mini))

(define-minor-mode sw-center-mode
  "Center buffer text with left and right window margins.
Margins are removed when the frame has side-by-side windows."
  :global t :lighter nil
  (if sw-center-mode
      (progn
        (add-hook 'window-configuration-change-hook
                  #'sw-center--update)
        (sw-center--update))
    (remove-hook 'window-configuration-change-hook
                 #'sw-center--update)
    (sw-center--clear)))

(provide 'sw-center)
;;; sw-center.el ends here
