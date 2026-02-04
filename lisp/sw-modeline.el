;;; sw-modeline.el --- Echo area info -*- lexical-binding: t -*-

;;; Commentary:
;; Display buffer info in echo area, minimal modeline as window border.

;;; Code:

;; Echo area info display
(defun sw--update-echo-area ()
  "Update echo area with buffer info."
  (unless (active-minibuffer-window)
    (let* ((info (format "%s%s  %s %s,%d"
                         (if (buffer-modified-p) "** " "")
                         (buffer-name)
                         (format-mode-line "%p")
                         (format-mode-line "%l")
                         (current-column)))
           (cur (or (current-message) ""))
           (padding (- (frame-width) (length cur) (length info) 1))
           (msg (concat cur (make-string (max 1 padding) ?\s) info)))
      (let ((message-log-max nil))
        (message "%s" msg)))))

(add-hook 'post-command-hook #'sw--update-echo-area)


;; Modeline is only used to distinguish active window via color.
;; Hide it when there's only one window since there's nothing to distinguish.
(defun sw--update-mode-line-visibility ()
  "Show mode-line only when frame has multiple windows."
  (let ((fmt (if (> (count-windows) 1) " " nil)))
    (dolist (win (window-list))
      (with-selected-window win
        (unless (eq mode-line-format fmt)
          (setq mode-line-format fmt))))))

(add-hook 'window-configuration-change-hook #'sw--update-mode-line-visibility)

(set-face-attribute 'mode-line nil
                    :background "#e63946"
                    :box nil
                    :height 0.1)

(set-face-attribute 'mode-line-inactive nil
                    :background "#333333"
                    :box nil
                    :height 0.1)

(provide 'sw-modeline)
;;; sw-modeline.el ends here
