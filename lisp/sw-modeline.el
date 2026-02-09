;;; sw-modeline.el --- Echo area info -*- lexical-binding: t -*-

;;; Commentary:
;; Display buffer info in echo area, minimal modeline as window border.

;;; Code:

;; Echo area info display
(defvar sw--echo-area-last-info nil
  "Cached (INFO . CUR-MSG) from the last echo area update.")

(defvar sw--echo-area-last-message nil
  "The last message string written by `sw--update-echo-area'.")

(defun sw--update-echo-area ()
  "Update echo area with buffer info.
Skips redisplay when neither the info string nor the current
message have changed since the last call."
  (unless (active-minibuffer-window)
    (let* ((info (format "%s%s  %s %s,%d"
                         (if (buffer-modified-p) "** " "")
                         (buffer-name)
                         (format-mode-line "%p")
                         (format-mode-line "%l")
                         (current-column)))
           (raw (current-message))
           (cur (if (equal raw sw--echo-area-last-message)
                    "" (or raw ""))))
      (unless (and sw--echo-area-last-info
                   (equal info (car sw--echo-area-last-info))
                   (equal cur (cdr sw--echo-area-last-info)))
        (setq sw--echo-area-last-info (cons info cur))
        (let* ((padding (- (frame-width)
                           (length cur) (length info) 1))
               (msg (concat cur
                            (make-string (max 1 padding) ?\s)
                            info))
               (message-log-max nil))
          (setq sw--echo-area-last-message msg)
          (message "%s" msg))))))

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
