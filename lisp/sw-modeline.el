;;; sw-modeline.el --- Frame title info -*- lexical-binding: t -*-

;;; Commentary:
;; Display buffer info in frame title, no modeline.

;;; Code:

(require 'sw-workspaces)

;; Show counter while in search modes
(use-package evil-anzu
  :ensure (:wait t)
  :after evil
  :demand t
  :config
  (global-anzu-mode 1))

;; Buffer count
(defvar sw--buffer-count-cache nil
  "Cached (workspace . total) buffer counts.")

(defvar sw--buffer-count-tick 0
  "Tick to invalidate buffer count cache.")

(defun sw--invalidate-buffer-count ()
  "Mark buffer count cache as stale."
  (cl-incf sw--buffer-count-tick))

(add-hook 'kill-buffer-hook #'sw--invalidate-buffer-count)
(add-hook 'find-file-hook #'sw--invalidate-buffer-count)

(defun sw--buffer-counts ()
  "Return (workspace . total) buffer counts, cached per tick."
  (unless (eq (car sw--buffer-count-cache) sw--buffer-count-tick)
    (setq sw--buffer-count-cache
          (cons sw--buffer-count-tick
                (cons (length (sw-workspace-buffer-list))
                      (cl-count-if
                       (lambda (b)
                         (or (buffer-file-name b)
                             (not (string-prefix-p " " (buffer-name b)))))
                       (buffer-list))))))
  (cdr sw--buffer-count-cache))

(defun sw-number-of-buffers ()
  "Return the count of user-visible buffers."
  (cdr (sw--buffer-counts)))

(defun sw-number-of-workspace-buffers ()
  "Return the count of workspace buffers."
  (car (sw--buffer-counts)))

;; Frame title with position info
(defun sw--frame-title ()
  "Return frame title string."
  (format "%s%s  %s %s,%d"
          (if (buffer-modified-p) "** " "")
          (buffer-name)
          (format-mode-line "%p")
          (format-mode-line "%l")
          (current-column)))

(defun sw--update-frame-title ()
  "Update frame title with current position."
  (set-frame-parameter nil 'name (sw--frame-title)))

(add-hook 'post-command-hook #'sw--update-frame-title)

;; Minimal modeline as colored border, hidden when single window
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
