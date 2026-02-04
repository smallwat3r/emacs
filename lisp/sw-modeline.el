;;; sw-modeline.el --- Custom modeline -*- lexical-binding: t -*-

;;; Commentary:
;; Custom modeline with buffer count, position, and VC info.

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

;; Custom mode-line format
(setq-default mode-line-format
              '("%e"
                " "
                mode-line-modified
                mode-line-remote
                " "
                mode-line-buffer-identification
                "  "
                "%p %l,%c"
                "  "
                (:eval (format "%d:%d"
                               (sw-number-of-workspace-buffers)
                               (sw-number-of-buffers)))
                mode-line-format-right-align
                (:eval (when vc-mode
                         (propertize (substring vc-mode 1) 'face 'bold)))
                "  "
                mode-name
                " "))

;; Orange modeline for active window
(set-face-attribute 'mode-line nil
                    :background "#ffb86c"
                    :foreground "#1a1a1a"
                    :weight 'bold)

(provide 'sw-modeline)
;;; sw-modeline.el ends here
