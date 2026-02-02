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

;; Buffer count cache
(defvar sw-buffer-count-cache 0
  "Cached count of user-visible buffers.")

(defvar sw-workspace-buffer-count-cache 0
  "Cached count of buffers in current workspace.")

(defun sw-update-buffer-count ()
  "Update the cached buffer counts for modeline display.
Updates both `sw-buffer-count-cache' (all user-visible buffers) and
`sw-workspace-buffer-count-cache' (buffers in current workspace)."
  (setq sw-buffer-count-cache
        (cl-count-if
         (lambda (b)
           (or (buffer-file-name b)
               (not (string-match "^ " (buffer-name b)))))
         (buffer-list)))
  (setq sw-workspace-buffer-count-cache
        (length (sw-workspace-buffer-list))))

(add-hook 'buffer-list-update-hook #'sw-update-buffer-count)

(defun sw-number-of-buffers ()
  "Return the cached count of buffers."
  sw-buffer-count-cache)

(defun sw-number-of-workspace-buffers ()
  "Return the cached count of workspace buffers."
  sw-workspace-buffer-count-cache)

;; Custom mode-line format
;; Displays: buffer status, buffer name, buffer count, position, VC info, major mode
(setq-default mode-line-format
              `("%e"
                ,mode-line-front-space
                ,mode-line-client
                ,mode-line-modified
                ,mode-line-remote
                ,mode-line-frame-identification
                ,mode-line-buffer-identification
                (:eval (format "  b(%s/%s)"
                               (sw-number-of-workspace-buffers)
                               (sw-number-of-buffers)))
                " %p %l,%c  "
                (vc-mode vc-mode)
                " "
                ,mode-name
                " "
                ,mode-line-misc-info
                ,mode-line-end-spaces))

;; Orange modeline for active window
(set-face-attribute 'mode-line nil
                    :background "#ffb86c"
                    :foreground "#1a1a1a"
                    :weight 'bold)

(provide 'sw-modeline)
;;; sw-modeline.el ends here
