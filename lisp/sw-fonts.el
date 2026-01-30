;;; sw-fonts.el --- Font configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Font setup with adaptive sizing based on system, with fallback support.

;;; Code:

(defvar sw/fonts-configured nil
  "Non-nil if fonts have been successfully configured.")

(defun sw/get-font-size ()
  "Return a font size depending on the device or OS."
  (cond
   (sw/is-gpd 13)
   (sw/is-linux 13)
   (sw/is-mac 17)
   (t 13)))

(defun sw/font-available-p (font)
  "Return non-nil if FONT is available on this system."
  (and (find-font (font-spec :name font)) t))

(defun sw/safe-font (fonts &rest spec)
  "Return a font-spec using the first available font in FONTS.
SPEC are additional arguments passed to `font-spec'."
  (let ((available
         (seq-find (lambda (f)
                     (if (sw/font-available-p f) t
                       (message "Warning: font not found: %s" f) nil))
                   fonts)))
    (when available
      (apply #'font-spec :family available spec))))

(defun sw/configure-fonts (&optional frame)
  "Configure fonts when a graphical display is available.
FRAME is the frame to check for display capability."
  (when (and (not sw/fonts-configured)
             (display-graphic-p (or frame (selected-frame))))
    (let* ((fonts (cond (sw/is-linux '("MonacoB" "Monospace"))
                        (sw/is-mac '("Monaco"))
                        (t '("Monospace"))))
           (font-spec (sw/safe-font fonts :size (sw/get-font-size))))
      (when font-spec
        (dolist (face '(default fixed-pitch variable-pitch))
          (set-face-attribute face nil :font font-spec))))
    (setq sw/fonts-configured t)
    ;; Remove hooks after configuration (only needed once)
    (remove-function after-focus-change-function #'sw/configure-fonts)
    (remove-hook 'window-setup-hook #'sw/configure-fonts)))

;; Defer font configuration until the frame is ready
(if (daemonp)
    (add-function :after after-focus-change-function #'sw/configure-fonts)
  (add-hook 'window-setup-hook #'sw/configure-fonts))

;; Line spacing
(setq-default line-spacing 2)

(provide 'sw-fonts)
;;; sw-fonts.el ends here
