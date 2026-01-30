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
   (sw/is-fedora 13)
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
  (let ((frame (or frame (selected-frame))))
    (when (and (not sw/fonts-configured)
               (display-graphic-p frame))
      (let* ((size (sw/get-font-size))
             (font-spec
              (cond
               (sw/is-gpd
                (sw/safe-font '("MonacoB" "Monospace") :size size))
               (sw/is-linux
                (sw/safe-font '("MonacoB" "Triplicate A Code" "Monospace") :size size))
               (sw/is-mac
                (sw/safe-font '("Triplicate A Code" "Monaco") :size size))
               (t
                (sw/safe-font '("Triplicate A Code" "Monospace") :size size)))))
        (when font-spec
          (set-face-attribute 'default nil :font font-spec)
          (set-face-attribute 'fixed-pitch nil :font font-spec)
          (set-face-attribute 'variable-pitch nil :font font-spec)))
      (setq sw/fonts-configured t)
      ;; Clean up hooks after configuration
      (remove-function after-focus-change-function #'sw/configure-fonts)
      (remove-hook 'window-setup-hook #'sw/configure-fonts))))

;; Defer font configuration until the frame is ready
(if (daemonp)
    (add-function :after after-focus-change-function #'sw/configure-fonts)
  (add-hook 'window-setup-hook #'sw/configure-fonts))

;; Line spacing
(setq-default line-spacing 2)

(provide 'sw-fonts)
;;; sw-fonts.el ends here
