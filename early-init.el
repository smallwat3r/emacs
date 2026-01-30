;;; early-init.el --- Early initialization -*- lexical-binding: t -*-

;; Copyright (C) 2026 Matthieu Petiteau
;; Author: Matthieu Petiteau <mpetiteau.pro@gmail.com>

;;; Commentary:
;; This file is loaded before init.el, before package and UI initialization.
;; Use it for performance-critical settings that must be set early.

;;; Code:

;;; System detection (available early for font/frame setup)

(defconst sw/is-linux (eq system-type 'gnu/linux))
(defconst sw/is-mac (eq system-type 'darwin))

;; Increase gc threshold during startup for faster loading
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Restore reasonable values after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024)
                  gc-cons-percentage 0.1)))

;; Prevent package.el from auto-initializing
(setq package-enable-at-startup nil)

;; Disable expensive GUI elements early
(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      initial-scratch-message nil
      initial-major-mode 'fundamental-mode)

;; Disable GUI elements before they load
(setq default-frame-alist
      (append '((menu-bar-lines . 0)
                (tool-bar-lines . 0)
                (vertical-scroll-bars)
                (horizontal-scroll-bars)
                (internal-border-width . 0)
                (fullscreen . maximized))
              default-frame-alist))

;; Set font early to prevent random sizing
(defvar sw/font-family (if sw/is-mac "Monaco" "MonacoB"))
(defvar sw/font-height (if sw/is-mac 130 100))

(set-face-attribute 'default nil :family sw/font-family :height sw/font-height)
(push `(font . ,(format "%s-%d" sw/font-family (/ sw/font-height 10))) default-frame-alist)

;; Prevent flickering during resize
(setq frame-inhibit-implied-resize t)

;; Faster file-name-handler during startup
(defvar sw/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist sw/file-name-handler-alist)))

;; Native compilation settings
(setq native-comp-async-report-warnings-errors 'silent
      native-comp-jit-compilation t
      warning-suppress-log-types '((comp))
      warning-suppress-types '((comp)))

;; Prefer newer .el files over older .elc
(setq load-prefer-newer t)

;; Disable bidirectional text scanning for performance
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;; Faster rendering
(setq-default truncate-lines t)
(setq truncate-partial-width-windows nil
      fast-but-imprecise-scrolling t
      redisplay-skip-fontification-on-input t)

;;; early-init.el ends here
