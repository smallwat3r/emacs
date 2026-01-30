;;; early-init.el --- Early initialization -*- lexical-binding: t -*-

;; Copyright (C) 2026 Matthieu Petiteau
;; Author: Matthieu Petiteau <mpetiteau.pro@gmail.com>

;;; Commentary:
;; This file is loaded before init.el, before package and UI initialization.
;; Use it for performance-critical settings that must be set early.

;;; Code:

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
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)
(push '(internal-border-width . 0) default-frame-alist)
(push '(fullscreen . maximized) default-frame-alist)

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
      native-comp-deferred-compilation t
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
