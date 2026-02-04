;;; early-init.el --- Early initialization -*- lexical-binding: t -*-

;; Copyright (C) 2026 Matthieu Petiteau
;; Author: Matthieu Petiteau <mpetiteau.pro@gmail.com>

;;; Commentary:
;; This file is loaded before init.el, before package and UI initialization.
;; Use it for performance-critical settings that must be set early.

;;; Code:

;;; System detection (available early for font/frame setup)

(defconst sw-is-linux (eq system-type 'gnu/linux))
(defconst sw-is-mac (eq system-type 'darwin))

;;; Custom initialization hooks
;; These hooks allow deferring initialization until truly needed.
;; Use (add-hook 'sw-first-input-hook #'some-mode) in use-package :config blocks.

(defvar sw-first-input-hook nil
  "Transient hook run before the first user input.")

(defvar sw-first-file-hook nil
  "Transient hook run before the first file is opened.")

(defvar sw-first-buffer-hook nil
  "Transient hook run before the first buffer switch.")

;; Explicit mapping of transient hooks to their Emacs trigger hooks.
;; Avoids fragile intern-based function name construction.
(defvar sw--hook-triggers nil
  "Alist mapping (hook-var . (trigger-hook . trigger-fn)).")

(defun sw--run-hook-once (hook-var)
  "Run HOOK-VAR once, then remove its trigger from the parent hook."
  (when (and after-init-time (symbol-value hook-var))
    (run-hooks hook-var)
    (set hook-var nil)
    (when-let ((entry (assq hook-var sw--hook-triggers)))
      (remove-hook (cadr entry) (cddr entry)))))

(defun sw--trigger-first-input ()
  "Trigger `sw-first-input-hook'."
  (sw--run-hook-once 'sw-first-input-hook))

(defun sw--trigger-first-file ()
  "Trigger `sw-first-file-hook'."
  (sw--run-hook-once 'sw-first-file-hook))

(defun sw--trigger-first-buffer (&rest _)
  "Trigger `sw-first-buffer-hook'."
  (sw--run-hook-once 'sw-first-buffer-hook))

(defun sw-setup-initial-hooks ()
  "Set up triggers for first-* hooks. Call this after init."
  (setq sw--hook-triggers
        `((sw-first-input-hook  . (pre-command-hook . sw--trigger-first-input))
          (sw-first-file-hook   . (find-file-hook . sw--trigger-first-file))
          (sw-first-buffer-hook . (window-buffer-change-functions
                                   . sw--trigger-first-buffer))))
  (dolist (entry sw--hook-triggers)
    (add-hook (cadr entry) (cddr entry))))

(add-hook 'emacs-startup-hook #'sw-setup-initial-hooks)

;; Increase gc threshold during startup for faster loading
;; gcmh-mode (loaded in init.el) handles GC during idle time
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

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

;;; Font management

(defun sw-font-available-p (font)
  "Return non-nil if FONT is available on the system."
  (member font (font-family-list)))

(defun sw-first-available-font (fonts)
  "Return the first available font from FONTS list, or nil if none found."
  (seq-find #'sw-font-available-p fonts))

;; Primary font families (set early, before display is available)
(defvar sw-font-family (if sw-is-mac "Monaco" "MonacoB")
  "Primary monospace font for code and default text.")

(defvar sw-font-variable-pitch sw-font-family
  "Variable-pitch font for prose and UI elements.")

(defvar sw-font-serif sw-font-family
  "Serif font for documents and reading.")

;; Fallback font lists (resolved after display is available)
(defvar sw-font-symbol-fallbacks
  '("Symbola" "Apple Symbols" "Noto Sans Symbols" "Noto Sans Symbols 2")
  "Fallback list for symbol font.")

(defvar sw-font-emoji-fallbacks
  (if sw-is-mac
      '("Apple Color Emoji")
    '("Noto Color Emoji" "Noto Emoji" "Segoe UI Emoji"))
  "Fallback list for emoji font.")

(defvar sw-font-symbol nil
  "Font for mathematical and technical symbols. Set after init.")

(defvar sw-font-emoji nil
  "Font for emoji characters. Set after init.")

(defvar sw-font-height (if sw-is-mac 130 100)
  "Default font height in 1/10 points.")

(defvar sw-font-variable-pitch-height sw-font-height
  "Variable-pitch font height in 1/10 points.")

;; Set primary font early to prevent random sizing
(set-face-attribute 'default nil :family sw-font-family :height sw-font-height)
(push `(font . ,(format "%s-%d" sw-font-family (/ sw-font-height 10)))
      default-frame-alist)

;; Prevent font cache compaction for better performance
(setq inhibit-compacting-font-caches t)

;; Prevent flickering during resize
(setq frame-inhibit-implied-resize t)

;; Faster file-name-handler during startup
(defvar sw-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist sw-file-name-handler-alist)))

;; Native compilation settings
(setq native-comp-async-report-warnings-errors 'silent
      native-comp-jit-compilation t
      warning-suppress-log-types '((comp))
      warning-suppress-types '((comp)))

;; Prefer newer .el files over older .elc
(setq load-prefer-newer t)

;; Remove .so from load-suffixes to reduce file operations during load
(setq load-suffixes (remove ".so" load-suffixes))

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
