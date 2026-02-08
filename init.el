;;; init.el --- Main configuration -*- lexical-binding: t -*-

;; Copyright (C) 2026 Matthieu Petiteau
;; Author: Matthieu Petiteau <mpetiteau.pro@gmail.com>

;;; Commentary:
;; Personal Emacs configuration - fast and minimal.

;;; Code:

;;; Bootstrap

;; Add lisp/ to load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Bootstrap elpaca and use-package
(require 'sw-elpaca)

;;; Personal info

(defconst sw-full-name "Matt Petiteau")
(defconst sw-email-addresses '("mpetiteau.pro@gmail.com" "matt@smallwat3r.com"))
(defconst sw-email (car sw-email-addresses))
(defconst sw-dotfiles-directory "~/dotfiles/")

;;; Core settings

(use-package emacs
  :ensure nil
  :demand t
  :custom
  (user-full-name sw-full-name)
  (user-mail-address sw-email)

  ;; General behavior (startup settings in early-init.el)
  (ring-bell-function 'ignore)
  (use-short-answers t)
  (use-dialog-box nil)
  (confirm-kill-emacs 'y-or-n-p)

  ;; Files and backups
  (make-backup-files nil)
  (auto-save-default nil)
  (create-lockfiles nil)
  (require-final-newline t)

  ;; Editing
  (indent-tabs-mode nil)
  (tab-width 4)
  (fill-column 100)
  (sentence-end-double-space nil)
  (delete-selection-mode t)

  ;; Scrolling
  (scroll-margin 2)
  (scroll-conservatively 101)
  (scroll-preserve-screen-position t)
  (auto-window-vscroll nil)

  ;; Display
  (show-paren-delay 0)
  (indicate-empty-lines nil)
  (indicate-buffer-boundaries nil)

  ;; Performance
  (read-process-output-max (* 1024 1024))
  (process-adaptive-read-buffering nil)

  ;; Minibuffer
  (enable-recursive-minibuffers t)
  (minibuffer-depth-indicate-mode t)

  :config
  ;; UTF-8 everywhere
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)

  ;; Enable useful modes
  (column-number-mode 1)
  (electric-pair-mode 1)
  (global-auto-revert-mode 1)
  (pixel-scroll-precision-mode 1)
  (repeat-mode 1)

  ;; Deferred modes (via custom hooks from early-init.el)
  (add-hook 'sw-first-buffer-hook #'show-paren-mode)
  (add-hook 'sw-first-file-hook #'save-place-mode)
  (add-hook 'sw-first-file-hook #'recentf-mode)
  (add-hook 'sw-first-file-hook #'global-so-long-mode)

  ;; Disable blinking cursor
  (blink-cursor-mode -1)

  ;; Clean up whitespace on save
  (add-hook 'before-save-hook #'whitespace-cleanup))

;;; Garbage collection during idle time

(use-package gcmh
  :hook (sw-first-buffer . gcmh-mode)
  :custom
  (gcmh-idle-delay 'auto)
  (gcmh-auto-idle-delay-factor 10)
  (gcmh-high-cons-threshold (* 64 1024 1024)))

;;; Custom file (keep init.el clean)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file 'noerror 'nomessage))

;;; Sync shell PATH into Emacs

(use-package exec-path-from-shell
  :demand t
  :config
  (exec-path-from-shell-initialize))

;;; Early dependencies

;; Transient needs to be updated before magit/claude-code load
(use-package transient :ensure (:wait t))

;;; Load modules

;; Wait for all queued packages to be installed before loading modules
(elpaca-wait)

(require 'sw-theme)
(require 'sw-evil)
(require 'sw-modeline)
(require 'sw-completion)
(require 'sw-project)
(require 'sw-git)
(require 'sw-terminal)
(require 'sw-tailscale)
(require 'sw-dired)
(require 'sw-org)
(require 'sw-programming)
(require 'sw-eglot)
(require 'sw-tools)
(require 'sw-claude)
(require 'sw-workspaces)
(require 'sw-keybindings)
(require 'sw-dashboard)

;;; Dashboard setup

(sw-dashboard-setup)

;;; init.el ends here
