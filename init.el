;;; init.el --- Main configuration -*- lexical-binding: t -*-

;; Copyright (C) 2026 Matthieu Petiteau
;; Author: Matthieu Petiteau <mpetiteau.pro@gmail.com>

;;; Commentary:
;; Personal Emacs configuration - fast and minimal.

;;; Code:

;;; Bootstrap

;; Add lisp/ to load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
                         user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Integrate straight.el with use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t
      use-package-always-defer t
      use-package-expand-minimally t
      use-package-compute-statistics nil)

;;; System detection

(defconst sw/is-linux (eq system-type 'gnu/linux))
(defconst sw/is-mac (eq system-type 'darwin))

(defconst sw/is-fedora
  (and sw/is-linux
       (when-let ((os-release (ignore-errors
                                (shell-command-to-string "cat /etc/os-release"))))
         (and (string-match-p "fedora" (downcase os-release)) t))))

(defconst sw/is-gpd
  (and sw/is-linux
       (equal "GPD"
              (ignore-errors
                (string-trim
                 (with-temp-buffer
                   (insert-file-contents "/sys/devices/virtual/dmi/id/board_vendor")
                   (buffer-string)))))))

;;; Personal info

(defconst sw/full-name "Matt Petiteau")
(defconst sw/email-addresses '("mpetiteau.pro@gmail.com" "matt@smallwat3r.com"))
(defconst sw/email (car sw/email-addresses))

;;; Core settings

(use-package emacs
  :straight nil
  :demand t
  :custom
  (user-full-name sw/full-name)
  (user-mail-address sw/email)

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
  (show-paren-mode 1)
  (electric-pair-mode 1)
  (global-auto-revert-mode 1)
  (save-place-mode 1)
  (recentf-mode 1)
  (blink-cursor-mode -1)

  ;; Clean up whitespace on save
  (add-hook 'before-save-hook #'whitespace-cleanup))

;;; Custom file (keep init.el clean)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file 'noerror 'nomessage))

;;; Load modules

(require 'sw-fonts)
(require 'sw-theme)
(require 'sw-modeline)
(require 'sw-evil)
(require 'sw-completion)
(require 'sw-project)
(require 'sw-git)
(require 'sw-terminal)
(require 'sw-dired)
(require 'sw-filetypes)
(require 'sw-org)
(require 'sw-programming)
(require 'sw-eglot)
(require 'sw-tools)
(require 'sw-claude)
(require 'sw-workspaces)
(require 'sw-keybindings)
(require 'sw-dashboard)

;;; Dashboard setup

(sw/dashboard-setup)

;;; init.el ends here
