;;; init.el --- Main configuration -*- lexical-binding: t -*-

;; Copyright (C) 2026 Matthieu Petiteau
;; Author: Matthieu Petiteau <mpetiteau.pro@gmail.com>

;;; Commentary:
;; Personal Emacs configuration - fast and minimal.

;;; Code:

;;; Bootstrap

;; Add lisp/ and site-lisp/ to load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(let ((site-lisp (expand-file-name "site-lisp" user-emacs-directory)))
  (when (file-directory-p site-lisp)
    (dolist (dir (directory-files site-lisp t "^[^.]"))
      (when (file-directory-p dir)
        (add-to-list 'load-path dir)))))

;; Initialize package management
(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Refresh package contents only if archive cache doesn't exist
;; (Run M-x package-refresh-contents manually if needed)
(unless (file-exists-p (expand-file-name "archives/melpa/archive-contents"
                                         package-user-dir))
  (package-refresh-contents))

;; Ensure use-package is available (built-in since Emacs 29)
(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t
      use-package-always-defer t
      use-package-expand-minimally t
      use-package-compute-statistics nil)

;; List of packages to install (run M-x sw/install-packages to install)
(defvar sw/required-packages
  '(;; Core
    evil evil-collection evil-goggles evil-snipe evil-surround
    evil-nerd-commenter evil-visualstar evil-anzu anzu general which-key
    ;; Completion
    vertico orderless consult marginalia embark embark-consult corfu cape
    ;; Theme & UI
    creamy-theme simplicity-theme hl-todo rainbow-delimiters
    highlight-numbers symbol-overlay nerd-icons
    ;; Git
    magit diff-hl git-timemachine blamer git-modes browse-at-remote
    ;; Programming
    apheleia editorconfig markdown-mode web-mode dockerfile-mode docker
    terraform-mode lua-mode nginx-mode cargo pet yasnippet yasnippet-snippets
    ;; Tools
    helpful eat restart-emacs restclient pdf-tools
    pass password-store
    ;; Dired
    dired-narrow dired-subtree nerd-icons-dired diredfl
    ;; Org
    org-modern org-journal toc-org
    ;; Eglot
    consult-eglot
    ;; Misc
    imenu-list logview)
  "List of packages required by this configuration.")

(defun sw/install-packages ()
  "Install all missing packages from `sw/required-packages'."
  (interactive)
  (package-refresh-contents)
  (dolist (pkg sw/required-packages)
    (unless (package-installed-p pkg)
      (condition-case err
          (package-install pkg)
        (error (message "Failed to install %s: %s" pkg err)))))
  (message "Package installation complete!"))

;;; System detection

(defconst sw/is-linux (eq system-type 'gnu/linux))
(defconst sw/is-mac (eq system-type 'darwin))

(defconst sw/is-fedora
  (and sw/is-linux
       (let ((os-release (ignore-errors
                           (shell-command-to-string "cat /etc/os-release"))))
         (and os-release (string-match-p "fedora" (downcase os-release))))))

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
  :ensure nil
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
(require 'sw-keybindings)
(require 'sw-dashboard)

;;; Dashboard setup

(sw/dashboard-setup)

;;; init.el ends here
