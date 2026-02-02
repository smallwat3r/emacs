;;; sw-project.el --- Project management -*- lexical-binding: t -*-

;;; Commentary:
;; Project management using built-in project.el (replaces projectile).
;; project.el is lighter and integrates well with consult.

;;; Code:

(use-package project
  :ensure nil
  :demand t
  :custom
  ;; Remember known projects
  (project-list-file (expand-file-name "projects" user-emacs-directory))
  ;; Go directly to find-file when switching projects
  (project-switch-commands #'project-find-file)
  :config
  ;; Add additional root markers
  (setq project-vc-extra-root-markers
        '(".project" ".projectile" "Cargo.toml" "go.mod" "package.json"
          "pyproject.toml" "setup.py" "Makefile" ".git")))

;; Auto-discover projects in these directories
(defcustom sw/project-directories '("~/code" "~/work" "~/dotfiles")
  "Directories to scan for projects."
  :type '(repeat directory)
  :group 'sw)

(defun sw/project-discover ()
  "Scan `sw/project-directories' for git repos and add them as known projects."
  (interactive)
  (message "Discovering projects...")
  (dolist (dir sw/project-directories)
    (let ((expanded (expand-file-name dir)))
      (when (file-directory-p expanded)
        (project-remember-projects-under expanded nil))))
  (message "Project discovery complete."))

;; Only scan on first run (when project list is empty or doesn't exist)
(unless (and (file-exists-p project-list-file)
             (> (file-attribute-size (file-attributes project-list-file)) 10))
  (run-with-idle-timer 2 nil #'sw/project-discover))

(defun sw/project-root-or-default (&optional prefer-remote)
  "Return project root or `default-directory'.
When PREFER-REMOTE is non-nil and in a remote directory, return that directly."
  (if (and prefer-remote (file-remote-p default-directory))
      default-directory
    (or (when-let ((proj (project-current)))
          (project-root proj))
        default-directory)))

(defun sw/project-find-file ()
  "Find file in current project using fd."
  (interactive)
  (require 'consult)
  (consult-fd (sw/project-root-or-default)))

(defun sw/consult-ripgrep-project ()
  "Search in current project with ripgrep."
  (interactive)
  (require 'consult)
  (consult-ripgrep (sw/project-root-or-default)))

(defun sw/consult-ripgrep-project-symbol ()
  "Search for symbol at point in project."
  (interactive)
  (require 'consult)
  (consult-ripgrep (sw/project-root-or-default) (thing-at-point 'symbol t)))

(defun sw/project-make ()
  "Run make in project root."
  (interactive)
  (let ((default-directory (sw/project-root-or-default)))
    (call-interactively #'compile)))

(defun sw/project-refresh ()
  "Refresh the known projects list.
Removes projects that no longer exist and re-scans `sw/project-directories'
for new projects."
  (interactive)
  (project-forget-zombie-projects)
  (sw/project-discover))

(provide 'sw-project)
;;; sw-project.el ends here
