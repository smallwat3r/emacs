;;; sw-claude.el --- Claude Code integration -*- lexical-binding: t -*-

;;; Commentary:
;; Integration with claude-code CLI for AI-assisted coding.
;; Claude runs inside a Docker sandbox (see docker/claude-sandbox/)
;; via a wrapper script (bin/claude-docker) that mounts the project
;; directory and forwards auth, git config, and SSH agent.

;;; Code:

;; Forward-declare so the dynamic `let' binding in
;; `sw-claude-start-advice' is valid under lexical-binding.
(defvar eat-kill-buffer-on-exit)

(defconst sw-claude-docker-script
  (expand-file-name "bin/claude-docker" user-emacs-directory)
  "Path to the Docker wrapper script for sandboxed Claude.")

(defun sw-claude-notify (title message)
  "Display a Linux notification with TITLE and MESSAGE using notify-send."
  (when (and sw-is-linux (executable-find "notify-send"))
    (call-process "notify-send" nil nil nil title message)))

(defun sw-claude-display-buffer-full-frame (buffer)
  "Display claude-code BUFFER.
If only one window exists, use full frame.
If in a split view, display in the current window."
  (display-buffer buffer
                  (if (one-window-p)
                      '((display-buffer-full-frame))
                    '((display-buffer-same-window)))))

(defun sw-claude-rebuild-sandbox ()
  "Force rebuild the Claude Docker sandbox image."
  (interactive)
  (let ((default-directory user-emacs-directory))
    (compile "docker build --no-cache -t claude-code-sandbox docker/claude-sandbox/")))

;; Required dependency for claude-code
(use-package inheritenv
  :ensure (:host github :repo "purcell/inheritenv" :wait t)
  :demand t)

;; Claude Code (sandboxed via Docker)
(use-package claude-code
  :ensure (:host github :repo "stevemolitor/claude-code.el" :wait t)
  :when (and (executable-find "docker")
             (file-executable-p sw-claude-docker-script))
  :after inheritenv
  :init
  (setq claude-code-program sw-claude-docker-script
        claude-code-program-switches
        '("--dangerously-skip-permissions")
        claude-code-terminal-backend 'eat
        claude-code-notification-function #'sw-claude-notify
        claude-code-toggle-auto-select t
        claude-code-display-window-fn
        #'sw-claude-display-buffer-full-frame)

  ;; Custom toggle that uses full frame display
  ;; Defined in :init so the command exists before the package is loaded
  (defun sw-claude-code-toggle ()
    "Show or hide the Claude window in full frame."
    (interactive)
    (require 'claude-code)
    (let ((claude-code-buffer (claude-code--get-or-prompt-for-buffer)))
      (if claude-code-buffer
          (if (get-buffer-window claude-code-buffer)
              (delete-window (get-buffer-window claude-code-buffer))
            (let ((window (sw-claude-display-buffer-full-frame claude-code-buffer)))
              (set-window-parameter window 'no-delete-other-windows
                                    claude-code-no-delete-other-windows)
              (when claude-code-toggle-auto-select
                (select-window window))))
        (claude-code--show-not-running-message))))

  :config
  (advice-add 'claude-code-toggle :override #'sw-claude-code-toggle)

  (defun sw-claude-start-advice (orig-fn &rest args)
    "Advice around `claude-code--start'.
Kill stale Claude buffers, then run with `eat-kill-buffer-on-exit'
disabled so eat does not kill the buffer if the process exits
during the startup delay."
    (dolist (buf (buffer-list))
      (when (and (string-prefix-p "*claude:" (buffer-name buf))
                 (not (get-buffer-process buf)))
        (kill-buffer buf)))
    (let ((eat-kill-buffer-on-exit nil))
      (apply orig-fn args)))

  (advice-add 'claude-code--start :around #'sw-claude-start-advice))

(provide 'sw-claude)
;;; sw-claude.el ends here
