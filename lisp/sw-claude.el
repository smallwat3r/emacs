;;; sw-claude.el --- Claude Code integration -*- lexical-binding: t -*-

;;; Commentary:
;; Integration with claude-code CLI for AI-assisted coding.
;; Claude runs inside a Docker sandbox (see docker/claude-sandbox/)
;; via a wrapper script (bin/claude-docker) that mounts the project
;; directory and forwards auth, git config, and SSH agent.

;;; Code:

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
    (compile (format "docker build --no-cache --build-arg HOST_HOME=%s \
-t claude-code-sandbox docker/claude-sandbox/"
                     (shell-quote-argument (expand-file-name "~"))))))

(defun sw-claude--filter-substring (beg end &optional delete)
  "Like `buffer-substring--filter' but strip trailing whitespace per line.
Claude's TUI pads each line to terminal width, so plain copies pick
up the padding instead of clean newlines."
  (let ((text (buffer-substring--filter beg end delete)))
    (if (stringp text)
        (replace-regexp-in-string "[ \t]+$" "" text)
      text)))

(defun sw-claude--setup-eat-buffer ()
  "Tweak eat settings in claude-code buffers.
Strip trailing whitespace from copies, and keep the mouse wheel
scrolling the Emacs window: when Claude enables mouse tracking,
eat would otherwise forward wheel events to the TUI, making
scrollback appear stuck."
  (when (string-prefix-p "*claude:" (buffer-name))
    (setq-local filter-buffer-substring-function
                #'sw-claude--filter-substring)
    (setq-local eat-enable-mouse nil)))

(add-hook 'eat-mode-hook #'sw-claude--setup-eat-buffer)

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

  ;; Defined in :init so the command exists before the package is loaded
  (defun sw-claude-with-dirs ()
    "Start Claude with extra project directories mounted in the sandbox.
Prompts for directories (empty answer to \"Add another\" stops), then
launches Claude with them passed to the Docker wrapper through
CLAUDE_DOCKER_EXTRA_DIRS, which mounts each one, shadows its .env
files and hands it to claude via --add-dir."
    (interactive)
    (let ((dirs (list (directory-file-name
                       (expand-file-name
                        (read-directory-name "Extra directory: "))))))
      (while (y-or-n-p "Add another directory? ")
        (push (directory-file-name
               (expand-file-name
                (read-directory-name "Extra directory: ")))
              dirs))
      (let ((process-environment
             (cons (concat "CLAUDE_DOCKER_EXTRA_DIRS="
                           (mapconcat #'identity (nreverse dirs) ":"))
                   process-environment)))
        (claude-code))))

  :config
  (advice-add 'claude-code-toggle :override #'sw-claude-code-toggle)

  (defvar eat-kill-buffer-on-exit)

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
