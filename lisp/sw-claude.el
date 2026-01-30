;;; sw-claude.el --- Claude Code integration -*- lexical-binding: t -*-

;;; Commentary:
;; Integration with claude-code CLI for AI-assisted coding.

;;; Code:

(defun sw/claude-notify (title message)
  "Display a Linux notification with TITLE and MESSAGE using notify-send."
  (when (and sw/is-linux (executable-find "notify-send"))
    (call-process "notify-send" nil nil nil title message)))

(defun sw/claude-display-buffer-full-frame (buffer)
  "Display claude-code BUFFER in full frame."
  (display-buffer buffer '((display-buffer-full-frame))))

;; Required dependency for claude-code
(use-package inheritenv
  :straight (:host github :repo "purcell/inheritenv")
  :demand t)

;; Claude Code
(use-package claude-code
  :straight (:host github :repo "stevemolitor/claude-code.el")
  :when (executable-find "claude")
  :after inheritenv
  :commands (claude-code
             claude-code-continue
             claude-code-resume
             claude-code-new-instance
             claude-code-start-in-directory
             claude-code-switch-to-buffer
             claude-code-toggle
             claude-code-send-region
             claude-code-send-command
             claude-code-send-command-with-context
             claude-code-send-buffer-file
             claude-code-fix-error-at-point
             claude-code-cycle-mode
             claude-code-transient
             claude-code-kill)
  :init
  (setq claude-code-terminal-backend 'eat
        claude-code-notification-function #'sw/claude-notify
        claude-code-toggle-auto-select t
        claude-code-display-window-fn #'sw/claude-display-buffer-full-frame)

  ;; Custom toggle that uses full frame display
  ;; Defined in :init so the command exists before the package is loaded
  (defun sw/claude-code-toggle ()
    "Show or hide the Claude window in full frame."
    (interactive)
    (require 'claude-code)
    (let ((claude-code-buffer (claude-code--get-or-prompt-for-buffer)))
      (if claude-code-buffer
          (if (get-buffer-window claude-code-buffer)
              (delete-window (get-buffer-window claude-code-buffer))
            (let ((window (sw/claude-display-buffer-full-frame claude-code-buffer)))
              (set-window-parameter window 'no-delete-other-windows
                                    claude-code-no-delete-other-windows)
              (when claude-code-toggle-auto-select
                (select-window window))))
        (claude-code--show-not-running-message))))

  :config
  (advice-add 'claude-code-toggle :override #'sw/claude-code-toggle))

(provide 'sw-claude)
;;; sw-claude.el ends here
