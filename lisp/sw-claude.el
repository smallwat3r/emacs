;;; sw-claude.el --- Claude Code integration -*- lexical-binding: t -*-

;;; Commentary:
;; Integration with claude-code CLI for AI-assisted coding.

;;; Code:

(defun sw/claude-notify (title message)
  "Display a Linux notification with TITLE and MESSAGE using notify-send."
  (when (and sw/is-linux (executable-find "notify-send"))
    (call-process "notify-send" nil nil nil title message)))

(defun sw/claude-display-buffer-full-frame (buffer)
  "Display claude-code BUFFER.
If only one window exists, use full frame.
If in a split view, display in the current window."
  (display-buffer buffer
                  (if (one-window-p)
                      '((display-buffer-full-frame))
                    '((display-buffer-same-window)))))

;; Required dependency for claude-code
(use-package inheritenv
  :ensure (:host github :repo "purcell/inheritenv" :wait t)
  :demand t)

;; Claude Code
(use-package claude-code
  :ensure (:host github :repo "stevemolitor/claude-code.el" :wait t)
  :when (executable-find "claude")
  :after inheritenv
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
