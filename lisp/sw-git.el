;;; sw-git.el --- Git integration -*- lexical-binding: t -*-

;;; Commentary:
;; Magit and other Git-related packages.

;;; Code:

;; Custom SSH options for Git commands from Emacs to improve reliability:
;; -4: force IPv4 (avoids IPv6 connectivity issues)
;; ConnectTimeout: fail quickly if host is unreachable
;; ServerAliveInterval/CountMax: detect dropped connections
;; TCPKeepAlive: prevent firewall from closing idle connections
;; GSSAPIAuthentication=no: skip Kerberos (faster for non-Kerberos hosts)
;; ControlMaster=no: avoid conflicts with user's ControlMaster settings
(setenv "GIT_SSH_COMMAND" "ssh -4 \
  -o ConnectTimeout=10 \
  -o ServerAliveInterval=20 \
  -o ServerAliveCountMax=3 \
  -o TCPKeepAlive=yes \
  -o GSSAPIAuthentication=no \
  -o ControlMaster=no")

(use-package magit
  :commands (magit-status magit-dispatch magit-file-dispatch)
  :custom
  ;; Performance
  (magit-refresh-status-buffer nil)
  (magit-diff-refine-hunk 'all)

  ;; Display
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (magit-save-repository-buffers 'dontask)

  ;; Process
  (magit-process-finish-apply-ansi-colors t)

  :config
  ;; Unbind h/l keys for Evil compatibility (allow cursor movement)
  (define-key magit-mode-map (kbd "l") nil)
  (define-key magit-mode-map (kbd "h") nil)

  ;; Commit buffer settings
  (add-hook 'git-commit-mode-hook
            (lambda ()
              (display-line-numbers-mode -1)
              (evil-insert-state)))

  ;; Git commit settings
  (with-eval-after-load 'git-commit
    (setq git-commit-summary-max-length 75))

  ;; Kill all COMMIT_EDITMSG buffers after committing
  (with-eval-after-load 'with-editor
    (add-hook 'with-editor-post-finish-hook
              (lambda ()
                (dolist (buf (buffer-list))
                  (when (string-prefix-p "COMMIT_EDITMSG" (buffer-name buf))
                    (kill-buffer buf))))))

  ;; Git rebase keybindings for moving commits
  ;; n, a bindings match custom keyboard layout (up/down)
  (with-eval-after-load 'git-rebase
    (define-key git-rebase-mode-map (kbd "K") #'git-rebase-move-line-up)
    (define-key git-rebase-mode-map (kbd "J") #'git-rebase-move-line-down)
    (define-key git-rebase-mode-map (kbd "N") #'git-rebase-move-line-up)
    (define-key git-rebase-mode-map (kbd "A") #'git-rebase-move-line-down)))

;; Git gutter
(use-package diff-hl
  :demand t
  :custom
  (diff-hl-draw-borders nil)
  :config
  (global-diff-hl-mode 1)
  (diff-hl-flydiff-mode 1)
  ;; Integration with magit
  (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

;; Git time machine
(use-package git-timemachine
  :commands git-timemachine
  :config
  ;; C-n, C-a navigation matches custom keyboard layout (up/down)
  (evil-define-key 'normal git-timemachine-mode-map
    (kbd "C-n") #'git-timemachine-show-previous-revision
    (kbd "C-a") #'git-timemachine-show-next-revision))

;; Git modes for config files
(use-package git-modes)

;; Show blame inline
(use-package blamer
  :custom
  (blamer-idle-time 0.5)
  (blamer-min-offset 70)
  (blamer-author-formatter " %s ")
  (blamer-datetime-formatter "[%s]")
  (blamer-commit-formatter " - %s")
  :commands blamer-mode)

;; Open remote repo in browser
(use-package browse-at-remote)

(provide 'sw-git)
;;; sw-git.el ends here
