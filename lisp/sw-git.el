;;; sw-git.el --- Git integration -*- lexical-binding: t -*-

;;; Commentary:
;; Magit and other Git-related packages.

;;; Code:

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

  ;; Ensure commit buffer is focused in daemon mode
  (add-hook 'server-switch-hook #'raise-frame)

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
  :hook (sw-first-file . global-diff-hl-mode)
  :custom
  (diff-hl-draw-borders nil)
  (diff-hl-show-staged-changes nil)
  :config
  (diff-hl-flydiff-mode 1)
  ;; Show deleted lines indicator in margin (more visible than fringe)
  (diff-hl-margin-mode 1)
  ;; Integration with magit
  (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

;; Git time machine
(use-package git-timemachine
  :commands git-timemachine
  :config
  ;; Rehash evil keybindings so they are recognized
  (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps))

;; Git modes for config files
(use-package git-modes)

;; Open remote repo in browser
(use-package browse-at-remote)

(provide 'sw-git)
;;; sw-git.el ends here
