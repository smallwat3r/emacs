;;; sw-git.el --- Git integration -*- lexical-binding: t -*-

;;; Commentary:
;; Magit and other Git-related packages.

;;; Code:

(require 'cl-lib)

;; Display source line numbers in magit diff buffers, similar
;; to GitHub's side-by-side view. Each hunk body line gets a
;; before-string overlay showing old/new file line numbers.
;;
;; Line type determines which columns are shown:
;;   removed (-): old number only, in magit-diff-removed color
;;   added   (+): new number only, in magit-diff-added color
;;   context ( ): both numbers
;;
;; Column width adapts to the highest line number in each hunk.
;; Overlays are tagged with `sw-diff-lnum' for bulk removal on
;; refresh. Combined/merge diffs are skipped (different format).

(defun sw-magit-diff--annotate-hunk (section)
  "Add line number overlays to SECTION's body lines."
  (with-slots (combined from-range to-range content end) section
    (when (and from-range to-range (not combined))
      (let* ((old (car from-range))
             (new (car to-range))
             ;; Column width from highest reachable line number.
             (w (max (length (number-to-string
                              (+ old (cadr from-range))))
                     (length (number-to-string
                              (+ new (cadr to-range))))))
             (num-fmt (format "%%%dd" w))
             (blank (make-string w ?\s))
             ;; Faces
             (old-face
              (list :foreground
                    (face-foreground
                     'magit-diff-removed nil t)))
             (new-face
              (list :foreground
                    (face-foreground
                     'magit-diff-added nil t))))
        (goto-char content)
        (while (< (point) end)
          ;; Diff prefix char determines which counters apply:
          ;; -/space/\ show old, +/space/\ show new.
          (let* ((ch (char-after))
                 (old-p (memq ch '(?- ?\s ?\\)))
                 (new-p (memq ch '(?+ ?\s ?\\))))
            (when (or old-p new-p)
              (let ((ov (make-overlay (point) (point))))
                (overlay-put ov 'sw-diff-lnum t)
                (overlay-put ov 'before-string
                             (concat
                              (if old-p
                                  (propertize
                                   (format num-fmt old)
                                   'face old-face)
                                blank)
                              " "
                              (if new-p
                                  (propertize
                                   (format num-fmt new)
                                   'face new-face)
                                blank)
                              " ")))
              (when old-p (cl-incf old))
              (when new-p (cl-incf new))))
          (forward-line 1))))))

(defun sw-magit-diff--add-line-numbers ()
  "Add source line number overlays to all hunks in the buffer."
  (remove-overlays (point-min) (point-max) 'sw-diff-lnum t)
  (save-excursion
    (magit-map-sections
     (lambda (section)
       (when (magit-hunk-section-p section)
         (sw-magit-diff--annotate-hunk section))))))

(add-hook 'magit-refresh-buffer-hook
          #'sw-magit-diff--add-line-numbers)

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
  ;; Show deleted lines indicator in margin (more visible than fringe)
  (diff-hl-margin-mode 1)
  ;; Update diff indicators as you type, not just on save
  (diff-hl-flydiff-mode 1)
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
(use-package git-modes
  :defer t)

;; Open remote repo in browser
(use-package browse-at-remote
  :commands (browse-at-remote browse-at-remote-kill))

(provide 'sw-git)
;;; sw-git.el ends here
