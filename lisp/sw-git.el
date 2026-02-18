;;; sw-git.el --- Git integration -*- lexical-binding: t -*-

;;; Commentary:
;; Magit and other Git-related packages.

;;; Code:

(require 'cl-lib)

(defun sw-magit-diff--hunk-max-line (section)
  "Return the highest line number reachable in SECTION."
  (with-slots (combined from-range to-range) section
    (if (or combined (not from-range) (not to-range)) 0
      (max (+ (car from-range) (cadr from-range))
           (+ (car to-range) (cadr to-range))))))

(defun sw-magit-diff--annotate-hunk (section fmt blank old-face new-face)
  "Add line number overlays to SECTION body lines.
FMT is the number format, BLANK the empty placeholder,
OLD-FACE and NEW-FACE the face plists."
  ;; Guard is in the caller via `sw-magit-diff--hunk-max-line'.
  (with-slots (from-range to-range content end) section
    (let ((old (car from-range)) (new (car to-range)))
      (goto-char content)
      (while (< (point) end)
        ;; Diff prefix determines which counters apply:
        ;; -/space/\ show old, +/space/\ show new.
        (let* ((ch (char-after))
               (old-p (memq ch '(?- ?\s ?\\)))
               (new-p (memq ch '(?+ ?\s ?\\))))
          (when (or old-p new-p)
            (let* ((str
                    (concat
                     (if old-p (propertize (format fmt old) 'face old-face)
                       blank)
                     " "
                     (if new-p (propertize (format fmt new) 'face new-face)
                       blank)
                     " "))
                   (ov (make-overlay (1+ (point)) (1+ (point)))))
              (overlay-put ov 'sw-diff-lnum t)
              (overlay-put ov 'before-string str))
            (when old-p (cl-incf old))
            (when new-p (cl-incf new))))
        (forward-line 1)))))

(defun sw-magit-diff--add-line-numbers ()
  "Add line number overlays to all hunks."
  (remove-overlays (point-min) (point-max) 'sw-diff-lnum t)
  (when (derived-mode-p 'magit-diff-mode 'magit-status-mode)
    (let ((max-line 0) (hunks nil))
      ;; Single pass: collect hunks and find max line number.
      (magit-map-sections
       (lambda (section)
         (when (magit-hunk-section-p section)
           (let ((ml (sw-magit-diff--hunk-max-line section)))
             (when (> ml 0)
               (push section hunks)
               (setq max-line (max max-line ml)))))))
      (when hunks
        ;; Compute shared values once for all hunks.
        (let* ((w (length (number-to-string max-line)))
               (fmt (format "%%%dd" w))
               (blank (make-string w ?\s))
               (old-face
                (list :foreground (face-foreground 'magit-diff-removed nil t)))
               (new-face
                (list :foreground (face-foreground 'magit-diff-added nil t))))
          (save-excursion
            (dolist (section (nreverse hunks))
              (sw-magit-diff--annotate-hunk
               section fmt blank
               old-face new-face))))))))

;; Display source line numbers next to magit diff hunks,
;; similar to GitHub's diff view.
;;
;; Line type determines which columns are shown:
;;   removed (-): old number only, in magit-diff-removed color
;;   added   (+): new number only, in magit-diff-added color
;;   context ( ): both numbers
;;
;; Column width is uniform across all hunks. Overlays are
;; tagged with `sw-diff-lnum' for bulk removal on refresh.
;; Combined/merge diffs are skipped.
(add-hook 'magit-refresh-buffer-hook #'sw-magit-diff--add-line-numbers)

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
  (add-to-list 'magit-git-environment
               (concat "GIT_SSH_COMMAND="
                       "ssh -4"
                       " -o ConnectTimeout=10"
                       " -o ServerAliveInterval=20"
                       " -o ServerAliveCountMax=3"
                       " -o TCPKeepAlive=yes"
                       " -o GSSAPIAuthentication=no"
                       " -o ControlMaster=no"))

  ;; Commit buffer settings
  (add-hook 'git-commit-mode-hook
            (lambda ()
              (display-line-numbers-mode -1)
              (evil-insert-state)))

  ;; Ensure commit buffer is focused in daemon mode
  (add-hook 'server-switch-hook #'raise-frame))

(use-package git-commit
  :ensure nil
  :custom
  (git-commit-summary-max-length 75))

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
(use-package git-modes)

;; Open remote repo in browser
(use-package browse-at-remote
  :commands (browse-at-remote browse-at-remote-kill))

(provide 'sw-git)
;;; sw-git.el ends here
