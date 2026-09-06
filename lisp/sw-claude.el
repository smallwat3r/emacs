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

(defun sw-claude-upgrade-sandbox ()
  "Refresh the pinned versions in the Claude Docker sandbox Dockerfile.
Run `sw-claude-rebuild-sandbox' afterwards to build the new image."
  (interactive)
  (let ((default-directory user-emacs-directory))
    (compile "docker/claude-sandbox/update-pins.py")))

(defun sw-claude-rebuild-sandbox ()
  "Force rebuild the Claude Docker sandbox image."
  (interactive)
  (let ((default-directory user-emacs-directory))
    (compile (format "docker build --no-cache --build-arg HOST_HOME=%s \
-t claude-code-sandbox docker/claude-sandbox/"
                     (shell-quote-argument (expand-file-name "~"))))))

(defun sw-claude--prose-line-p (line)
  "Non-nil when LINE looks like flowing prose safe to reflow.
Indented lines (code blocks, diff gutters, wrapped list items) and
lines starting with TUI chrome glyphs keep their line breaks."
  (not (string-match-p "\\`[ \t●⏺⎿│❯✻>+|-]" line)))

(defun sw-claude--reflow (text)
  "Clean Claude TUI artifacts from copied TEXT.
Strip the trailing padding and the two-space left margin, then
join prose lines the TUI hard-wrapped to the terminal width.  The
TUI writes real newlines when wrapping, so wraps are detected by
length: a prose line ending near the longest line in the copy
continues on the next prose line."
  (let* ((text (replace-regexp-in-string "[ \t]+$" "" text))
         (text (replace-regexp-in-string "^  " "" text))
         (lines (split-string text "\n"))
         (maxlen (apply #'max 0 (mapcar #'length lines))))
    ;; Width heuristic, a long final paragraph line followed by
    ;; text joins wrongly; good enough for prose copies.
    (if (< maxlen 60)
        text
      (let (out)
        (while lines
          (let* ((line (pop lines))
                 (seg line))
            (while (and lines
                        (>= (length seg) (- maxlen 15))
                        (sw-claude--prose-line-p seg)
                        (not (string-empty-p (car lines)))
                        (sw-claude--prose-line-p (car lines)))
              (setq seg (pop lines))
              (setq line (concat line " " seg)))
            (push line out)))
        (mapconcat #'identity (nreverse out) "\n")))))

(defun sw-claude--filter-substring (beg end &optional delete)
  "Like `buffer-substring--filter' but clean up Claude TUI output.
Claude's TUI pads each line to terminal width, indents everything
by two spaces, and hard-wraps prose, so plain copies pick up
padding, margins and bogus line breaks."
  (let ((text (buffer-substring--filter beg end delete)))
    (if (stringp text)
        (sw-claude--reflow text)
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

;; Same trick as bin/claude-docker: keep Claude's TUI off the
;; alternate screen so eat scrollback works when running `claude'
;; from a plain eat shell.  Inherited by every Emacs subprocess,
;; harmless outside Claude.
(setenv "CLAUDE_CODE_DISABLE_ALTERNATE_SCREEN" "1")

(defvar eat-terminal)
(declare-function eat-term-end "eat" (terminal))

(defun sw-claude--scrolled-back-p (win)
  "Non-nil if WIN is scrolled away from the end of its terminal."
  (and (window-live-p win)
       (bound-and-true-p eat-terminal)
       (not (pos-visible-in-window-p
             (eat-term-end eat-terminal) win t))))

(defun sw-claude--keep-scroll (orig-fn buffer)
  "Around advice for `eat--process-output-queue' on BUFFER.
ORIG-FN is the original function.  Claude repaints its TUI
constantly, and eat writes output at point then recenters the
window on the terminal cursor, so a scrolled-back window is
dragged to the bottom within milliseconds.  While the window is
scrolled back, put point and the window start back where the user
left them.  Typing resumes following, eat forces a scroll sync
when it sends input."
  (if (not (and (buffer-live-p buffer)
                (string-prefix-p "*claude:" (buffer-name buffer))))
      (funcall orig-fn buffer)
    (with-current-buffer buffer
      (let* ((win (get-buffer-window buffer))
             (frozen (sw-claude--scrolled-back-p win))
             (pt (and frozen (point-marker)))
             (start (and frozen (copy-marker (window-start win)))))
        (unwind-protect
            (funcall orig-fn buffer)
          (when frozen
            (when (window-live-p win)
              (set-window-start win start t)
              (set-window-point win pt))
            (goto-char pt)
            (set-marker pt nil)
            (set-marker start nil)))))))

(with-eval-after-load 'eat
  (advice-add 'eat--process-output-queue
              :around #'sw-claude--keep-scroll))

(defconst sw-claude-commit-prompt
  "Review what changed in this repo: run git status and look at the \
staged and unstaged diffs (and untracked files). Stage what is needed and \
build commits, splitting unrelated changes into separate logical commits, \
each with a clear one-line message and, when useful, a short description \
body. If a change logically belongs to an existing commit that has not \
been pushed yet (check with git log @{upstream}.., or treat everything as \
unpushed when there is no upstream), fold it into that commit with an \
amend or a fixup and autosquash rebase, instead of creating a new one. \
Never rewrite pushed commits. Do not push, I'll review and do it manually."
  "Prompt sent to the ephemeral Claude session by `sw-claude-commit'.")

(defvar sw-claude-ephemeral-log-file
  (expand-file-name "~/.local/state/claude/ephemeral.log")
  "File where every `sw-claude-ephemeral' run is appended.")

(defun sw-claude-ephemeral (name prompt &optional model on-success)
  "Run PROMPT in an ephemeral headless sandboxed Claude session.
Launches a one-shot claude -p in a fresh Docker container that is
removed when it exits.  Output goes silently into a *claude-NAME*
buffer (holding the last run) and is appended on exit to
`sw-claude-ephemeral-log-file'.  Notifies when the run finishes.
MODEL, when non-nil, is passed to claude --model (an alias like
\"sonnet\" or a full model ID); otherwise the default model is used.
ON-SUCCESS, when non-nil, is called with no arguments after a
successful run, with `default-directory' set to the run directory."
  (let* ((default-directory
          (or (locate-dominating-file default-directory ".git")
              default-directory))
         (dir default-directory)
         (bufname (format "*claude-%s*" name))
         (buffer (get-buffer-create bufname))
         ;; Unique instance name so the container never collides with an
         ;; interactive session's claude-{project} container
         (process-environment
          (cons (format "CLAUDE_BUFFER_NAME=*claude:%s-%x*"
                        name (random #x10000))
                process-environment)))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (format "=== %s | %s | %s\n"
                      (format-time-string "%F %T") name default-directory)))
    (make-process
     :name (format "claude-%s" name)
     :buffer buffer
     ;; The wrapper runs docker with -it, which needs pty stdin
     :connection-type 'pty
     :command (append (list sw-claude-docker-script
                            "--dangerously-skip-permissions")
                      (when model (list "--model" model))
                      (list "-p" prompt))
     :sentinel (lambda (proc _event)
                 (when (memq (process-status proc) '(exit signal))
                   (let ((ok (zerop (process-exit-status proc))))
                     (when (buffer-live-p buffer)
                       (with-current-buffer buffer
                         (goto-char (point-max))
                         (insert (format "=== exit %d\n\n"
                                         (process-exit-status proc)))
                         (write-region (point-min) (point-max)
                                       sw-claude-ephemeral-log-file
                                       t 'silent)))
                     (sw-claude-notify
                      (format "Claude %s" name)
                      (if ok "Done" (format "Failed, see %s" bufname)))
                     (message "claude-%s %s, output in %s"
                              name (if ok "done" "failed") bufname)
                     (when (and ok on-success)
                       (let ((default-directory dir))
                         (funcall on-success)))))))
    (message "claude-%s running in the background..." name)))

(defun sw-claude--resign-commits ()
  "Re-sign the unpushed commits on the host, where gpg lives.
The sandbox has no gpg, so its commits are made unsigned (see
bin/claude-docker); rewrite everything since upstream with real
signatures.  Skipped when the branch has no upstream, everything
would be unpushed and rewriting from the root is not worth it.

The rebase runs asynchronously on purpose: gpg prompts through the
Emacs pinentry server, which can only answer while the Emacs event
loop is free, a synchronous call would deadlock until gpg gives up."
  (if (zerop (call-process "git" nil nil nil
                           "rev-parse" "--verify" "-q" "@{upstream}"))
      (progn
        (require 'pinentry)
        (pinentry-start 'quiet)
        (make-process
         :name "claude-resign"
         :buffer (generate-new-buffer " *claude-resign*")
         :command '("git" "rebase"
                    "--exec" "git commit --amend --no-edit -n -S"
                    "@{upstream}")
         :sentinel
         (lambda (proc _event)
           (unless (process-live-p proc)
             (if (zerop (process-exit-status proc))
                 (message "claude-commit done, commits signed")
               (message "claude-commit: signing rebase failed: %s"
                        (with-current-buffer (process-buffer proc)
                          (string-trim (buffer-string)))))
             (kill-buffer (process-buffer proc))))))
    (message "claude-commit done, no upstream so commits left unsigned")))

(defun sw-claude-commit ()
  "Stage and commit the repo changes with an ephemeral sandboxed Claude.
Claude groups the changes into logical commits, never pushes and never
credits itself.  Once done, the new commits are re-signed on the host."
  (interactive)
  (unless (locate-dominating-file default-directory ".git")
    (user-error "Not in a git repository"))
  (sw-claude-ephemeral "commit" sw-claude-commit-prompt "sonnet"
                       #'sw-claude--resign-commits))

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
