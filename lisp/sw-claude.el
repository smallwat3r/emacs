;;; sw-claude.el --- Claude Code integration -*- lexical-binding: t -*-

;;; Commentary:
;; Integration with claude-code CLI for AI-assisted coding.
;; Claude runs inside a Docker sandbox via the claude-docker launcher
;; (from the claude-sandbox repository), which mounts the project
;; directory, shares the login and the git config, and keeps a record
;; of what each session did.  On top of the sessions themselves this
;; covers the launcher's own features: opening doors for one session
;; (egress filter, SSH agent, Docker socket), a shell in the sandbox,
;; the trace of a session and following one live, the upkeep of the
;; image, and ending a session so that the launcher's checks run.

;;; Code:

(require 'sw-lib)
(require 'seq)
(require 'ansi-color)
(require 'transient)

(defcustom sw-claude-docker-script
  (or (executable-find "claude-docker")
      (expand-file-name "~/code/claude-sandbox/bin/claude-docker"))
  "Path to the Docker wrapper script for sandboxed Claude.
It lives in the claude-sandbox repository, found on `exec-path'
or at its usual checkout."
  :type 'file
  :group 'tools)

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

;;; The launcher

(defun sw-claude--sandbox-root ()
  "Return the claude-sandbox checkout the launcher belongs to."
  (file-name-directory
   (directory-file-name
    (file-name-directory (file-truename sw-claude-docker-script)))))

(defun sw-claude--project-dir ()
  "Return the directory a session of the current buffer runs in."
  (expand-file-name
   (or (locate-dominating-file default-directory ".git")
       default-directory)))

(defun sw-claude--launcher-compile (&rest args)
  "Run the launcher with ARGS in a compilation buffer."
  (compile (mapconcat #'shell-quote-argument
                      (cons sw-claude-docker-script args) " ")))

(defvar-local sw-claude--output-args nil
  "Launcher arguments that produced the output of this buffer.")

(defvar-local sw-claude--output-follow nil
  "Non-nil when this buffer follows output that keeps coming.")

(defun sw-claude--output-revert (&rest _)
  "Run the launcher command of the current buffer again."
  (sw-claude--launcher-buffer (buffer-name)
                              sw-claude--output-args
                              sw-claude--output-follow))

(define-derived-mode sw-claude-output-mode special-mode "Claude-Sandbox"
  "Major mode for the output of the claude-docker launcher.
\\[revert-buffer] runs the command again, \\[quit-window] leaves."
  (setq truncate-lines t)
  (setq-local ansi-color-context-region nil)
  (setq-local revert-buffer-function #'sw-claude--output-revert))

(defun sw-claude--output-filter (proc string)
  "Append STRING, the output of PROC, to its buffer with its colours.
A window that sits at the end of a followed buffer stays there."
  (let ((buffer (process-buffer proc)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let* ((inhibit-read-only t)
               (start (point-max))
               (following
                (and sw-claude--output-follow
                     (seq-filter (lambda (win) (= (window-point win) start))
                                 (get-buffer-window-list buffer nil t)))))
          (save-excursion
            (goto-char start)
            (insert string)
            (ansi-color-apply-on-region start (point-max)))
          (dolist (win following)
            (set-window-point win (point-max))))))))

(defun sw-claude--launcher-buffer (name args &optional follow)
  "Run the launcher with ARGS for the current project, output in buffer NAME.
The output is not a terminal's, so the launcher writes plain lines
that each say when, who and what, without its pager.  FOLLOW
non-nil is for output that keeps coming: windows at the end of the
buffer scroll along."
  (let ((dir (sw-claude--project-dir))
        (buffer (get-buffer-create name))
        ;; Colours although the output is a pipe, and no buffering on
        ;; the way, a followed session shows as it happens
        (process-environment
         (append '("FORCE_COLOR=1" "PYTHONUNBUFFERED=1")
                 process-environment)))
    (when-let* ((running (get-buffer-process buffer)))
      (delete-process running))
    (with-current-buffer buffer
      (sw-claude-output-mode)
      (let ((inhibit-read-only t))
        (erase-buffer))
      (setq default-directory dir
            sw-claude--output-args args
            sw-claude--output-follow follow)
      (make-process
       :name "claude-docker"
       :buffer buffer
       :command (cons sw-claude-docker-script args)
       :connection-type 'pipe
       :noquery t
       :filter #'sw-claude--output-filter
       :sentinel #'ignore))
    (pop-to-buffer buffer)))

;;; The image and the egress filter

(defun sw-claude-upgrade-sandbox ()
  "Refresh the pinned versions of the Claude Docker sandbox image.
Run `sw-claude-rebuild-sandbox' afterwards to build the new image."
  (interactive)
  (compile (shell-quote-argument
            (expand-file-name "docker/update-pins.py"
                              (sw-claude--sandbox-root)))))

(defun sw-claude-rebuild-sandbox ()
  "Force rebuild the Claude Docker sandbox image."
  (interactive)
  (sw-claude--launcher-compile "--rebuild"))

(defun sw-claude-prune-sandbox ()
  "Delete every Claude Docker sandbox image but the current one."
  (interactive)
  (when (y-or-n-p "Delete all sandbox images but the current one? ")
    (sw-claude--launcher-compile "--prune")))

(defun sw-claude-dockerfile ()
  "Show the Dockerfile the sandbox image is built from."
  (interactive)
  (sw-claude--launcher-buffer "*claude-dockerfile*" '("--dockerfile")))

(defun sw-claude-net-log ()
  "Show the domains the egress filter refused, most frequent first."
  (interactive)
  (sw-claude--launcher-buffer "*claude-net-log*" '("--net-log")))

(defun sw-claude-net-reload ()
  "Restart the egress filter, so that it reads its allowlist again.
The connections of the filtered sessions drop for a moment."
  (interactive)
  (sw-run-async (list sw-claude-docker-script "--net-reload")))

(defun sw-claude-edit-allowed-domains ()
  "Visit the allowlist of the egress filter.
`sw-claude-net-reload' applies it to the sessions that run."
  (interactive)
  (find-file (expand-file-name "docker/allowed-domains.txt"
                               (sw-claude--sandbox-root))))

;;; Reviewing a session

(defun sw-claude--read-session ()
  "Read a session of the current project, return the start of its id."
  (let* ((default-directory (sw-claude--project-dir))
         (sessions
          (condition-case nil
              (process-lines sw-claude-docker-script "--trace" "list")
            (error (user-error "No session recorded for %s"
                               default-directory))))
         ;; The launcher lists the latest first, keep its order
         (table (lambda (string pred action)
                  (if (eq action 'metadata)
                      '(metadata (display-sort-function . identity)
                                 (cycle-sort-function . identity))
                    (complete-with-action action sessions string pred))))
         (choice (completing-read "Session: " table nil t)))
    ;; A line is when (16 columns), the start of the id, then the rest
    (if (string-match "\\`.\\{16\\}  \\([^ ]+\\)" choice)
        (match-string 1 choice)
      (user-error "No session id in: %s" choice))))

(defun sw-claude-trace (&optional args)
  "Show what the latest Claude session of this project did.
ARGS are the options of `sw-claude-trace-menu'."
  (interactive (list (transient-args 'sw-claude-trace-menu)))
  (sw-claude--launcher-buffer "*claude-trace*" (cons "--trace" args)))

(defun sw-claude-trace-session (session &optional args)
  "Show what SESSION, a Claude session of this project, did.
SESSION is the start of its id.  ARGS are the options of
`sw-claude-trace-menu'."
  (interactive (list (sw-claude--read-session)
                     (transient-args 'sw-claude-trace-menu)))
  (sw-claude--launcher-buffer "*claude-trace*"
                              (append (list "--trace" session) args)))

(defun sw-claude-trace-follow (&optional args)
  "Show what the Claude sessions of this project do, as they do it.
Needs nothing from the sessions, so it can start before them and
stop at any time: kill the buffer.  ARGS are the options of
`sw-claude-trace-menu'."
  (interactive (list (transient-args 'sw-claude-trace-menu)))
  (sw-claude--launcher-buffer "*claude-follow*"
                              (append '("--trace" "--follow") args)
                              t))

(transient-define-prefix sw-claude-trace-menu ()
  "Review what the sandboxed Claude sessions of this project did."
  ["Show"
   ("-v" "Commands, diffs and output whole" "--verbose")
   ("-f" "Only what went wrong" "--failed")
   ("-o" "Only these kinds" "--only="
    :prompt "Kinds, comma separated (bash,net,edit,dns...): ")]
  ["Trace"
   ("t" "Latest session" sw-claude-trace)
   ("s" "Pick a session" sw-claude-trace-session)
   ("f" "Follow live" sw-claude-trace-follow)])

;;; Ending a session

;; Killing the buffer of a session, or its process, takes the launcher
;; down with it: the check of what the session left behind and the
;; records of its sidecars are then left to the next launch, and what
;; the launcher has to say goes nowhere.  Told to end with SIGTERM it
;; stops the container and ends as after any session, so that is how
;; a session is ended here, and what it reports is kept.

(defconst sw-claude--report-lines 60
  "How many lines from the end of a session buffer may hold its report.")

(defvar-local sw-claude--ending nil
  "Non-nil once the session of this buffer was told to end.")

(defvar-local sw-claude--kill-on-exit nil
  "Non-nil when this buffer is to be killed once its launcher is done.")

(defun sw-claude--launcher-report ()
  "Return what the launcher said at the end of the current buffer, or nil.
Its lines start with its name in the first column, where the TUI,
which indents what it shows, has nothing."
  (save-excursion
    (goto-char (point-max))
    (forward-line (- sw-claude--report-lines))
    (when (re-search-forward "^claude-docker: " nil t)
      (let ((text (buffer-substring-no-properties (match-beginning 0)
                                                  (point-max))))
        (string-trim
         (replace-regexp-in-string
          "\nProcess [^\n]*\n?\\'" ""
          (replace-regexp-in-string "\r" "" text)))))))

(defun sw-claude--show-report (session report)
  "Show REPORT, what the launcher said when SESSION ended."
  (let ((buffer (get-buffer-create "*claude-sandbox*")))
    (with-current-buffer buffer
      (unless (derived-mode-p 'special-mode)
        (special-mode))
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (format "=== %s | %s\n%s\n\n"
                        (format-time-string "%F %T") session report))))
    (display-buffer buffer)
    (sw-claude-notify "Claude sandbox"
                      (format "%s left something to review" session))))

(defun sw-claude--end-session (buffer)
  "Tell the launcher of BUFFER to end its session.
Return non-nil when it was told, BUFFER is then killed once the
launcher is done.  Return nil when no launcher runs there, or when
it was told before: the caller is free to kill BUFFER outright,
which is how a session that does not end is forced."
  (let ((proc (get-buffer-process buffer)))
    (when (and (process-live-p proc)
               (not (buffer-local-value 'sw-claude--ending buffer)))
      (with-current-buffer buffer
        (setq sw-claude--ending t
              sw-claude--kill-on-exit t))
      (signal-process proc 'SIGTERM)
      (message "Ending %s, the sandbox checks what it left behind..."
               (buffer-name buffer))
      t)))

(defun sw-claude--kill-buffer-query ()
  "Keep the current buffer until the launcher of its session is done.
For `kill-buffer-query-functions'."
  (not (sw-claude--end-session (current-buffer))))

(declare-function claude-code--kill-buffer "claude-code" (buffer))

(defun sw-claude--session-ended (_proc)
  "Keep what the launcher said when the session of this buffer ended.
For `eat-exit-hook'."
  (let ((buffer (current-buffer))
        (name (buffer-name))
        (report (sw-claude--launcher-report)))
    (cond (report (sw-claude--show-report name report))
          (sw-claude--ending
           (message "%s ended, the sandbox has nothing to report" name)))
    (when sw-claude--kill-on-exit
      (if (and (string-prefix-p "*claude:" name)
               (fboundp 'claude-code--kill-buffer))
          (claude-code--kill-buffer buffer)
        (kill-buffer buffer)))))

(defun sw-claude--watch-session ()
  "End the session of the current eat buffer through its launcher."
  (add-hook 'eat-exit-hook #'sw-claude--session-ended 90 t)
  (add-hook 'kill-buffer-query-functions
            #'sw-claude--kill-buffer-query nil t))

(defun sw-claude--kill-buffer-advice (orig-fn buffer)
  "Around advice for `claude-code--kill-buffer' on BUFFER.
ORIG-FN deletes the process, which kills the launcher on the spot.
End the session through the launcher instead, and leave ORIG-FN
the buffers that have none to end."
  (unless (sw-claude--end-session buffer)
    (funcall orig-fn buffer)))

;;; Sessions

(defvar eat-kill-buffer-on-exit)
(declare-function eat-make "eat"
                  (name program &optional startfile &rest switches))

(defun sw-claude-shell ()
  "Open bash in the sandbox of the current project.
The same container a Claude session gets, to look around in it."
  (interactive)
  (require 'eat)
  (let* ((default-directory (sw-claude--project-dir))
         (name (format "claude-shell:%s"
                       (abbreviate-file-name
                        (directory-file-name default-directory))))
         ;; `sw-claude--session-ended' kills the buffer instead, after
         ;; it took what the launcher said
         (eat-kill-buffer-on-exit nil)
         (buffer (get-buffer-create (format "*%s*" name)))
         (fresh (not (get-buffer-process buffer))))
    ;; Shown before the shell starts, eat sizes the terminal from the
    ;; window of its buffer and starts at 80x24 without one
    (pop-to-buffer buffer)
    (eat-make name sw-claude-docker-script nil "--shell")
    (when fresh
      (with-current-buffer buffer
        (sw-claude--watch-session)
        (setq sw-claude--kill-on-exit t)))))

(defconst sw-claude-session-options
  '(("extra-dirs" . dirs)
    ("extra-dirs-read-only" . dirs-ro)
    ("worktree" . worktree)
    ("net-filter" . "CLAUDE_DOCKER_NET_FILTER=1")
    ("ssh-agent" . "CLAUDE_DOCKER_SSH_AGENT=1")
    ;; Ahead of the plain socket: of two settings of one variable the
    ;; first counts, and full includes the other
    ("docker-socket-full" . "CLAUDE_DOCKER_SOCKET=full")
    ("docker-socket" . "CLAUDE_DOCKER_SOCKET=1")
    ("writable-config" . "CLAUDE_DOCKER_RW_CONFIG=1")
    ("shared-history" . "CLAUDE_DOCKER_SHARED_HISTORY=1")
    ("telemetry" . "CLAUDE_DOCKER_TELEMETRY=1")
    ("no-net-log" . "CLAUDE_DOCKER_NET_LOG=0")
    ("no-proc-log" . "CLAUDE_DOCKER_PROC_LOG=0")
    ("no-cache" . "CLAUDE_DOCKER_CACHE=0"))
  "What `sw-claude-with-options' offers for one session.
Each name comes with the setting of the launcher it stands for, or
with a symbol for what is asked for next.")

(defun sw-claude--read-dirs (prompt)
  "Read directories with PROMPT until none is added, colon separated."
  (let ((dirs (list (read-directory-name prompt))))
    (while (y-or-n-p "Add another directory? ")
      (push (read-directory-name prompt) dirs))
    (mapconcat (lambda (dir)
                 (directory-file-name (expand-file-name dir)))
               (nreverse dirs) ":")))

(declare-function claude-code--start "claude-code"
                  (arg extra-switches
                       &optional force-prompt force-switch-to-buffer))

(defun sw-claude-with-options (options)
  "Start Claude with OPTIONS, names of `sw-claude-session-options'.
They hold for this one session: directories mounted next to the
project, a git worktree of its own, and the doors of the sandbox
that are closed unless asked for.  What every session should get
belongs in ~/.config/claude-sandbox/config instead."
  (interactive
   (list (completing-read-multiple
          "Session options (comma separated): "
          sw-claude-session-options nil t)))
  (require 'claude-code)
  (let (env switches)
    (dolist (option sw-claude-session-options)
      (when (member (car option) options)
        (pcase (cdr option)
          ('dirs
           (push (concat "CLAUDE_DOCKER_EXTRA_DIRS="
                         (sw-claude--read-dirs "Extra directory: "))
                 env))
          ('dirs-ro
           (push (concat "CLAUDE_DOCKER_EXTRA_DIRS_RO="
                         (sw-claude--read-dirs "Read-only directory: "))
                 env))
          ('worktree
           (let ((name (read-string "Worktree name (empty for any): ")))
             (setq switches
                   (if (string-empty-p name)
                       '("--worktree")
                     (list "--worktree" name)))))
          (setting (push setting env)))))
    (let ((process-environment
           (append (nreverse env) process-environment)))
      (claude-code--start nil switches))))

(transient-define-prefix sw-claude-sandbox-menu ()
  "The Claude sandbox: its sessions, its image and its egress filter."
  [["Sessions"
    ("o" "Start with options" sw-claude-with-options)
    ("s" "Shell in the sandbox" sw-claude-shell)
    ("t" "Trace" sw-claude-trace-menu)
    ("f" "Follow live" sw-claude-trace-follow)]
   ["Image"
    ("u" "Refresh the pins" sw-claude-upgrade-sandbox)
    ("r" "Rebuild" sw-claude-rebuild-sandbox)
    ("p" "Prune old images" sw-claude-prune-sandbox)
    ("d" "Show the Dockerfile" sw-claude-dockerfile)]
   ["Egress filter"
    ("n" "Refused domains" sw-claude-net-log)
    ("a" "Edit the allowlist" sw-claude-edit-allowed-domains)
    ("R" "Reload" sw-claude-net-reload)]])

;;; Copying from the TUI

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

;; Same trick as claude-docker: keep Claude's TUI off the
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
  (let* ((default-directory (sw-claude--project-dir))
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
     ;; A pty, the launcher then gives the container a TTY as in an
     ;; interactive session
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
                         (when-let* ((report (sw-claude--launcher-report)))
                           (sw-claude--show-report bufname report))
                         (goto-char (point-max))
                         (insert (format "=== exit %d\n\n"
                                         (process-exit-status proc)))
                         (make-directory
                          (file-name-directory sw-claude-ephemeral-log-file) t)
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
claude-docker); rewrite everything since upstream with real
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
        ;; git-sign-unpushed (dotfiles) aborts the rebase when signing
        ;; fails, so a missed passphrase prompt never leaves HEAD detached
        (sw-run-async '("git" "sign-unpushed")
                      (lambda ()
                        (message "claude-commit done, commits signed"))))
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
  :ensure (:host github :repo "purcell/inheritenv")
  :demand t)

;; Claude Code (sandboxed via Docker)
(use-package claude-code
  :ensure (:host github :repo "stevemolitor/claude-code.el")
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
  (add-hook 'claude-code-start-hook #'sw-claude--watch-session)
  (advice-add 'claude-code--kill-buffer
              :around #'sw-claude--kill-buffer-advice)

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
