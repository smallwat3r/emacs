;;; sw-terminal.el --- Terminal and TRAMP configuration -*- lexical-binding: t -*-

;;; Commentary:
;; eat terminal emulator, TRAMP remote editing, and SSH helpers.

;;; Code:

;;; SSH config files

(defvar sw/ssh-config-files
  '("~/.ssh/config"
    "~/.ssh/work"
    "~/.ssh/private")
  "List of user SSH config files used for TRAMP and SSH helpers.")

(add-to-list 'auto-mode-alist
             '("/\\.ssh/\\(?:work\\|private\\)\\'" . ssh-config-mode))

;;; SSH helpers

(defun sw/ssh-config-hosts ()
  "Return a list of SSH host aliases from the files in `sw/ssh-config-files'."
  (let ((hosts '()))
    (dolist (file sw/ssh-config-files)
      (setq file (expand-file-name file))
      (when (file-readable-p file)
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (while (re-search-forward "^[Hh]ost[ \t]+\\(.+\\)$" nil t)
            (let ((raw (match-string 1)))
              (dolist (h (split-string raw "[ \t]+" t))
                (unless (string-match-p "[*?]" h)
                  (push h hosts))))))))
    (delete-dups hosts)))

(defun sw/zsh-history-candidates (&optional limit)
  "Return recent unique zsh history lines (most recent first).
LIMIT defaults to 10000."
  (let* ((histfile (expand-file-name (or (getenv "HISTFILE") "~/.zsh_history")))
         (limit (or limit 10000))
         (cmd (format
               "H=%s; [ -r \"$H\" ] || exit 0; \
(tac -- \"$H\" 2>/dev/null || tail -r -- \"$H\") \
| awk -F';' '{sub(/^: [0-9]+:[0-9]+;/, \"\"); if (length($0) && !seen[$0]++) print}' \
| head -n %d"
               (shell-quote-argument histfile) limit)))
    (split-string (shell-command-to-string cmd) "\n" t)))

;;;###autoload
(defun sw/tramp-connect ()
  "Open remote SSH connection with TRAMP."
  (interactive)
  (find-file (read-file-name "SSH target: " "/ssh:")))

;;; Eat - Emulate A Terminal

(use-package eat
  :commands (eat eat-project eat-other-window)
  :custom
  (eat-kill-buffer-on-exit t)
  (eat-enable-mouse t)
  (eat-enable-shell-integration nil)
  (eat-enable-shell-prompt-annotation nil)
  (eat-tramp-shells '(("ssh" . "/bin/bash")
                      ("scp" . "/bin/bash")
                      ("sshx" . "/bin/bash")
                      ("docker" . "/bin/sh")))
  :config
  ;; Terminal colors optimized for light backgrounds
  (dolist (spec '((eat-term-color-0 . "#000000")
                  (eat-term-color-1 . "#aa0000")
                  (eat-term-color-2 . "#00aa00")
                  (eat-term-color-3 . "#aa5500")
                  (eat-term-color-4 . "#0000aa")
                  (eat-term-color-5 . "#aa00aa")
                  (eat-term-color-6 . "#00aaaa")
                  (eat-term-color-7 . "#555555")
                  (eat-term-color-8 . "#444444")
                  (eat-term-color-9 . "#cc0000")
                  (eat-term-color-10 . "#00cc00")
                  (eat-term-color-11 . "#cc7700")
                  (eat-term-color-12 . "#0000cc")
                  (eat-term-color-13 . "#cc00cc")
                  (eat-term-color-14 . "#00cccc")
                  (eat-term-color-15 . "#333333")))
    (set-face-attribute (car spec) nil :foreground (cdr spec)))

  ;; Evil integration: switch eat modes based on evil state
  (defun sw/eat-evil-insert-enter ()
    "Switch to semi-char mode when entering insert state in eat."
    (when (and (derived-mode-p 'eat-mode)
               (boundp 'eat--input-mode)
               (not (eq eat--input-mode 'semi-char)))
      (eat-semi-char-mode)))

  (defun sw/eat-evil-insert-exit ()
    "Switch to emacs mode when exiting insert state in eat."
    (when (and (derived-mode-p 'eat-mode)
               (boundp 'eat--input-mode)
               (not (eq eat--input-mode 'emacs)))
      (eat-emacs-mode)))

  (add-hook 'evil-insert-state-entry-hook #'sw/eat-evil-insert-enter)
  (add-hook 'evil-insert-state-exit-hook #'sw/eat-evil-insert-exit)

  ;; Start in insert state (semi-char mode) when opening eat
  (add-hook 'eat-mode-hook #'evil-insert-state)

  ;; Don't ask about running process when killing eat buffer
  (add-hook 'eat-exec-hook
            (lambda (proc)
              (set-process-query-on-exit-flag proc nil)))

  ;; Yank from kill ring into terminal
  (defun sw/eat-yank ()
    "Yank the last killed text into eat terminal."
    (interactive)
    (when eat-terminal
      (eat-term-send-string eat-terminal (current-kill 0))))

  (defun sw/eat-backward-kill-word ()
    "Send backward-kill-word (M-backspace / ESC DEL) to terminal."
    (interactive)
    (when eat-terminal
      (eat-term-send-string eat-terminal "\e\C-?")))

  (defun sw/eat-interrupt ()
    "Send interrupt (C-c) to the eat terminal."
    (interactive)
    (when (bound-and-true-p eat-terminal)
      (eat-term-send-string eat-terminal "\C-c")))

  ;; Keybindings for semi-char mode
  (define-key eat-semi-char-mode-map (kbd "<escape>") #'evil-normal-state)
  (define-key eat-semi-char-mode-map (kbd "C-<backspace>") #'sw/eat-backward-kill-word)
  (define-key eat-semi-char-mode-map (kbd "M-<backspace>") #'eat-self-input)
  (define-key eat-semi-char-mode-map (kbd "M-d") #'eat-self-input)
  (define-key eat-semi-char-mode-map (kbd "M-f") #'eat-self-input)
  (define-key eat-semi-char-mode-map (kbd "M-b") #'eat-self-input)
  (define-key eat-semi-char-mode-map (kbd "C-<left>") #'eat-self-input)
  (define-key eat-semi-char-mode-map (kbd "C-<right>") #'eat-self-input)
  (define-key eat-semi-char-mode-map (kbd "C-k") #'eat-self-input)
  (define-key eat-semi-char-mode-map (kbd "C-j") #'eat-self-input)
  (define-key eat-semi-char-mode-map (kbd "C-y") #'sw/eat-yank)
  (define-key eat-semi-char-mode-map (kbd "C-,") #'sw/eat-zsh-history-pick)

  ;; TRAMP integration: rename buffer, inject `e` function to open remote files
  (defun sw/eat-find-file-handler (path)
    "Open PATH in another window."
    (when (and path (not (string-empty-p path)))
      (find-file-other-window path)))

  (add-to-list 'eat-message-handler-alist '("find-file" . sw/eat-find-file-handler))

  (defun sw/eat-setup-tramp (proc)
    "Configure eat for TRAMP: rename buffer, set TERM, inject `e` file opener."
    (when-let* ((buf (process-buffer proc))
                (_ (buffer-live-p buf))
                (_ (file-remote-p default-directory))
                (tramp-prefix (file-remote-p default-directory)))
      (with-current-buffer buf
        (rename-buffer (format "*eat@%s*" tramp-prefix) t)
        (setq-local eat-enable-shell-integration nil)
        (run-at-time
         0.5 nil
         (lambda (process prefix)
           (when (and (process-live-p process)
                      (buffer-live-p (process-buffer process)))
             (process-send-string
              process
              (format "export TERM=xterm-256color
e() { local f=\"$1\"; [[ \"$f\" != /* ]] && f=\"$PWD/$f\"; \
printf '\\033]51;e;M;%%s;%%s\\033\\\\' \"$(printf 'find-file' | base64)\" \
\"$(printf '%s%%s' \"$f\" | base64)\"; }
clear\n" prefix))))
         proc tramp-prefix))))

  (add-hook 'eat-exec-hook #'sw/eat-setup-tramp))

;;; Eat buffer management functions

(defun sw/eat--project-root ()
  "Return the project root directory or `default-directory'.
For remote directories, returns the remote default-directory."
  (sw/project-root-or-default t))

(defun sw/eat--buffer-for-dir (dir)
  "Return buffer name for eat in DIR."
  (if (file-remote-p dir)
      (format "*eat@%s*" (file-remote-p dir))
    (format "*eat:%s*" (abbreviate-file-name dir))))

(defun sw/eat--get-buffer (dir)
  "Get or create an eat buffer for DIR."
  (require 'eat)
  (let ((buf-name (sw/eat--buffer-for-dir dir)))
    (or (get-buffer buf-name)
        (sw/eat--new-buffer dir buf-name))))

(defun sw/eat--new-buffer (dir &optional name)
  "Create a new eat buffer for DIR with optional NAME."
  (require 'eat)
  (let* ((default-directory dir)
         (buf-name (or name (generate-new-buffer-name (sw/eat--buffer-for-dir dir)))))
    (if (file-remote-p dir)
        ;; For remote directories, use eat's built-in TRAMP support
        (let* ((remote (file-remote-p dir))
               (method (file-remote-p dir 'method))
               (shell (or (cdr (assoc method eat-tramp-shells)) "/bin/bash"))
               (buffer (get-buffer-create buf-name)))
          (with-current-buffer buffer
            (unless (eq major-mode #'eat-mode)
              (eat-mode))
            (unless (and (bound-and-true-p eat-terminal)
                         (eat-term-parameter eat-terminal 'eat--process))
              (eat-exec buffer buf-name shell nil nil)))
          buffer)
      ;; For local directories
      (let ((shell (or (getenv "SHELL") "/bin/bash"))
            (buffer (get-buffer-create buf-name)))
        (with-current-buffer buffer
          (unless (eq major-mode #'eat-mode)
            (eat-mode))
          (unless (and (bound-and-true-p eat-terminal)
                       (eat-term-parameter eat-terminal 'eat--process))
            (eat-exec buffer buf-name shell nil nil)))
        buffer))))

;;;###autoload
(defun sw/eat-here (&optional here)
  "Open a new eat buffer at the project root, replacing the current buffer.
If HERE is non-nil, open at current buffer's directory.
For remote directories, opens a shell on the remote host."
  (interactive "P")
  (require 'eat)
  (let* ((dir (if here
                  (or (and buffer-file-name (file-name-directory buffer-file-name))
                      default-directory)
                (sw/eat--project-root)))
         (buf (sw/eat--new-buffer dir)))
    (switch-to-buffer buf)))

;;;###autoload
(defun sw/eat-toggle (&optional here)
  "Toggle eat buffer visibility.
If HERE is non-nil, use buffer-specific directory.
For remote directories, opens a shell on the remote host."
  (interactive "P")
  (require 'eat)
  (let* ((dir (if here
                  (or (and buffer-file-name (file-name-directory buffer-file-name))
                      default-directory)
                (sw/eat--project-root)))
         (buf-name (sw/eat--buffer-for-dir dir))
         (buf (get-buffer buf-name)))
    (if-let ((win (and buf (get-buffer-window buf))))
        (delete-window win)
      (pop-to-buffer (sw/eat--get-buffer dir)))))

;;;###autoload
(defun sw/eat-here-current-buffer ()
  "Open an eat buffer from the current directory."
  (interactive)
  (sw/eat-here t))

;;;###autoload
(defun sw/eat-toggle-current-buffer ()
  "Toggle an eat buffer from the current directory."
  (interactive)
  (sw/eat-toggle t))

;;;###autoload
(defun sw/eat-zsh-history-pick ()
  "Prompt from zsh history and insert into eat (recency preserved)."
  (interactive)
  (require 'eat)
  (unless (bound-and-true-p eat-terminal)
    (user-error "No eat process in current buffer"))
  (let* ((history (sw/zsh-history-candidates))
         (collection (lambda (string pred action)
                       (if (eq action 'metadata)
                           '(metadata
                             (display-sort-function . identity)
                             (cycle-sort-function . identity))
                         (complete-with-action action history string pred))))
         (initial (or (thing-at-point 'symbol t) ""))
         (choice (completing-read "zsh history: " collection nil nil initial)))
    (when (thing-at-point 'symbol)
      (eat-term-send-string eat-terminal "\C-w"))
    (eat-term-send-string eat-terminal choice)))

;;;###autoload
(defun sw/eat-project ()
  "Open eat terminal in project root."
  (interactive)
  (sw/eat-here nil))

;;; External terminal helpers

(defun sw/terminal-here--default-directory ()
  "Directory where the terminal should start."
  (or (when buffer-file-name
        (file-name-directory buffer-file-name))
      (expand-file-name "~")))

(defun sw/terminal-here--pick-terminal ()
  "Pick which terminal to use for this system."
  (cond
   (sw/is-mac "alacritty")
   (sw/is-fedora "foot")
   (t "st")))

(defun sw/terminal-here--command ()
  "Build the shell command to launch the chosen terminal here."
  (let* ((term (sw/terminal-here--pick-terminal))
         (dir (sw/terminal-here--default-directory)))
    (unless (executable-find term)
      (error "Executable '%s' not found in PATH" term))
    (format "sh -lc 'cd %s && INSIDE_EMACS=%s %s' >/dev/null 2>&1"
            (shell-quote-argument dir) term term)))

;;;###autoload
(defun sw/terminal-here ()
  "Open a terminal window in the current directory."
  (interactive "@")
  (start-process-shell-command
   "terminal-here" nil
   (sw/terminal-here--command))
  (message "Terminal is ready!"))

(defun sw/terminal-ssh--command (host)
  "Build shell command to open an external terminal and SSH to HOST."
  (let* ((term (sw/terminal-here--pick-terminal))
         (extra-flags (if (string= term "foot") "-t xterm-256color" ""))
         (ssh-cmd (format "ssh %s" (shell-quote-argument host))))
    (unless (executable-find term)
      (error "Executable '%s' not found in PATH" term))
    (format "INSIDE_EMACS=1 %s %s -e sh -lc %s"
            term
            extra-flags
            (shell-quote-argument ssh-cmd))))

;;;###autoload
(defun sw/ssh-external (host)
  "Open an external terminal and SSH to HOST."
  (interactive
   (list (completing-read "SSH target: "
                          (sw/ssh-config-hosts)
                          nil nil)))
  (let ((cmd (sw/terminal-ssh--command host)))
    (message "SSH external running: %s" cmd)
    (start-process-shell-command
     "ssh-external" nil cmd)))

;;; TRAMP configuration

(use-package tramp
  :straight nil
  :defer t
  :custom
  (tramp-default-method "ssh")
  (tramp-verbose 1)
  (tramp-auto-save-directory (expand-file-name "tramp-autosave" user-emacs-directory))
  (tramp-persistency-file-name (expand-file-name "tramp" user-emacs-directory))
  (tramp-use-ssh-controlmaster-options nil)
  (remote-file-name-inhibit-cache nil)

  :config
  ;; SSH completion from config files
  (tramp-set-completion-function
   "ssh"
   (append
    (mapcar (lambda (f)
              (list 'tramp-parse-sconfig (expand-file-name f)))
            sw/ssh-config-files)
    '((tramp-parse-sconfig "/etc/ssh_config")
      (tramp-parse-shosts "/etc/hosts")
      (tramp-parse-shosts "~/.ssh/known_hosts"))))

  ;; Disable version control checks on remote files
  (setq vc-ignore-dir-regexp
        (format "\\(%s\\)\\|\\(%s\\)"
                vc-ignore-dir-regexp
                tramp-file-name-regexp)))

;; Docker TRAMP support (built-in since Emacs 29)
(use-package tramp-container
  :straight nil
  :after tramp)

(provide 'sw-terminal)
;;; sw-terminal.el ends here
