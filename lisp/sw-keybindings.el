;;; sw-keybindings.el --- Keybindings -*- lexical-binding: t -*-

;;; Commentary:
;; Global and mode-specific keybindings using general.el

;;; Code:

(require 'general)
(require 'sw-commands)

;;; Escape quits everything

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;;; Leader key bindings

(sw/leader
  ;; Top level
  "SPC" '(project-find-file :wk "Find file in project")
  "1"   '(sw/show-buffer-path :wk "Show path")
  "."   '(find-file :wk "Find file")
  ","   '(consult-buffer :wk "Switch buffer")
  "`"   '(evil-switch-to-windows-last-buffer :wk "Last buffer")
  ";"   '(pp-eval-expression :wk "Eval expression")
  ":"   '(execute-extended-command :wk "M-x")
  "'"   '(vertico-repeat :wk "Resume last search")
  "*"   '(sw/consult-ripgrep-project-symbol :wk "Search symbol in project")
  "/"   '(sw/consult-ripgrep-project :wk "Search project")
  "x"   '(scratch-buffer :wk "Scratch buffer")
  "RET" '(bookmark-jump :wk "Jump to bookmark")
  "§"   '(other-frame :wk "Other frame")
  "u"   '(universal-argument :wk "Universal arg")

  ;; Buffers
  "b" '(:ignore t :wk "Buffer")
  "bb" '(sw/workspace-switch-buffer :wk "Switch buffer")
  "bB" '(sw/switch-buffer-global :wk "Switch buffer (all)")
  "bd" '(kill-current-buffer :wk "Kill buffer")
  "bi" '(ibuffer :wk "Ibuffer")
  "bl" '(evil-switch-to-windows-last-buffer :wk "Last buffer")
  "bm" '(bookmark-set :wk "Set bookmark")
  "bM" '(bookmark-delete :wk "Delete bookmark")
  "bn" '(next-buffer :wk "Next buffer")
  "bp" '(previous-buffer :wk "Previous buffer")
  "br" '(revert-buffer :wk "Revert buffer")
  "bs" '(save-buffer :wk "Save buffer")
  "bS" '(evil-write-all :wk "Save all buffers")
  "bx" '(scratch-buffer :wk "Scratch buffer")

  ;; Files
  "f" '(:ignore t :wk "File")
  "ff" '(find-file :wk "Find file")
  "fF" '(consult-find :wk "Find file from here")
  "fd" '(dired-jump :wk "Find directory")
  "fr" '(consult-recent-file :wk "Recent files")
  "fs" '(save-buffer :wk "Save file")
  "fS" '(write-file :wk "Save file as")
  "fD" '(sw/delete-this-file :wk "Delete this file")
  "fy" '(sw/copy-file-path :wk "Yank file path")
  "f." '(sw/workspace-find-dotfiles :wk "Find in dotfiles")
  "fe" '(sw/workspace-find-emacs-config :wk "Find in .emacs.d")

  ;; Project
  "p" '(:ignore t :wk "Project")
  "pp" '(sw/workspace-switch-to-project :wk "Switch project")
  "pf" '(sw/project-find-file :wk "Find file in project")
  "pg" '(sw/consult-ripgrep-project :wk "Grep project")
  "pd" '(project-find-dir :wk "Find dir")
  "pb" '(project-switch-to-buffer :wk "Project buffer")
  "pk" '(project-kill-buffers :wk "Kill project buffers")
  "pm" '(sw/project-make :wk "Run make")

  ;; Search
  "s" '(:ignore t :wk "Search")
  "ss" '(consult-line :wk "Search buffer")
  "sS" '(consult-line-multi :wk "Search all buffers")
  "sp" '(sw/consult-ripgrep-project :wk "Search project")
  "sw" '(sw/consult-ripgrep-project-symbol :wk "Search symbol")
  "si" '(consult-imenu :wk "Imenu")
  "sI" '(consult-imenu-multi :wk "Imenu all")
  "sr" '(consult-ripgrep :wk "Ripgrep")
  "sf" '(consult-find :wk "Find file")
  "sm" '(consult-mark :wk "Marks")
  "so" '(consult-outline :wk "Outline")

  ;; Code/LSP
  "c" '(:ignore t :wk "Code")
  "ca" '(eglot-code-actions :wk "Code actions")
  "cr" '(eglot-rename :wk "Rename")
  "cf" '(sw/format-buffer-or-region :wk "Format")
  "cd" '(xref-find-definitions :wk "Go to definition")
  "cD" '(xref-find-references :wk "Find references")
  "ci" '(eglot-find-implementation :wk "Find impl")
  "ct" '(eglot-find-typeDefinition :wk "Find type def")
  "ck" '(eldoc :wk "Show docs")
  "ch" '(symbol-overlay-put :wk "Highlight symbol")
  "cH" '(symbol-overlay-remove-all :wk "Clear highlights")

  ;; Git (Doom-style)
  "g" '(:ignore t :wk "Git")
  "g/" '(magit-dispatch :wk "Magit dispatch")
  "g." '(magit-file-dispatch :wk "File dispatch")
  "gb" '(magit-branch-checkout :wk "Switch branch")
  "gB" '(magit-blame-addition :wk "Blame")
  "gc" '(:ignore t :wk "Create")
  "gcc" '(magit-commit-create :wk "Commit")
  "gcf" '(magit-commit-fixup :wk "Fixup")
  "gcb" '(magit-branch-and-checkout :wk "Branch")
  "gd" '(magit-diff-dwim :wk "Diff")
  "gf" '(:ignore t :wk "Find")
  "gff" '(magit-find-file :wk "Find file")
  "gfc" '(magit-show-commit :wk "Find commit")
  "gF" '(magit-fetch :wk "Fetch")
  "gg" '(magit-status :wk "Status")
  "gG" '(magit-status-here :wk "Status here")
  "gl" '(:ignore t :wk "List")
  "glr" '(magit-list-repositories :wk "Repositories")
  "gL" '(magit-log-buffer-file :wk "Log file")
  "gr" '(diff-hl-revert-hunk :wk "Revert hunk")
  "gs" '(diff-hl-stage-current-hunk :wk "Stage hunk")
  "gS" '(magit-file-stage :wk "Stage file")
  "gU" '(magit-file-unstage :wk "Unstage file")
  "gt" '(git-timemachine-toggle :wk "Timemachine")
  "go" '(:ignore t :wk "Open")
  "goo" '(browse-at-remote :wk "Browse remote")
  "goy" '(browse-at-remote-kill :wk "Copy remote URL")
  "g[" '(diff-hl-previous-hunk :wk "Previous hunk")
  "g]" '(diff-hl-next-hunk :wk "Next hunk")

  ;; Toggle
  "t" '(:ignore t :wk "Toggle")
  "tt" '(toggle-truncate-lines :wk "Truncate lines")
  "tn" '(display-line-numbers-mode :wk "Line numbers")
  "ti" '(imenu-list-smart-toggle :wk "Imenu list")
  "tw" '(whitespace-mode :wk "Whitespace")
  "tb" '(blamer-mode :wk "Git blame")

  ;; Workspaces (tab-bar)
  "TAB" '(:ignore t :wk "Workspace")
  "TAB TAB" '(sw/workspace-display :wk "Display workspaces")
  "TAB ." '(tab-bar-switch-to-tab :wk "Switch workspace")
  "TAB n" '(sw/workspace-new :wk "New workspace")
  "TAB d" '(tab-bar-close-tab :wk "Close workspace")
  "TAB r" '(tab-bar-rename-tab :wk "Rename workspace")
  "TAB ]" '(tab-bar-switch-to-next-tab :wk "Next workspace")
  "TAB [" '(tab-bar-switch-to-prev-tab :wk "Prev workspace")

  ;; Window
  "w" '(:ignore t :wk "Window")
  "ww" '(other-window :wk "Other window")
  "wv" '(split-window-right :wk "Split right")
  "ws" '(split-window-below :wk "Split below")
  "wd" '(delete-window :wk "Delete window")
  "wD" '(delete-other-windows :wk "Delete others")
  "w=" '(balance-windows :wk "Balance")
  "wm" '(maximize-window :wk "Maximize")
  "w <left>" '(evil-window-left :wk "Window left")
  "w <right>" '(evil-window-right :wk "Window right")
  "w <up>" '(evil-window-up :wk "Window up")
  "w <down>" '(evil-window-down :wk "Window down")
  "wy" '(evil-window-left :wk "Window left")
  "wn" '(evil-window-down :wk "Window down")
  "wa" '(evil-window-up :wk "Window up")
  "we" '(evil-window-right :wk "Window right")

  ;; Window selection (arrow keys)
  "l" '(:ignore t :wk "Select window")
  "l <left>" '(evil-window-left :wk "Window left")
  "l <right>" '(evil-window-right :wk "Window right")
  "l <up>" '(evil-window-up :wk "Window up")
  "l <down>" '(evil-window-down :wk "Window down")
  "ly" '(evil-window-left :wk "Window left")
  "ln" '(evil-window-down :wk "Window down")
  "la" '(evil-window-up :wk "Window up")
  "le" '(evil-window-right :wk "Window right")
  "lv" '(split-window-right :wk "Split right")
  "ls" '(split-window-below :wk "Split below")

  ;; Open/Apps
  "o" '(:ignore t :wk "Open")
  "o1" '(sw/terminal-here :wk "External terminal")
  "oT" '(sw/eat-here :wk "Eat at root")
  "ot" '(sw/eat-toggle :wk "Toggle eat at root")
  "oV" '(sw/eat-here-current-buffer :wk "Eat at buffer")
  "ov" '(sw/eat-toggle-current-buffer :wk "Toggle eat at buffer")
  "o." '(sw/tramp-connect :wk "TRAMP SSH")
  "os" '(sw/ssh-external :wk "SSH external term")
  "od" '(dired-jump :wk "Dired")
  "op" '(pass :wk "Pass")
  "ol" '(browse-url-at-point :wk "Open URL")

  ;; Claude AI / LSP
  "r" '(:ignore t :wk "AI/LSP")
  "rc" '(claude-code :wk "Claude chat")
  "rC" '(claude-code-continue :wk "Claude continue")
  "rR" '(claude-code-resume :wk "Claude resume")
  "rn" '(claude-code-new-instance :wk "New instance")
  "rd" '(claude-code-start-in-directory :wk "Start in directory")
  "rs" '(claude-code-select-buffer :wk "Switch buffer")
  "rt" '(sw/claude-code-toggle :wk "Toggle Claude")
  "rr" '(claude-code-send-region :wk "Send region")
  "rp" '(claude-code-send-command :wk "Send command")
  "rx" '(claude-code-send-command-with-context :wk "Send with context")
  "rb" '(claude-code-send-buffer-file :wk "Send buffer file")
  "re" '(claude-code-fix-error-at-point :wk "Fix error at point")
  "rm" '(claude-code-cycle-mode :wk "Cycle mode")
  "rM" '(claude-code-transient :wk "Transient menu")
  "rk" '(claude-code-kill :wk "Kill session")
  "rw" '(eglot-reconnect :wk "Reconnect Eglot")

  ;; Insert
  "i" '(:ignore t :wk "Insert")
  "id" '(sw/insert-date :wk "Date")
  "it" '(sw/insert-datetime :wk "Datetime")
  "ie" '(sw/insert-email :wk "Email")
  "iy" '(consult-yank-pop :wk "From kill ring")

  ;; Help
  "h" '(:ignore t :wk "Help")
  "hf" '(helpful-callable :wk "Function")
  "hv" '(helpful-variable :wk "Variable")
  "hk" '(helpful-key :wk "Key")
  "hp" '(helpful-at-point :wk "At point")
  "hi" '(info :wk "Info")
  "hm" '(describe-mode :wk "Mode")

  ;; Local/Mode (placeholder for which-key)
  "m" '(:ignore t :wk "Local mode"))

;; Workspace number bindings (TAB 1-9)
(dotimes (i 9)
  (let* ((n (1+ i))
         (fn (intern (format "sw/workspace-switch-to-%d" n))))
    (eval `(sw/leader
            ,(format "TAB %d" n)
            '(,fn :wk ,(format "Workspace %d" n))))))

;;; Evil normal state bindings

(general-define-key
 :states 'normal
 ;; Window navigation (HJKL and YNAE for custom layout)
 "C-h" 'evil-window-left
 "C-j" 'evil-window-down
 "C-l" 'evil-window-right
 "C-y" 'evil-window-left
 "C-n" 'evil-window-down
 "C-e" 'evil-window-right

 ;; Line operations
 "M-o" 'delete-blank-lines
 "C-k" 'join-line
 "C-a" 'join-line
 "B" 'beginning-of-line-text
 "E" 'end-of-line

 ;; Buffer navigation
 "]b" 'next-buffer
 "[b" 'previous-buffer

 ;; Git hunk navigation
 "]g" 'diff-hl-next-hunk
 "[g" 'diff-hl-previous-hunk

 ;; Commenting (gc is an operator, gcc handled by evil-nerd-commenter)
 "gc" 'evilnc-comment-operator

 ;; Quick commands with ;
 ";f" 'sw/format-buffer-or-region
 ";w" 'save-buffer)

;;; Evil visual state bindings

(general-define-key
 :states 'visual
 "gc" 'evilnc-comment-operator
 ";f" 'sw/format-buffer-or-region)

;;; Python mode bindings

(with-eval-after-load 'python
  (sw/local-leader
    :keymaps '(python-mode-map python-ts-mode-map)
    "f" '(sw/python-toggle-fstring :wk "Toggle f-string")))

;;; Eat mode bindings

(with-eval-after-load 'eat
  (general-define-key
   :keymaps 'eat-mode-map
   :states 'normal
   "B" 'beginning-of-line
   "E" 'end-of-line
   "0" 'beginning-of-line
   "$" 'end-of-line
   "G" 'end-of-buffer
   "gg" 'beginning-of-buffer
   "C-u" 'evil-scroll-up
   "C-d" 'evil-scroll-down
   "RET" 'evil-insert-state
   "dd" 'sw/eat-interrupt
   "p" 'sw/eat-yank)

  (general-define-key
   :keymaps 'eat-mode-map
   :states '(normal insert)
   "C-," 'sw/eat-zsh-history-pick))

(provide 'sw-keybindings)
;;; sw-keybindings.el ends here
