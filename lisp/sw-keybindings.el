;;; sw-keybindings.el --- Keybindings -*- lexical-binding: t -*-

;;; Commentary:
;; Global and mode-specific keybindings using general.el

;;; Code:

(require 'general)
(require 'sw-commands)

;;; Window navigation macro
;; Generates arrow keys, YNAE keys (custom layout), and split bindings for a prefix.

(defmacro sw-define-window-nav-keys (prefix label)
  "Define window navigation bindings under PREFIX with LABEL for which-key."
  (let ((p prefix))
    `(sw-leader
       ,(concat p "") '(:ignore t :wk ,label)
       ;; Arrow keys
       ,(concat p " <left>") '(evil-window-left :wk "Window left")
       ,(concat p " <right>") '(evil-window-right :wk "Window right")
       ,(concat p " <up>") '(evil-window-up :wk "Window up")
       ,(concat p " <down>") '(evil-window-down :wk "Window down")
       ;; YNAE keys (custom keyboard layout)
       ,(concat p "y") '(evil-window-left :wk "Window left")
       ,(concat p "n") '(evil-window-down :wk "Window down")
       ,(concat p "a") '(evil-window-up :wk "Window up")
       ,(concat p "e") '(evil-window-right :wk "Window right")
       ;; Splits (auto-select new window)
       ,(concat p "v") '(sw-split-window-right :wk "Split right")
       ,(concat p "s") '(sw-split-window-below :wk "Split below"))))

;;; Escape quits minibuffer

(dolist (map (list minibuffer-local-map
                   minibuffer-local-ns-map
                   minibuffer-local-completion-map
                   minibuffer-local-must-match-map))
  (define-key map [escape] #'abort-recursive-edit))

;;; Leader key bindings

(sw-leader
  ;; Top level
  "SPC" '(project-find-file :wk "Find file in project")
  "1"   '(sw-show-buffer-path :wk "Show path")
  "."   '(find-file :wk "Find file")
  ","   '(consult-buffer :wk "Switch buffer")
  "`"   '(evil-switch-to-windows-last-buffer :wk "Last buffer")
  ";"   '(pp-eval-expression :wk "Eval expression")
  ":"   '(execute-extended-command :wk "M-x")
  "'"   '(vertico-repeat :wk "Resume last search")
  "*"   '(sw-consult-ripgrep-project-symbol :wk "Search symbol in project")
  "/"   '(sw-consult-ripgrep-project :wk "Search project")
  "x"   '(scratch-buffer :wk "Scratch buffer")
  "RET" '(bookmark-jump :wk "Jump to bookmark")
  "§"   '(other-frame :wk "Other frame")
  "u"   '(universal-argument :wk "Universal arg")

  ;; Buffers
  "b" '(:ignore t :wk "Buffer")
  "bb" '(sw-workspace-switch-buffer :wk "Switch buffer")
  "bB" '(sw-switch-buffer-global :wk "Switch buffer (all)")
  "bd" '(kill-current-buffer :wk "Kill buffer")
  "bD" '(sw-workspace-kill-all-buffers :wk "Kill workspace buffers")
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
  "be" '(:ignore t :wk "Eat")
  "bek" '(sw-eat-kill-other :wk "Kill other eat buffers")
  "beK" '(sw-eat-kill-all :wk "Kill all eat buffers")

  ;; Files
  "f" '(:ignore t :wk "File")
  "ff" '(find-file :wk "Find file")
  "fF" '(consult-find :wk "Find file from here")
  "fd" '(dired-jump :wk "Find directory")
  "fr" '(consult-recent-file :wk "Recent files")
  "fs" '(save-buffer :wk "Save file")
  "fS" '(write-file :wk "Save file as")
  "fD" '(sw-delete-this-file :wk "Delete this file")
  "fy" '(sw-copy-file-path :wk "Yank file path")
  "f." '(sw-workspace-find-dotfiles :wk "Find in dotfiles")
  "fe" '(sw-workspace-find-emacs-config :wk "Find in .emacs.d")

  ;; Project
  "p" '(:ignore t :wk "Project")
  "pp" '(sw-workspace-switch-to-project :wk "Switch project")
  "pf" '(sw-project-find-file :wk "Find file in project")
  "pg" '(sw-consult-ripgrep-project :wk "Grep project")
  "pd" '(project-find-dir :wk "Find dir")
  "pb" '(project-switch-to-buffer :wk "Project buffer")
  "pk" '(project-kill-buffers :wk "Kill project buffers")
  "pK" '(sw-kill-all-projects-and-buffers :wk "Kill all projects")
  "pm" '(sw-project-make :wk "Run make")
  "pi" '(sw-project-refresh :wk "Refresh projects")

  ;; Search
  "s" '(:ignore t :wk "Search")
  "ss" '(consult-line :wk "Search buffer")
  "sS" '(consult-line-multi :wk "Search all buffers")
  "sp" '(sw-consult-ripgrep-project :wk "Search project")
  "sw" '(sw-consult-ripgrep-project-symbol :wk "Search symbol")
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
  "cf" '(apheleia-format-buffer :wk "Format buffer")
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
  "tf" '(load-theme :wk "Load theme")

  ;; Workspaces (tab-bar)
  "TAB" '(:ignore t :wk "Workspace")
  "TAB TAB" '(sw-workspace-display :wk "Display workspaces")
  "TAB ." '(tab-bar-switch-to-tab :wk "Switch workspace")
  "TAB n" '(sw-workspace-new :wk "New workspace")
  "TAB d" '(tab-bar-close-tab :wk "Close workspace")
  "TAB r" '(tab-bar-rename-tab :wk "Rename workspace")
  "TAB ]" '(tab-bar-switch-to-next-tab :wk "Next workspace")
  "TAB [" '(tab-bar-switch-to-prev-tab :wk "Prev workspace")
  "TAB 1" '(sw-workspace-switch-to-1 :wk "Workspace 1")
  "TAB 2" '(sw-workspace-switch-to-2 :wk "Workspace 2")
  "TAB 3" '(sw-workspace-switch-to-3 :wk "Workspace 3")
  "TAB 4" '(sw-workspace-switch-to-4 :wk "Workspace 4")
  "TAB 5" '(sw-workspace-switch-to-5 :wk "Workspace 5")
  "TAB 6" '(sw-workspace-switch-to-6 :wk "Workspace 6")
  "TAB 7" '(sw-workspace-switch-to-7 :wk "Workspace 7")
  "TAB 8" '(sw-workspace-switch-to-8 :wk "Workspace 8")
  "TAB 9" '(sw-workspace-switch-to-9 :wk "Workspace 9")

  ;; Window (unique bindings only, navigation via macro below)
  "w" '(:ignore t :wk "Window")
  "ww" '(other-window :wk "Other window")
  "wd" '(delete-window :wk "Delete window")
  "wD" '(delete-other-windows :wk "Delete others")
  "w=" '(balance-windows :wk "Balance")
  "wm" '(maximize-window :wk "Maximize")

  ;; Open/Apps
  "o" '(:ignore t :wk "Open")
  "o1" '(sw-terminal-here :wk "External terminal")
  "oT" '(sw-eat-here :wk "Eat at root")
  "ot" '(sw-eat-toggle :wk "Toggle eat at root")
  "oV" '(sw-eat-here-current-buffer :wk "Eat at buffer")
  "ov" '(sw-eat-toggle-current-buffer :wk "Toggle eat at buffer")
  "o." '(sw-tramp-connect :wk "TRAMP SSH")
  "os" '(sw-ssh-external :wk "SSH external term")
  "od" '(dired-jump :wk "Dired")
  "op" '(pass :wk "Pass")
  "ol" '(browse-url-at-point :wk "Open URL")

  ;; Tailscale
  "T" '(:ignore t :wk "Tailscale")
  "Ts" '(sw-tailscale-switch :wk "Switch account")
  "TS" '(sw-tailscale-status :wk "Status")
  "Tc" '(sw-tailscale-ssh :wk "SSH to device")

  ;; Claude AI / LSP
  "r" '(:ignore t :wk "AI/LSP")
  "rc" '(claude-code :wk "Claude chat")
  "rC" '(claude-code-continue :wk "Claude continue")
  "rR" '(claude-code-resume :wk "Claude resume")
  "rn" '(claude-code-new-instance :wk "New instance")
  "rd" '(claude-code-start-in-directory :wk "Start in directory")
  "rs" '(claude-code-select-buffer :wk "Switch buffer")
  "rt" '(sw-claude-code-toggle :wk "Toggle Claude")
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
  "id" '(sw-insert-date :wk "Date")
  "it" '(sw-insert-datetime :wk "Datetime")
  "ie" '(sw-insert-email :wk "Email")
  "iy" '(consult-yank-pop :wk "From kill ring")

  ;; Notes
  "n" '(:ignore t :wk "Notes")
  "nd" '(deft :wk "Deft")
  "nj" '(org-journal-new-entry :wk "Journal entry")

  ;; Help
  "h" '(:ignore t :wk "Help")
  "hf" '(helpful-callable :wk "Function")
  "hF" '(describe-char :wk "Face at point")
  "hv" '(helpful-variable :wk "Variable")
  "hk" '(helpful-key :wk "Key")
  "hp" '(helpful-at-point :wk "At point")
  "hi" '(info :wk "Info")
  "hm" '(describe-mode :wk "Mode")

  ;; Local/Mode (placeholder for which-key)
  "m" '(:ignore t :wk "Local mode"))

;; Window navigation bindings (arrows, YNAE, splits) for both prefixes.
;; "w" makes sense (for window), "l" is kept for muscle memory.
(sw-define-window-nav-keys "w" "Window")
(sw-define-window-nav-keys "l" "Select window")

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

 ;; Alignment (gl/gL operator, e.g., glip= to align paragraph by =)
 "gl" 'evil-lion-left
 "gL" 'evil-lion-right

 ;; Quick commands with ;
 ";w" 'save-buffer)

;;; Evil visual state bindings

(general-define-key
 :states 'visual
 "gc" 'evilnc-comment-operator
 "gl" 'evil-lion-left
 "gL" 'evil-lion-right
 ";f" 'sw-format-region)

;;; Emacs Lisp mode bindings

(sw-local-leader
  :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
  "e" '(:ignore t :wk "Eval")
  "eb" '(eval-buffer :wk "Eval buffer")
  "er" '(eval-region :wk "Eval region")
  "ee" '(eval-last-sexp :wk "Eval last sexp")
  "ed" '(eval-defun :wk "Eval defun"))

;;; Python mode bindings

(with-eval-after-load 'python
  (sw-local-leader
    :keymaps '(python-mode-map python-ts-mode-map)
    "f" '(sw-python-toggle-fstring :wk "Toggle f-string")
    "i" '(:ignore t :wk "Import")
    "io" '(sw-python-isort :wk "Optimize (isort)")))

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
   "dd" 'sw-eat-interrupt
   "p" 'sw-eat-yank)

  (general-define-key
   :keymaps 'eat-mode-map
   :states '(normal insert)
   "C-," 'sw-eat-zsh-history-pick))

;;; Deft mode bindings

(with-eval-after-load 'deft
  (general-define-key
   :keymaps 'deft-mode-map
   :states 'normal
   "gr" 'deft-refresh
   "a"  'deft-new-file
   "A"  'deft-new-file-named
   "d"  'deft-delete-file
   "D"  'deft-archive-file
   "r"  'deft-rename-file
   "q"  'kill-current-buffer)

  (general-define-key
   :keymaps 'deft-mode-map
   :states 'insert
   "C-n" 'deft-new-file
   "C-d" 'deft-delete-file
   "C-r" 'deft-rename-file))

(provide 'sw-keybindings)
;;; sw-keybindings.el ends here
