;;; sw-keybindings.el --- Keybindings -*- lexical-binding: t -*-

;;; Commentary:
;; Global and mode-specific keybindings.

;;; Code:

(require 'sw-commands)

;;; Which-key for discoverability

(use-package which-key
  :ensure nil
  :hook (sw-first-input . which-key-mode)
  :custom
  (which-key-idle-delay 0.3)
  (which-key-idle-secondary-delay 0.05)
  (which-key-separator "  ")
  (which-key-add-column-padding 2)
  (which-key-max-display-columns 4)
  (which-key-sort-order 'which-key-key-order)
  :config
  (set-face-attribute
   'which-key-key-face nil :weight 'bold))

;;; Leader key infrastructure

(defvar sw-leader-map (make-sparse-keymap)
  "Keymap for SPC leader bindings.")

(define-minor-mode sw-leader-mode
  "Global minor mode providing SPC as leader key."
  :global t :lighter nil
  :keymap (make-sparse-keymap))

(sw-leader-mode 1)

(evil-define-key* '(normal visual motion) sw-leader-mode-map
  (kbd "SPC") sw-leader-map)
(global-set-key (kbd "C-SPC") sw-leader-map)

(defvar sw-local-leader-alist nil
  "Alist of (MODE . KEYMAP) for SPC m bindings.")

(defun sw-local-leader-keymap ()
  "Return the local leader keymap for the current major mode."
  (or (alist-get major-mode sw-local-leader-alist)
      (cl-loop for (m . km) in sw-local-leader-alist
               when (derived-mode-p m) return km)))

(defun sw-define-keys (keymap bindings)
  "Define BINDINGS in KEYMAP with which-key labels.
BINDINGS is a list of (KEY DEF LABEL) entries.
DEF is a command or nil (prefix-only label)."
  (let (wk-args)
    (dolist (b bindings)
      (pcase-let ((`(,key ,def ,label) b))
        (when def
          (define-key keymap (kbd key) def))
        (when label
          (push key wk-args)
          (push label wk-args))))
    (when wk-args
      (apply #'which-key-add-keymap-based-replacements
             keymap (nreverse wk-args)))))

;;; Window navigation helper

(defun sw-define-window-nav-keys (prefix label)
  "Define window navigation bindings under PREFIX with LABEL."
  (sw-define-keys sw-leader-map
    `((,prefix nil ,label)
      (,(concat prefix " <left>")
       evil-window-left "Window left")
      (,(concat prefix " <right>")
       evil-window-right "Window right")
      (,(concat prefix " <up>")
       evil-window-up "Window up")
      (,(concat prefix " <down>")
       evil-window-down "Window down")
      (,(concat prefix " y")
       evil-window-left "Window left")
      (,(concat prefix " n")
       evil-window-down "Window down")
      (,(concat prefix " a")
       evil-window-up "Window up")
      (,(concat prefix " e")
       evil-window-right "Window right")
      (,(concat prefix " v")
       sw-split-window-right "Split right")
      (,(concat prefix " s")
       sw-split-window-below "Split below"))))

;;; Minibuffer bindings

(dolist (map (list minibuffer-local-map
                   minibuffer-local-ns-map
                   minibuffer-local-completion-map
                   minibuffer-local-must-match-map))
  (define-key map [escape] #'abort-recursive-edit)
  (define-key map (kbd "C-v") #'yank)
  (define-key map (kbd "C-r") #'evil-paste-from-register))

;;; Global editing bindings

(global-set-key (kbd "C-<backspace>") #'sw-backward-kill-word)
(when (eq system-type 'darwin)
  (global-set-key (kbd "A-<backspace>") #'sw-backward-kill-word))

;;; Text scaling (global)

(global-set-key (kbd "C-=") #'sw-text-scale-increase)
(global-set-key (kbd "C-+") #'sw-text-scale-increase)
(global-set-key (kbd "C--") #'sw-text-scale-decrease)
(global-set-key (kbd "C-0") #'sw-text-scale-reset)

;;; Leader key bindings

(sw-define-keys sw-leader-map
  '(;; Top level
    ("SPC" project-find-file "Find file in project")
    ("1"   sw-show-buffer-path "Show path")
    ("."   find-file "Find file")
    (","   consult-buffer "Switch buffer")
    ("a"   evil-switch-to-windows-last-buffer "Last buffer")
    (":"   execute-extended-command "M-x")
    ("'"   vertico-repeat "Resume last search")
    ("x"   scratch-buffer "Scratch buffer")
    ("RET" bookmark-jump "Jump to bookmark")
    ("u"   universal-argument "Universal arg")
    ("y"   sw-copy-dedented "Copy dedented")

    ;; Buffers
    ("b"   nil "Buffer")
    ("b b" sw-workspace-switch-buffer "Switch buffer")
    ("b B" sw-switch-buffer-global "Switch buffer (all)")
    ("b d" kill-current-buffer "Kill buffer")
    ("b D" sw-workspace-kill-all-buffers
     "Kill workspace buffers")
    ("b i" ibuffer "Ibuffer")
    ("b l" evil-switch-to-windows-last-buffer "Last buffer")
    ("b m" bookmark-set "Set bookmark")
    ("b M" bookmark-delete "Delete bookmark")
    ("b n" next-buffer "Next buffer")
    ("b p" previous-buffer "Previous buffer")
    ("b r" revert-buffer "Revert buffer")
    ("b s" save-buffer "Save buffer")
    ("b S" evil-write-all "Save all buffers")
    ("b x" scratch-buffer "Scratch buffer")

    ;; Eat terminal
    ("e"   nil "Eat")
    ("e e" sw-eat-here "Eat at root")
    ("e t" sw-eat-show "Show eat at root")
    ("e E" sw-eat-here-current-buffer "Eat at buffer")
    ("e T" sw-eat-show-current-buffer
     "Show eat at buffer")
    ("e k" sw-eat-kill-other "Kill other eat buffers")
    ("e K" sw-eat-kill-all "Kill all eat buffers")

    ;; Files
    ("f"   nil "File")
    ("f f" find-file "Find file")
    ("f F" consult-find "Find file from here")
    ("f d" dired-jump "Find directory")
    ("f r" consult-recent-file "Recent files")
    ("f s" save-buffer "Save file")
    ("f S" write-file "Save file as")
    ("f u" sudo-edit "Sudo this file")
    ("f U" sudo-edit-find-file "Sudo find file")
    ("f D" sw-delete-this-file "Delete this file")
    ("f y" sw-copy-file-path "Yank file path")
    ("f ." sw-workspace-find-dotfiles "Find in dotfiles")
    ("f e" sw-workspace-find-emacs-config
     "Find in .emacs.d")

    ;; Project
    ("p"   nil "Project")
    ("p p" sw-workspace-switch-to-project
     "Switch project")
    ("p f" sw-project-find-file "Find file in project")
    ("p g" sw-consult-ripgrep-project "Grep project")
    ("p d" project-find-dir "Find dir")
    ("p b" project-switch-to-buffer "Project buffer")
    ("p k" project-kill-buffers "Kill project buffers")
    ("p K" sw-kill-all-projects-and-buffers
     "Kill all projects")
    ("p m" sw-project-make "Run make")
    ("p i" sw-project-refresh "Refresh projects")

    ;; Search
    ("s"   nil "Search")
    ("s s" consult-line "Search buffer")
    ("s S" consult-line-multi "Search all buffers")
    ("s p" sw-consult-ripgrep-project "Search project")
    ("s b" sw-consult-line-symbol
     "Search symbol in buffer")
    ("s w" sw-consult-ripgrep-project-symbol
     "Search symbol in project")
    ("s i" consult-imenu "Imenu")
    ("s I" consult-imenu-multi "Imenu all")
    ("s r" consult-ripgrep "Ripgrep")
    ("s f" consult-find "Find file")
    ("s m" consult-mark "Marks")
    ("s o" consult-outline "Outline")

    ;; Code/LSP
    ("c"   nil "Code")
    ("c a" eglot-code-actions "Code actions")
    ("c r" eglot-rename "Rename")
    ("c f" apheleia-format-buffer "Format buffer")
    ("c d" xref-find-definitions "Go to definition")
    ("c D" xref-find-references "Find references")
    ("c i" eglot-find-implementation "Find impl")
    ("c t" eglot-find-typeDefinition "Find type def")
    ("c k" eldoc "Show docs")
    ("c h" symbol-overlay-put "Highlight symbol")
    ("c H" symbol-overlay-remove-all
     "Clear highlights")

    ;; Git
    ("g"     nil "Git")
    ("g /"   magit-dispatch "Magit dispatch")
    ("g ."   magit-file-dispatch "File dispatch")
    ("g b"   magit-branch-checkout "Switch branch")
    ("g B"   magit-blame-addition "Blame")
    ("g c"   nil "Create")
    ("g c c" magit-commit-create "Commit")
    ("g c f" magit-commit-fixup "Fixup")
    ("g c b" magit-branch-and-checkout "Branch")
    ("g d"   magit-diff-dwim "Diff")
    ("g f"   nil "Find")
    ("g f f" magit-find-file "Find file")
    ("g f c" magit-show-commit "Find commit")
    ("g F"   magit-fetch "Fetch")
    ("g g"   magit-status "Status")
    ("g G"   magit-status-here "Status here")
    ("g l"   nil "List")
    ("g l r" magit-list-repositories "Repositories")
    ("g L"   magit-log-buffer-file "Log file")
    ("g r"   diff-hl-revert-hunk "Revert hunk")
    ("g s"   diff-hl-stage-current-hunk "Stage hunk")
    ("g S"   magit-file-stage "Stage file")
    ("g U"   magit-file-unstage "Unstage file")
    ("g t"   git-timemachine-toggle "Timemachine")
    ("g o"   nil "Open")
    ("g o o" browse-at-remote "Browse remote")
    ("g o y" browse-at-remote-kill "Copy remote URL")
    ("g ["   diff-hl-previous-hunk "Previous hunk")
    ("g ]"   diff-hl-next-hunk "Next hunk")

    ;; Help
    ("h"   nil "Help")
    ("h r" restart-emacs "Restart Emacs")
    ("h f" helpful-callable "Function")
    ("h F" describe-char "Face at point")
    ("h v" helpful-variable "Variable")
    ("h k" helpful-key "Key")
    ("h p" helpful-at-point "At point")
    ("h i" info "Info")
    ("h m" describe-mode "Mode")

    ;; Toggle
    ("t"   nil "Toggle")
    ("t t" toggle-truncate-lines "Truncate lines")
    ("t n" display-line-numbers-mode "Line numbers")
    ("t i" imenu-list-smart-toggle "Imenu list")
    ("t w" whitespace-mode "Whitespace")
    ("t f" load-theme "Load theme")
    ("t a" warm-mode "Warm mode")

    ;; Workspaces (tab-bar)
    ("TAB"     nil "Workspace")
    ("TAB TAB" sw-workspace-display
     "Display workspaces")
    ("TAB ." tab-bar-switch-to-tab "Switch workspace")
    ("TAB n" sw-workspace-new "New workspace")
    ("TAB d" tab-bar-close-tab "Close workspace")
    ("TAB r" tab-bar-rename-tab "Rename workspace")
    ("TAB ]" tab-bar-switch-to-next-tab
     "Next workspace")
    ("TAB [" tab-bar-switch-to-prev-tab
     "Prev workspace")
    ("TAB 1" sw-workspace-switch-to-1 "Workspace 1")
    ("TAB 2" sw-workspace-switch-to-2 "Workspace 2")
    ("TAB 3" sw-workspace-switch-to-3 "Workspace 3")
    ("TAB 4" sw-workspace-switch-to-4 "Workspace 4")
    ("TAB 5" sw-workspace-switch-to-5 "Workspace 5")
    ("TAB 6" sw-workspace-switch-to-6 "Workspace 6")
    ("TAB 7" sw-workspace-switch-to-7 "Workspace 7")
    ("TAB 8" sw-workspace-switch-to-8 "Workspace 8")
    ("TAB 9" sw-workspace-switch-to-9 "Workspace 9")

    ;; Window (unique bindings, navigation via function below)
    ("w"   nil "Window")
    ("w w" other-window "Other window")
    ("w d" delete-window "Delete window")
    ("w D" delete-other-windows "Delete others")
    ("w =" balance-windows "Balance")
    ("w m" maximize-window "Maximize")

    ;; Open/Apps
    ("o"   nil "Open")
    ("o 1" sw-terminal-here "External terminal")
    ("o ." sw-tramp-connect "TRAMP SSH")
    ("o s" sw-ssh-external "SSH external term")
    ("o d" dired-jump "Dired")
    ("o p" pass "Pass")
    ("o l" browse-url-at-point "Open URL")

    ;; Tailscale
    ("T"   nil "Tailscale")
    ("T s" sw-tailscale-switch "Switch account")
    ("T S" sw-tailscale-status "Status")
    ("T c" sw-tailscale-ssh "SSH to device")

    ;; AI/LSP
    ("r"   nil "AI/LSP")
    ("r c" claude-code "Claude chat")
    ("r C" claude-code-continue "Claude continue")
    ("r R" claude-code-resume "Claude resume")
    ("r n" claude-code-new-instance "New instance")
    ("r d" claude-code-start-in-directory
     "Start in directory")
    ("r s" claude-code-select-buffer "Switch buffer")
    ("r t" sw-claude-code-toggle "Toggle Claude")
    ("r r" claude-code-send-region "Send region")
    ("r p" claude-code-send-command "Send command")
    ("r x" claude-code-send-command-with-context
     "Send with context")
    ("r b" claude-code-send-buffer-file
     "Send buffer file")
    ("r e" claude-code-fix-error-at-point
     "Fix error at point")
    ("r m" claude-code-cycle-mode "Cycle mode")
    ("r M" claude-code-transient "Transient menu")
    ("r k" claude-code-kill "Kill session")
    ("r w" eglot-reconnect "Reconnect Eglot")

    ;; Insert
    ("i"   nil "Insert")
    ("i d" sw-insert-date "Date")
    ("i t" sw-insert-datetime "Datetime")
    ("i e" sw-insert-email "Email")
    ("i y" consult-yank-pop "From kill ring")
    ("i s" yas-insert-snippet "Snippet")

    ;; Notes
    ("n"   nil "Notes")
    ("n d" deft "Deft")
    ("n j" org-journal-new-entry "Journal entry")))

;; SPC m dispatches to the mode-local keymap via menu-item filter.
;; which-key can't introspect :filter, so the label is set separately.
(define-key sw-leader-map "m"
  `(menu-item "Local mode" nil
    :filter ,(lambda (&optional _)
               (sw-local-leader-keymap))))
(which-key-add-key-based-replacements "SPC m" "Local mode")

;; Window navigation bindings (arrows, YNAE, splits).
;; "w" for window, "l" kept for muscle memory.
(sw-define-window-nav-keys "w" "Window")
(sw-define-window-nav-keys "l" "Window")

;;; Evil normal state bindings

(evil-define-key* 'normal 'global
  ;; Window navigation (HJKL and YNAE for custom layout)
  (kbd "C-h") #'evil-window-left
  (kbd "C-j") #'evil-window-down
  (kbd "C-l") #'evil-window-right
  (kbd "C-y") #'evil-window-left
  (kbd "C-n") #'evil-window-down
  (kbd "C-e") #'evil-window-right

  ;; Line operations
  (kbd "M-o") #'delete-blank-lines
  (kbd "C-k") #'join-line
  (kbd "C-a") #'join-line
  (kbd "B")   #'beginning-of-line-text
  (kbd "E")   #'end-of-line

  ;; Buffer navigation
  (kbd "]b") #'next-buffer
  (kbd "[b") #'previous-buffer

  ;; Git hunk navigation
  (kbd "]g") #'diff-hl-next-hunk
  (kbd "[g") #'diff-hl-previous-hunk

  ;; Commenting
  (kbd "gc") #'evilnc-comment-operator

  ;; Alignment
  (kbd "gl") #'evil-lion-left
  (kbd "gL") #'evil-lion-right

  ;; Quick commands with ;
  (kbd ";w") #'save-buffer
  (kbd ";q") #'evil-quit

  ;; Symbol highlight (stay at point, use n/N to navigate)
  (kbd "*")  #'sw-highlight-symbol-at-point
  (kbd "n")  #'sw-search-next
  (kbd "N")  #'sw-search-previous
  [escape]   #'evil-ex-nohighlight)

;;; Evil visual state bindings

(evil-define-key* 'visual 'global
  (kbd "gc") #'evilnc-comment-operator
  (kbd "gl") #'evil-lion-left
  (kbd "gL") #'evil-lion-right
  (kbd ";f") #'sw-format-region)

;;; Emacs Lisp mode bindings

(defvar sw-elisp-local-map (make-sparse-keymap)
  "Local leader keymap for Emacs Lisp modes.")

(sw-define-keys sw-elisp-local-map
  '(("r"   sw-ielm-toggle "Toggle REPL")
    ("e"   nil "Eval")
    ("e b" eval-buffer "Eval buffer")
    ("e r" eval-region "Eval region")
    ("e e" eval-last-sexp "Eval last sexp")
    ("e d" eval-defun "Eval defun")))

(dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
  (push (cons mode sw-elisp-local-map)
        sw-local-leader-alist))

;;; Python mode bindings

(with-eval-after-load 'python
  (defvar sw-python-local-map (make-sparse-keymap)
    "Local leader keymap for Python modes.")

  (sw-define-keys sw-python-local-map
    '(("f"   sw-python-toggle-fstring "Toggle f-string")
      ("r"   sw-python-repl-toggle "Toggle REPL")
      ("i"   nil "Import")
      ("i o" sw-python-isort "Optimize (isort)")
      ("t"   nil "Test")
      ("t t" python-pytest-run-def-or-class-at-point
       "Test at point")
      ("t f" python-pytest-file-dwim "Test file")
      ("t a" python-pytest "Test all")
      ("t r" python-pytest-repeat "Repeat last")
      ("t p" python-pytest-dispatch
       "Pytest dispatch")))

  (dolist (mode '(python-mode python-ts-mode))
    (push (cons mode sw-python-local-map)
          sw-local-leader-alist)))

;;; SQL mode bindings

(with-eval-after-load 'sql
  (defvar sw-sql-local-map (make-sparse-keymap)
    "Local leader keymap for SQL modes.")

  (sw-define-keys sw-sql-local-map
    '(("r" sw-sql-repl-toggle "Toggle REPL")))

  (push (cons 'sql-mode sw-sql-local-map)
        sw-local-leader-alist))

;;; Go mode bindings

(with-eval-after-load 'go-ts-mode
  (defvar sw-go-local-map (make-sparse-keymap)
    "Local leader keymap for Go modes.")

  (sw-define-keys sw-go-local-map
    `(("t"   nil "Test")
      ("t t" go-test-current-test "Test at point")
      ("t f" go-test-current-file "Test file")
      ("t a" go-test-current-project "Test all")
      ("b"   nil "Build")
      ("b r" ,(lambda () (interactive)
                (compile "go run ."))
       "Run")
      ("b b" ,(lambda () (interactive)
                (compile "go build"))
       "Build")))

  (push (cons 'go-ts-mode sw-go-local-map)
        sw-local-leader-alist))

;;; Eat mode bindings

(with-eval-after-load 'eat
  (evil-define-key* 'normal eat-mode-map
    (kbd "B")   #'beginning-of-line
    (kbd "E")   #'end-of-line
    (kbd "0")   #'beginning-of-line
    (kbd "$")   #'end-of-line
    (kbd "G")   #'end-of-buffer
    (kbd "gg")  #'beginning-of-buffer
    (kbd "C-u") #'evil-scroll-up
    (kbd "C-d") #'evil-scroll-down
    (kbd "RET") #'evil-insert-state
    (kbd "dd")  #'sw-eat-interrupt
    (kbd "p")   #'sw-eat-yank)

  (evil-define-key* '(normal insert) eat-mode-map
    (kbd "C-,") #'sw-eat-zsh-history-pick)

  ;; Semi-char mode bindings
  (define-key eat-semi-char-mode-map
    (kbd "<escape>") #'evil-normal-state)
  (define-key eat-semi-char-mode-map
    (kbd "C-<backspace>") #'sw-eat-backward-kill-word)
  (define-key eat-semi-char-mode-map
    (kbd "M-<backspace>") #'eat-self-input)
  (define-key eat-semi-char-mode-map
    (kbd "M-d") #'eat-self-input)
  (define-key eat-semi-char-mode-map
    (kbd "M-f") #'eat-self-input)
  (define-key eat-semi-char-mode-map
    (kbd "M-b") #'eat-self-input)
  (define-key eat-semi-char-mode-map
    (kbd "C-<left>") #'eat-self-input)
  (define-key eat-semi-char-mode-map
    (kbd "C-<right>") #'eat-self-input)
  (define-key eat-semi-char-mode-map
    (kbd "C-k") #'eat-self-input)
  (define-key eat-semi-char-mode-map
    (kbd "C-j") #'eat-self-input)
  (define-key eat-semi-char-mode-map
    (kbd "C-y") #'sw-eat-yank)
  (define-key eat-semi-char-mode-map
    (kbd "C-,") #'sw-eat-zsh-history-pick))

;;; Wgrep mode bindings

(with-eval-after-load 'wgrep
  (evil-define-key* 'normal grep-mode-map
    "i"  #'wgrep-change-to-wgrep-mode)
  (evil-define-key* 'normal wgrep-mode-map
    ";w" #'wgrep-finish-edit
    ";q" #'wgrep-abort-changes))

;;; Dired mode bindings

(with-eval-after-load 'dired
  (evil-define-key* 'normal dired-mode-map
    (kbd "TAB")      #'dired-subtree-toggle
    (kbd "<backtab>") #'dired-subtree-remove
    (kbd "/")        #'dired-narrow-fuzzy))

;;; Git timemachine bindings

(with-eval-after-load 'git-timemachine
  (evil-define-key* 'normal git-timemachine-mode-map
    (kbd "C-n") #'git-timemachine-show-previous-revision
    (kbd "C-a") #'git-timemachine-show-next-revision))

;;; Deft mode bindings

(with-eval-after-load 'deft
  (defvar sw-deft-local-map (make-sparse-keymap)
    "Local leader keymap for Deft mode.")

  (sw-define-keys sw-deft-local-map
    '(("a" deft-new-file "New file")
      ("A" deft-new-file-named "New file (named)")
      ("d" deft-delete-file "Delete file")
      ("D" deft-archive-file "Archive file")
      ("r" deft-rename-file "Rename file")
      ("g" deft-refresh "Refresh")
      ("q" kill-current-buffer "Quit")))

  (push (cons 'deft-mode sw-deft-local-map)
        sw-local-leader-alist)

  (evil-define-key* 'normal deft-mode-map
    (kbd "gr") #'deft-refresh
    (kbd "a")  #'deft-new-file
    (kbd "A")  #'deft-new-file-named
    (kbd "d")  #'deft-delete-file
    (kbd "D")  #'deft-archive-file
    (kbd "r")  #'deft-rename-file
    (kbd "q")  #'kill-current-buffer)

  (evil-define-key* 'insert deft-mode-map
    (kbd "C-n") #'deft-new-file
    (kbd "C-d") #'deft-delete-file
    (kbd "C-r") #'deft-rename-file))

(provide 'sw-keybindings)
;;; sw-keybindings.el ends here
