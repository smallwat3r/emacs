;;; sw-workspaces.el --- Workspace management -*- lexical-binding: t -*-

;;; Commentary:
;; Workspace management using built-in tab-bar-mode.
;; Inspired by Doom's workspace handling with persp-mode.

;;; Code:

;; Faces for workspace display
(defface sw/workspace-tab-face
  '((t (:inherit default)))
  "Face for inactive workspace tabs.")

(defface sw/workspace-tab-selected-face
  '((t (:inherit highlight)))
  "Face for the selected workspace tab.")

;; Core functions
(defun sw/fallback-buffer ()
  "Return the fallback buffer, creating it if necessary."
  (get-buffer-create "*scratch*"))

(defun sw/workspace--get-names ()
  "Return list of all workspace names from tab-bar.
Names may be nil for unnamed workspaces."
  (mapcar (lambda (tab) (alist-get 'name tab))
          (funcall tab-bar-tabs-function)))

(defun sw/workspace--format-tab (index name is-current)
  "Format a single tab for echo area display.
INDEX is the 0-based tab position, NAME is the workspace name (may be nil),
and IS-CURRENT indicates if this is the active workspace."
  (let* ((num (1+ index))
         (has-name (and name (not (string-empty-p name))))
         (label (if has-name (format "[%d] %s" num name) (number-to-string num)))
         (text (if is-current (format "(%s) " label) (format " %s  " label)))
         (face (if is-current 'sw/workspace-tab-selected-face 'sw/workspace-tab-face)))
    (propertize text 'face face)))

(defun sw/workspace--tabline ()
  "Return formatted workspace tabline for echo area display."
  (let* ((tabs (funcall tab-bar-tabs-function))
         (current-index (tab-bar--current-tab-index tabs)))
    (mapconcat
     (lambda (tab)
       (sw/workspace--format-tab
        (cl-position tab tabs)
        (alist-get 'name tab)
        (eq (cl-position tab tabs) current-index)))
     tabs
     "")))

;; Interactive commands
(defun sw/workspace-display ()
  "Display workspaces in the echo area."
  (interactive)
  (let (message-log-max)
    (message "%s" (sw/workspace--tabline))))

(defun sw/workspace-new ()
  "Create a new blank workspace.
Starts with only scratch buffer, single window, rooted at home directory."
  (interactive)
  (let ((tab-bar-new-tab-choice #'sw/fallback-buffer))
    (tab-bar-new-tab)
    (delete-other-windows)
    (switch-to-buffer (sw/fallback-buffer))
    (setq-local default-directory "~/")))

(defun sw/workspace-switch-to-project ()
  "Switch project, opening it in a dedicated workspace.
If a workspace for the project already exists, switch to it."
  (interactive)
  (let* ((dir (project-prompt-project-dir))
         (name (file-name-nondirectory (directory-file-name dir)))
         (existing (member name (sw/workspace--get-names))))
    (if existing
        (tab-bar-switch-to-tab name)
      (let* ((tab-bar-new-tab-choice #'sw/fallback-buffer)
             (pr (project-current nil dir))
             (root (if pr (project-root pr) dir))
             (default-directory root))
        (tab-bar-new-tab)
        (tab-bar-rename-tab name)
        (delete-other-windows)
        ;; Find file using project's file list if available
        (if pr
            (let* ((files (project-files pr))
                   (file (completing-read "Find file: " files nil t)))
              (find-file file))
          (call-interactively #'find-file))))))

(defun sw/workspace-find-in-directory (dir name)
  "Find file in DIR, switching to or creating workspace NAME."
  (let ((existing (member name (sw/workspace--get-names))))
    (if existing
        (tab-bar-switch-to-tab name)
      (let ((tab-bar-new-tab-choice #'sw/fallback-buffer))
        (tab-bar-new-tab)
        (tab-bar-rename-tab name)
        (delete-other-windows)))
    (let ((default-directory dir))
      (project-find-file))))

(defun sw/workspace-find-dotfiles ()
  "Find file in dotfiles directory, with dedicated workspace."
  (interactive)
  (sw/workspace-find-in-directory sw/dotfiles-directory "dotfiles"))

(defun sw/workspace-find-emacs-config ()
  "Find file in Emacs config directory, with dedicated workspace."
  (interactive)
  (sw/workspace-find-in-directory user-emacs-directory ".emacs.d"))

;; Display after workspace operations
(defvar sw/workspace--last-kill-count nil
  "Temporary storage for buffer kill count during workspace close.")

(defun sw/workspace--display-after (&rest _)
  "Display workspaces after tab operations."
  (sw/workspace-display))

(defun sw/workspace--display-after-close (&rest _)
  "Display workspaces after closing, including buffer kill count."
  (let ((count sw/workspace--last-kill-count))
    (setq sw/workspace--last-kill-count nil)
    (let (message-log-max)
      (message "%s | Closed workspace, killed %d buffer(s)"
               (sw/workspace--tabline) (or count 0)))))

(dolist (fn '(tab-bar-new-tab
              tab-bar-switch-to-tab
              tab-bar-switch-to-next-tab
              tab-bar-switch-to-prev-tab
              tab-bar-select-tab
              tab-bar-rename-tab))
  (advice-add fn :after #'sw/workspace--display-after))

(advice-add 'tab-bar-close-tab :after #'sw/workspace--display-after-close)

;; Tab-bar configuration
(setq tab-bar-show nil
      tab-bar-new-tab-choice #'sw/fallback-buffer
      tab-bar-tab-hints t
      tab-bar-tab-name-function (lambda () "")
      tab-bar-format '(tab-bar-format-tabs))
(tab-bar-mode 1)

;; Name the initial workspace "main" and show workspaces on startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (tab-bar-rename-tab "main")
            (sw/workspace-display)))

;; Buffer tracking per workspace
(defvar sw/workspace-buffer-alist nil
  "Alist mapping tab indices to their buffer lists.")

(defun sw/workspace--current-index ()
  "Return current workspace index (0-based)."
  (tab-bar--current-tab-index))

(defun sw/workspace--get-buffers ()
  "Return list of buffers for current workspace."
  (let ((idx (sw/workspace--current-index)))
    (alist-get idx sw/workspace-buffer-alist)))

(defun sw/workspace--add-buffer (buffer)
  "Add BUFFER to current workspace's buffer list."
  (let* ((idx (sw/workspace--current-index))
         (buffers (alist-get idx sw/workspace-buffer-alist)))
    (unless (memq buffer buffers)
      (setf (alist-get idx sw/workspace-buffer-alist)
            (cons buffer buffers)))))

(defun sw/workspace--remove-killed-buffer ()
  "Remove the current buffer from all workspace buffer lists.
Intended for use in `kill-buffer-hook'."
  (let ((buf (current-buffer)))
    (dolist (entry sw/workspace-buffer-alist)
      (setcdr entry (delq buf (cdr entry))))))

(add-hook 'kill-buffer-hook #'sw/workspace--remove-killed-buffer)

(defun sw/workspace--kill-buffers (buffers)
  "Kill BUFFERS, skipping scratch. Return count of killed buffers."
  (let ((count 0))
    (dolist (buf buffers)
      (when (and (buffer-live-p buf)
                 (not (eq buf (sw/fallback-buffer))))
        (kill-buffer buf)
        (cl-incf count)))
    count))

(defun sw/workspace--kill-buffers-on-close (&optional tab-number &rest _)
  "Kill buffers associated with the workspace being closed.
TAB-NUMBER is the 1-based tab number, or nil for current tab."
  (let* ((idx (if tab-number (1- tab-number) (sw/workspace--current-index)))
         (buffers (alist-get idx sw/workspace-buffer-alist)))
    (setq sw/workspace--last-kill-count (sw/workspace--kill-buffers buffers))
    (setq sw/workspace-buffer-alist
          (assq-delete-all idx sw/workspace-buffer-alist))))

(advice-add 'tab-bar-close-tab :before #'sw/workspace--kill-buffers-on-close)

(defun sw/workspace--track-buffer (&rest _)
  "Track current buffer in workspace buffer list.
Intended for use in `window-buffer-change-functions'.
Ignores minibuffers."
  (when-let ((buf (current-buffer)))
    (unless (minibufferp buf)
      (sw/workspace--add-buffer buf))))

(add-hook 'window-buffer-change-functions #'sw/workspace--track-buffer)

(defun sw/workspace-buffer-list ()
  "Return live buffers for current workspace."
  (seq-filter #'buffer-live-p (sw/workspace--get-buffers)))

(defun sw/workspace--read-buffer (buffers prompt)
  "Select a buffer from BUFFERS with PROMPT and preview."
  (require 'consult)
  (consult--read
   (mapcar #'buffer-name buffers)
   :prompt prompt
   :category 'buffer
   :state (consult--buffer-state)
   :require-match t))

(defun sw/workspace--find-buffer-workspace (buffer)
  "Return workspace index containing BUFFER, or nil if not found."
  (let ((buf (get-buffer buffer)))
    (cl-loop for (idx . buffers) in sw/workspace-buffer-alist
             when (memq buf buffers)
             return idx)))

(defun sw/workspace-kill-all-buffers ()
  "Kill all buffers in the current workspace.
Switches to scratch buffer after killing."
  (interactive)
  (let ((buffers (sw/workspace-buffer-list)))
    (when (yes-or-no-p (format "Kill %d buffer(s) in this workspace? " (length buffers)))
      (let ((count (sw/workspace--kill-buffers buffers)))
        (switch-to-buffer (sw/fallback-buffer))
        (delete-other-windows)
        (message "Killed %d buffer(s)" count)))))

(defun sw/workspace-switch-buffer ()
  "Switch to a buffer in the current workspace with preview."
  (interactive)
  (let ((workspace-buffers (sw/workspace-buffer-list)))
    (if workspace-buffers
        (when-let ((buf (sw/workspace--read-buffer workspace-buffers
                                                   "Switch to buffer: ")))
          (switch-to-buffer buf))
      (call-interactively #'consult-buffer))))

(defun sw/switch-buffer-global ()
  "Switch to any buffer, switching workspace if needed."
  (interactive)
  (let* ((all-buffers (cl-remove-if
                       (lambda (b) (string-prefix-p " " (buffer-name b)))
                       (buffer-list)))
         (buf (sw/workspace--read-buffer all-buffers "Switch to buffer (global): ")))
    (when buf
      (when-let ((workspace-idx (sw/workspace--find-buffer-workspace buf)))
        (unless (= workspace-idx (sw/workspace--current-index))
          (tab-bar-select-tab (1+ workspace-idx))))
      (switch-to-buffer buf))))

;; Generates `sw/workspace-switch-to-1' through `sw/workspace-switch-to-9'.
;; Each function switches to the corresponding workspace index, or displays
;; an error if the workspace does not exist.
(defmacro sw/workspace--define-switchers ()
  "Define workspace switching functions 1-9."
  `(progn
     ,@(mapcar (lambda (n)
                 `(defun ,(intern (format "sw/workspace-switch-to-%d" n)) ()
                    ,(format "Switch to workspace %d." n)
                    (interactive)
                    (if (<= ,n (length (tab-bar-tabs)))
                        (tab-bar-select-tab ,n)
                      (message "Workspace %d does not exist" ,n))))
               (number-sequence 1 9))))

(sw/workspace--define-switchers)

(provide 'sw-workspaces)
;;; sw-workspaces.el ends here
