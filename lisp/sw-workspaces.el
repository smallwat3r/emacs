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
  "Return list of all workspace names."
  (mapcar (lambda (tab) (alist-get 'name tab))
          (funcall tab-bar-tabs-function)))

(defun sw/workspace--format-tab (index name is-current)
  "Format a single tab with INDEX, NAME, and IS-CURRENT status."
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
        ;; Find file using project's file list directly
        (let* ((files (project-files pr))
               (file (completing-read "Find file: " files nil t)))
          (find-file file))))))

;; Display after workspace operations
(defun sw/workspace--display-after (&rest _)
  "Display workspaces after tab operations."
  (sw/workspace-display))

(dolist (fn '(tab-bar-new-tab
              tab-bar-switch-to-tab
              tab-bar-switch-to-next-tab
              tab-bar-switch-to-prev-tab
              tab-bar-select-tab
              tab-bar-close-tab
              tab-bar-rename-tab))
  (advice-add fn :after #'sw/workspace--display-after))

;; Tab-bar configuration
(setq tab-bar-show nil
      tab-bar-new-tab-choice #'sw/fallback-buffer
      tab-bar-tab-hints t
      tab-bar-tab-name-function (lambda () "")
      tab-bar-format '(tab-bar-format-tabs))
(tab-bar-mode 1)

;; Show workspaces on startup
(add-hook 'emacs-startup-hook #'sw/workspace-display)

;; Buffer tracking per workspace
(defvar sw/workspace-buffer-alist nil
  "Alist mapping tab indices to their buffer lists.")

(defun sw/workspace--current-index ()
  "Return current workspace index."
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

(defun sw/workspace--track-buffer (&rest _)
  "Track current buffer in workspace."
  (when-let ((buf (current-buffer)))
    (unless (minibufferp buf)
      (sw/workspace--add-buffer buf))))

(add-hook 'window-buffer-change-functions #'sw/workspace--track-buffer)

(defun sw/workspace-buffer-list ()
  "Return live buffers for current workspace."
  (seq-filter #'buffer-live-p (sw/workspace--get-buffers)))

(defun sw/workspace-switch-buffer ()
  "Switch to a buffer in the current workspace."
  (interactive)
  (let* ((workspace-buffers (sw/workspace-buffer-list))
         (buffer-names (mapcar #'buffer-name workspace-buffers)))
    (if buffer-names
        (switch-to-buffer
         (completing-read "Switch to buffer: " buffer-names nil t))
      (call-interactively #'consult-buffer))))

(defun sw/switch-buffer-global ()
  "Switch to any buffer (all workspaces)."
  (interactive)
  (consult-buffer))

(provide 'sw-workspaces)
;;; sw-workspaces.el ends here
