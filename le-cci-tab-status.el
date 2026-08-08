;;; le-cci-tab-status.el --- Tab-bar status indicators for Claude Code sessions -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Le Wang
;; Keywords: tools
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Prepends a status indicator (⚡ pending input, ✅ finished) to a
;; workspace's tab-bar tab name for Claude Code IDE sessions.
;;
;; Indicators are set from external hook scripts via emacsclient
;; (`le::cci-tab-input-pending', `le::cci-tab-finished').  An indicator is
;; cleared by any of: switching to the tab, the first input the CCI buffer
;; receives (key/scroll/mouse), an explicit `le::cci-tab-clear', or stale
;; expiry.  `le::cci-tab-status-mode' is the global on/off switch.

;;; Code:

(defgroup le-cci-tab-status nil
  "Tab-bar status indicators for Claude Code sessions."
  :group 'tools
  :prefix "le::cci-tab-")

(declare-function claude-code-ide--buffer-session "claude-code-ide" (&optional buffer))
(declare-function claude-code-ide-mcp--active-sessions "claude-code-ide-mcp" ())
(declare-function claude-code-ide-mcp--sessions-for-project "claude-code-ide-mcp" (project-dir))
(declare-function claude-code-ide-mcp--mru-session "claude-code-ide-mcp" (project-dir))
(declare-function claude-code-ide-mcp--get-session-by-id "claude-code-ide-mcp" (session-id))
(declare-function claude-code-ide-mcp-session-buffer "claude-code-ide-mcp" (session))
(declare-function claude-code-ide-mcp-session-project-dir "claude-code-ide-mcp" (session))
(declare-function claude-code-ide-mcp-session-session-id "claude-code-ide-mcp" (session))
(declare-function claude-code-ide-mcp-session-window-slot "claude-code-ide-mcp" (session))
(declare-function w--find-workspace-for-root "w" (root))
(declare-function w--find-tab "w" (name))
(declare-function w--find-workspace "w" (name))

(defvar le::cci-tab-pending-indicator "⚡ "
  "String prepended to tab name when input is pending.")

(defvar le::cci-tab-finished-indicator "✅ "
  "String prepended to tab name when Claude has finished.")

(defvar le::cci-tab-finished-max-age 3600
  "Seconds after which a stale ✅ indicator is automatically cleared.")

(defvar le::cci-tab--finished-times (make-hash-table :test 'equal)
  "Maps session id to the `float-time' when ✅ was set.")

(defvar le::cci-tab--states (make-hash-table :test 'equal)
  "Maps session id to its indicator state: `pending', `finished', or absent.
Keyed per instance rather than per project, since a project may run
several and each carries its own state.")

(defun le::cci-tab--find-tab (project-dir)
  "Find the tab and frame for PROJECT-DIR via w workspace.
Returns (TAB . FRAME) or nil."
  (when (fboundp 'w--find-workspace-for-root)
    (when-let* ((ws (w--find-workspace-for-root project-dir))
                (name (plist-get ws :name)))
      (w--find-tab name))))

(defun le::cci-tab--strip-indicators (name)
  "Return NAME with any leading run of status indicators removed."
  (let ((changed t))
    (while changed
      (setq changed nil)
      (dolist (prefix (list le::cci-tab-pending-indicator
                            le::cci-tab-finished-indicator))
        (when (string-prefix-p prefix name)
          (setq name (substring name (length prefix))
                changed t))))
    name))

(defun le::cci-tab--indicator-string (project-dir)
  "Return the indicator prefix for PROJECT-DIR: one icon per live instance.
Instances are ordered by their side-window slot, which is assigned once
at creation and ascends within a project -- unlike the session table,
which is explicitly unordered, so icons would otherwise swap places
between renders.  Instances with no state contribute nothing."
  (let* ((sessions (claude-code-ide-mcp--sessions-for-project project-dir))
         (ordered (sort (copy-sequence sessions)
                        (lambda (a b)
                          (< (or (claude-code-ide-mcp-session-window-slot a) 0)
                             (or (claude-code-ide-mcp-session-window-slot b) 0)))))
         (icons (delq nil
                      (mapcar
                       (lambda (session)
                         (pcase (gethash (claude-code-ide-mcp-session-session-id session)
                                         le::cci-tab--states)
                           ('pending le::cci-tab-pending-indicator)
                           ('finished le::cci-tab-finished-indicator)
                           (_ nil)))
                       ordered))))
    (apply #'concat icons)))

(defun le::cci-tab--refresh (project-dir)
  "Rebuild the tab name for PROJECT-DIR from its instances' states."
  (when-let* ((found (le::cci-tab--find-tab project-dir))
              (tab (car found))
              (frame (cdr found)))
    (let* ((current-name (alist-get 'name (cdr tab)))
           (clean-name (le::cci-tab--strip-indicators current-name))
           (new-name (concat (le::cci-tab--indicator-string project-dir)
                             clean-name)))
      (unless (string= current-name new-name)
        (let* ((tabs (funcall tab-bar-tabs-function frame))
               (idx (1+ (seq-position tabs tab #'eq))))
          (with-selected-frame frame
            (tab-bar-rename-tab new-name idx)))
        (force-mode-line-update t)))))

(defun le::cci-tab--resolve-session (project-dir session-id)
  "Return the session SESSION-ID names, or PROJECT-DIR's most recent one.
The fallback keeps a hook that predates the session id working: it
behaves as it always did, marking the project's current instance."
  (or (and session-id
           (not (equal session-id "UNSET"))
           (claude-code-ide-mcp--get-session-by-id session-id))
      (claude-code-ide-mcp--mru-session project-dir)))

(defun le::cci-tab--set-state (project-dir session-id state)
  "Record STATE for the instance SESSION-ID names and repaint PROJECT-DIR.
STATE is `pending', `finished', or nil to clear.  Returns the session."
  (let ((session (le::cci-tab--resolve-session project-dir session-id)))
    (when session
      (let ((id (claude-code-ide-mcp-session-session-id session)))
        (if state
            (puthash id state le::cci-tab--states)
          (remhash id le::cci-tab--states))
        (if (eq state 'finished)
            (puthash id (float-time) le::cci-tab--finished-times)
          (remhash id le::cci-tab--finished-times))))
    (le::cci-tab--prune-dead-sessions)
    (le::cci-tab--refresh project-dir)
    session))

(defun le::cci-tab--prune-dead-sessions ()
  "Drop state for instances that are no longer running.
Without this a stopped instance would keep its icon on the tab forever,
since nothing else revisits its id."
  (let ((live (mapcar #'claude-code-ide-mcp-session-session-id
                      (claude-code-ide-mcp--active-sessions)))
        dead)
    (maphash (lambda (id _state)
               (unless (member id live) (push id dead)))
             le::cci-tab--states)
    (dolist (id dead)
      (remhash id le::cci-tab--states)
      (remhash id le::cci-tab--finished-times))))

(defun le::cci-tab--normalize-dir (project-dir)
  "Return PROJECT-DIR as a canonical absolute directory name (trailing slash).
External callers (e.g. the emacsclient hook passing CLAUDE_PROJECT_DIR) may
supply a slash-less path; Emacs directory names conventionally end in \"/\"
\(see `default-directory'), so coerce to that form for consistent hash keys
and lookups."
  (file-name-as-directory (expand-file-name project-dir)))

(defun le::cci-tab--clear-on-input ()
  "One-shot `pre-command-hook': clear this instance's icon on any input.
Removes itself first so it fires once and never aborts the user's
command.  Resolves its own session from the buffer, so typing in one
instance leaves its siblings' icons alone."
  (remove-hook 'pre-command-hook #'le::cci-tab--clear-on-input t)
  (with-demoted-errors "le::cci-tab--clear-on-input: %S"
    (when-let* ((session (claude-code-ide--buffer-session (current-buffer))))
      (le::cci-tab-clear (claude-code-ide-mcp-session-project-dir session)
                         (claude-code-ide-mcp-session-session-id session)))))

(defun le::cci-tab--arm-input-clear (session)
  "Arm a one-shot input-clear on SESSION's terminal buffer."
  (when-let* ((buf (and session (claude-code-ide-mcp-session-buffer session)))
              ((buffer-live-p buf)))
    (with-current-buffer buf
      ;; `add-hook' with a named function is idempotent across repeated sets.
      (add-hook 'pre-command-hook #'le::cci-tab--clear-on-input nil t))))

;;; Public API (called from hook scripts via emacsclient --eval)

;;;###autoload
(defun le::cci-tab-input-pending (project-dir &optional session-id)
  "Mark the instance SESSION-ID as needing user input.
PROJECT-DIR selects the tab.  SESSION-ID is optional so a hook that
predates it still works, marking the project's most recent instance."
  (let* ((project-dir (le::cci-tab--normalize-dir project-dir))
         (session (le::cci-tab--set-state project-dir session-id 'pending)))
    (when le::cci-tab-status-mode
      (le::cci-tab--arm-input-clear session))))

;;;###autoload
(defun le::cci-tab-finished (project-dir &optional session-id)
  "Mark the instance SESSION-ID as finished.
PROJECT-DIR selects the tab.  SESSION-ID is optional so a hook that
predates it still works, marking the project's most recent instance."
  (let ((project-dir (le::cci-tab--normalize-dir project-dir)))
    (le::cci-tab--expire-stale-indicators)
    (let ((session (le::cci-tab--set-state project-dir session-id 'finished)))
      (when le::cci-tab-status-mode
        (le::cci-tab--arm-input-clear session)))))

;;;###autoload
(defun le::cci-tab-clear (project-dir &optional session-id)
  "Clear the icon for the instance SESSION-ID names on PROJECT-DIR's tab.
Without SESSION-ID, clears every instance of the project."
  (let ((project-dir (le::cci-tab--normalize-dir project-dir)))
    (if session-id
        (le::cci-tab--set-state project-dir session-id nil)
      (dolist (session (claude-code-ide-mcp--sessions-for-project project-dir))
        (let ((id (claude-code-ide-mcp-session-session-id session)))
          (remhash id le::cci-tab--states)
          (remhash id le::cci-tab--finished-times)))
      (le::cci-tab--refresh project-dir))))

(defun le::cci-tab--expire-stale-indicators ()
  "Clear ✅ icons unvisited for more than `le::cci-tab-finished-max-age'."
  (let ((now (float-time))
        stale)
    (maphash (lambda (id set-time)
               (when (> (- now set-time) le::cci-tab-finished-max-age)
                 (push id stale)))
             le::cci-tab--finished-times)
    (dolist (id stale)
      (when-let* ((session (claude-code-ide-mcp--get-session-by-id id)))
        (le::cci-tab-clear (claude-code-ide-mcp-session-project-dir session) id)))))

;;; Auto-dismiss finished indicator on tab select

(defun le::cci-tab--on-tab-select (_prev-tab new-tab)
  "Clear any status indicator when user selects a tab."
  (when-let* ((name (alist-get 'name (cdr new-tab)))
              ((or (string-prefix-p le::cci-tab-finished-indicator name)
                   (string-prefix-p le::cci-tab-pending-indicator name)))
              (ws-name (alist-get 'w-workspace (cdr new-tab))))
    (when-let* ((ws (w--find-workspace ws-name))
                (project-dir (plist-get ws :project-root)))
      (le::cci-tab-clear project-dir))))

;;;###autoload
(define-minor-mode le::cci-tab-status-mode
  "Toggle tab bar status indicators for Claude Code sessions.
When enabled, selecting a tab clears its ⚡/✅ indicator."
  :global t
  :group 'le-cci-tab-status
  (if le::cci-tab-status-mode
      (add-hook 'tab-bar-tab-post-select-functions #'le::cci-tab--on-tab-select)
    (remove-hook 'tab-bar-tab-post-select-functions #'le::cci-tab--on-tab-select)))

(provide 'le-cci-tab-status)
;;; le-cci-tab-status.el ends here
