;;; le-cci.el --- Claude Code IDE utility functions -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Le Wang
;; Keywords: tools
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Utility functions for Claude Code IDE integration: vterm helpers,
;; server focus management, and prompt file setup.

;;; Code:

;;;###autoload
(defun le::vterm-send-C-g ()
  "Send C-g to the vterm process."
  (interactive)
  (vterm-send-key (kbd "C-g")))

;;;###autoload
(defun le::server-focus-on-claude-code-ide-on-exit ()
  "Focus on the vterm window after server-done.
Used as a `server-done-hook' to return to the CCI terminal."
  (when-let* ((vterm-window
              (get-window-with-predicate
               (lambda (window)
                 (with-current-buffer (window-buffer window)
                   (eq major-mode 'vterm-mode))))))
    (select-window vterm-window)))

;;;###autoload
(defun le::claude-prompt-file-setup ()
  "Setup buffer-local hooks for Claude prompt files.

Auto-return to vterm window after editing Claude prompt files.

Should have this setting: (setq server-window \\='pop-to-buffer)"
  (when (and (buffer-file-name)
             (string-match-p "^claude-prompt-.*\\.md$"
                             (file-name-nondirectory (buffer-file-name))))
    (add-hook 'server-done-hook 'le::server-focus-on-claude-code-ide-on-exit nil t)))

;;; Tab bar status indicators for Claude Code sessions

(defvar le::cci-tab-pending-indicator "⚡ "
  "String prepended to tab name when input is pending.")

(defvar le::cci-tab-finished-indicator "✅ "
  "String prepended to tab name when Claude has finished.")

(defvar le::cci-tab-finished-max-age 3600
  "Seconds after which a stale ✅ indicator is automatically cleared.")

(defvar le::cci-tab--finished-times (make-hash-table :test 'equal)
  "Maps project-dir to the `float-time' when ✅ was set.")

(declare-function w--find-workspace-for-root "w" (root))
(declare-function w--find-tab "w" (name))
(declare-function w--find-workspace "w" (name))

(defun le::cci-tab--find-tab (project-dir)
  "Find the tab and frame for PROJECT-DIR via w workspace.
Returns (TAB . FRAME) or nil."
  (when (fboundp 'w--find-workspace-for-root)
    (when-let* ((ws (w--find-workspace-for-root project-dir))
                (name (plist-get ws :name)))
      (w--find-tab name))))

(defun le::cci-tab--set-indicator (project-dir indicator)
  "Set INDICATOR prefix on the tab for PROJECT-DIR.
INDICATOR is a string prefix, or nil to clear."
  (when-let* ((found (le::cci-tab--find-tab project-dir))
              (tab (car found))
              (frame (cdr found)))
    (let* ((current-name (alist-get 'name (cdr tab)))
           (clean-name current-name))
      ;; Strip any existing indicator
      (dolist (prefix (list le::cci-tab-pending-indicator
                            le::cci-tab-finished-indicator))
        (when (string-prefix-p prefix clean-name)
          (setq clean-name (substring clean-name (length prefix)))))
      ;; Build new name
      (let ((new-name (if indicator (concat indicator clean-name) clean-name)))
        (unless (string= current-name new-name)
          (let* ((tabs (funcall tab-bar-tabs-function frame))
                 (idx (1+ (seq-position tabs tab #'eq))))
            (with-selected-frame frame
              (tab-bar-rename-tab new-name idx)))
          (force-mode-line-update t))))))

;;; Public API (called from hook scripts via emacsclient --eval)

;;;###autoload
(defun le::cci-tab-input-pending (project-dir)
  "Mark the tab for PROJECT-DIR as needing user input."
  (le::cci-tab--set-indicator project-dir le::cci-tab-pending-indicator))

;;;###autoload
(defun le::cci-tab-finished (project-dir)
  "Mark the tab for PROJECT-DIR as finished."
  (le::cci-tab--expire-stale-indicators)
  (puthash project-dir (float-time) le::cci-tab--finished-times)
  (le::cci-tab--set-indicator project-dir le::cci-tab-finished-indicator))

;;;###autoload
(defun le::cci-tab-clear (project-dir)
  "Clear any indicator from the tab for PROJECT-DIR."
  (remhash project-dir le::cci-tab--finished-times)
  (le::cci-tab--set-indicator project-dir nil))

(defun le::cci-tab--expire-stale-indicators ()
  "Clear ✅ indicators that have been unvisited for more than `le::cci-tab-finished-max-age' seconds."
  (let ((now (float-time))
        stale)
    (maphash (lambda (dir set-time)
               (when (> (- now set-time) le::cci-tab-finished-max-age)
                 (push dir stale)))
             le::cci-tab--finished-times)
    (dolist (dir stale)
      (le::cci-tab-clear dir))))

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
  :group 'le-cci
  (if le::cci-tab-status-mode
      (add-hook 'tab-bar-tab-post-select-functions #'le::cci-tab--on-tab-select)
    (remove-hook 'tab-bar-tab-post-select-functions #'le::cci-tab--on-tab-select)))

(provide 'le-cci)
;;; le-cci.el ends here
