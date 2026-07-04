;;; le-cci.el --- Claude Code IDE utility functions -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Le Wang
;; Keywords: tools
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Utility functions for Claude Code IDE integration: vterm helpers
;; and prompt file setup.

;;; Code:

(declare-function le::cci-edit-prompt "le-cci-edit-prompt" (force-choose &optional initial-text))
(defvar claude-code-ide-mcp-server--sessions)
(defvar claude-code-ide--routing-tokens)
(defvar le::cci--return-buffer)

;;;###autoload
(defun le::vterm-send-C-g ()
  "Send C-g to the vterm process."
  (interactive)
  (vterm-send-key (kbd "C-g")))

(defun le::cci--blank-prompt-file-and-capture ()
  "Capture the current buffer's text, then erase and save it to disk.
Returns the captured text.  Used to hand off a Claude CLI temp-file
prompt to `le::cci-edit-prompt' while leaving the temp file blank for
the CLI to re-read."
  (let ((text (buffer-string)))
    (erase-buffer)
    (save-buffer)
    text))

(defun le::cci--session-buffer-for-client ()
  "Return the CCI session buffer for the emacsclient connection that
opened the current buffer, resolved via the connecting client's
working directory, or nil.
`emacsclient' sends its cwd as a `-dir' protocol command (so relative
filenames resolve correctly); `server.el' stores it as the
`server-client-directory' process property.  That directory is
matched against `claude-code-ide--routing-tokens' the same way
`le::cci--dir-for-cci-buffer' does, even though the temp file itself
carries no session-identifying information."
  (when-let* ((client (car server-buffer-clients))
              (dir (process-get client 'server-client-directory))
              (token (gethash dir claude-code-ide--routing-tokens))
              (session (gethash token claude-code-ide-mcp-server--sessions))
              (buf (plist-get session :buffer))
              ((buffer-live-p buf)))
    buf))

(defun le::cci--server-edit-quietly ()
  "Call `server-edit', suppressing its \"When done...\" hint for just
this connection rather than via `server-client-instructions' globally.
A plain dynamic `let' cannot scope this: `server-visit-files' prints
the hint *after* `server-switch-hook' (and thus this function) returns,
so the mutation has to outlive our call and be undone later instead --
a timer fired once the current command finishes restores it."
  (let ((orig server-client-instructions))
    (setq server-client-instructions nil)
    (run-at-time 0 nil (lambda () (setq server-client-instructions orig))))
  (server-edit))

;;;###autoload
(defun le::claude-prompt-file-setup ()
  "When visiting a Claude CLI prompt temp file, redirect editing to
`le::cci-edit-prompt' instead of editing the temp file directly.

If a `*cci-prompt: ...*' buffer for the owning CCI session (identified
via `le::cci--session-buffer-for-client') is already open, switches to
it and leaves both it and the temp file untouched -- the CLI's own
prompt is not cleared, since there's nowhere for its text to go
without clobbering the existing draft.

Otherwise: captures the file's current text, blanks and saves the file
(so the CLI resumes with an empty prompt), and opens the CCI
prompt-editing buffer prefilled with the captured text (targeting the
owning session, falling back to `le::cci-edit-prompt''s own resolution
when `le::cci--session-buffer-for-client' can't determine it).

Either way, completes the emacsclient session for this buffer
(equivalent to \\[server-edit]).

Installed on `server-switch-hook' rather than `server-visit-hook':
per `server-visit-files'/`server-execute' in server.el,
`server-buffer-clients' (needed to read the connecting client's
working directory) is only populated *after* `server-visit-hook' runs,
but `server-switch-hook' fires later, once the client is fully
registered.

Should have this setting: (setq server-window \\='pop-to-buffer)"
  (when (and (buffer-file-name)
             (string-match-p "^claude-prompt-.*\\.md$"
                             (file-name-nondirectory (buffer-file-name))))
    (let* ((prompt-buf (current-buffer))
           (cci-buf (le::cci--session-buffer-for-client))
           (existing-editor-buf
            (when cci-buf
              (get-buffer (format "*cci-prompt: %s*" (buffer-name cci-buf))))))
      (if existing-editor-buf
          (progn
            (pop-to-buffer existing-editor-buf)
            (message "CCI prompt buffer for %s already open; leaving CLI text in place"
                     (buffer-name cci-buf)))
        (let* ((text (le::cci--blank-prompt-file-and-capture))
               (editor-buf (condition-case err
                               (if cci-buf
                                   (with-current-buffer cci-buf
                                     (le::cci-edit-prompt nil text))
                                 (le::cci-edit-prompt nil text))
                             (user-error
                              (message "%s" (error-message-string err))
                              nil))))
          (when (and cci-buf (buffer-live-p editor-buf))
            (with-current-buffer editor-buf
              (setq-local le::cci--return-buffer cci-buf)))))
      (when (buffer-live-p prompt-buf)
        (with-current-buffer prompt-buf
          (le::cci--server-edit-quietly))))))

;;;###autoload
(add-hook 'server-switch-hook #'le::claude-prompt-file-setup)

(provide 'le-cci)
;;; le-cci.el ends here
