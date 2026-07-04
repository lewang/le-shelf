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

(defun le::cci--prompt-buffer-open-p ()
  "Return non-nil if a CCI prompt-editing buffer for the current CCI
session buffer is already open with a non-empty draft.
An empty compose buffer has nothing to protect, so it doesn't count --
Claude CLI's captured text is free to populate it as normal.  Called
from a CCI session buffer (vterm or ghostel), so the session's own
buffer name is all `le::cci-edit-prompt' needs to compute its compose
buffer's name."
  (when-let* ((buf (get-buffer (format "*cci-prompt: %s*" (buffer-name)))))
    (> (buffer-size buf) 0)))

;;;###autoload
(defun le::vterm-send-C-g ()
  "Send C-g to the vterm process.
Errors instead when a CCI prompt-editing buffer for this session is
already open: Claude CLI's own edit-in-$EDITOR flow (triggered by the
C-g this sends) would hand its captured text to that buffer, but
`le::cci-edit-prompt' ignores new text for an existing buffer to avoid
clobbering its draft -- so sending C-g here would just silently
discard whatever's currently typed into the CLI's prompt."
  (interactive)
  (if (le::cci--prompt-buffer-open-p)
      (user-error "CCI prompt buffer for %s already open" (buffer-name))
    (vterm-send-key (kbd "C-g"))))

(declare-function ghostel-send-C-g "ghostel")

;;;###autoload
(defun le::ghostel-send-C-g ()
  "Like `ghostel-send-C-g', but errors instead of forwarding C-g to the
CLI when a CCI prompt-editing buffer for this session is already open.
See `le::vterm-send-C-g' for why."
  (interactive)
  (if (le::cci--prompt-buffer-open-p)
      (user-error "CCI prompt buffer for %s already open" (buffer-name))
    (ghostel-send-C-g)))

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

;;;###autoload
(defun le::claude-prompt-file-setup ()
  "When visiting a Claude CLI prompt temp file, redirect editing to
`le::cci-edit-prompt' instead of editing the temp file directly.

Captures the file's current text, blanks and saves the file (so the
CLI resumes with an empty prompt), opens the CCI prompt-editing buffer
prefilled with the captured text (targeting the CCI session identified
via `le::cci--session-buffer-for-client', falling back to
`le::cci-edit-prompt''s own resolution when that's unavailable), then
completes the emacsclient session for this buffer (equivalent to
\\[server-edit]).

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
           (text (le::cci--blank-prompt-file-and-capture))
           (editor-buf (if cci-buf
                           (with-current-buffer cci-buf
                             (le::cci-edit-prompt nil text))
                         (le::cci-edit-prompt nil text))))
      (when (and cci-buf (buffer-live-p editor-buf))
        (with-current-buffer editor-buf
          (setq-local le::cci--return-buffer cci-buf)))
      (when (buffer-live-p prompt-buf)
        (with-current-buffer prompt-buf
          (server-edit))))))

;;;###autoload
(add-hook 'server-switch-hook #'le::claude-prompt-file-setup)

(provide 'le-cci)
;;; le-cci.el ends here
