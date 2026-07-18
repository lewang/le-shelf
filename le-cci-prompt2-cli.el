;;; le-cci-prompt2-cli.el --- CCI prompt editor: Claude CLI handoff  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Le Wang
;; Keywords: tools
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The C-M-g temp-file handoff for the Claude Code prompt composer (see
;; `le-cci-prompt2').  When the CLI's edit-in-$EDITOR flow hands a
;; claude-prompt-*.md temp file to the Emacs server, redirect editing into
;; `le::cci-prompt2-edit' (autoloaded) instead of editing the temp file
;; directly, and forward a real C-g to vterm/ghostel terminals to trigger
;; that flow in the first place.

;;; Code:

(declare-function le::cci-prompt2-edit "le-cci-prompt2")
(declare-function vterm-send-key "vterm")
(declare-function ghostel-send-C-g "ghostel")
(declare-function server-edit "server")

(defvar claude-code-ide-mcp-server--sessions)
(defvar claude-code-ide--routing-tokens)
(defvar server-buffer-clients)
(defvar server-client-instructions)

(defun le::cci-prompt2--session-buffer-for-client ()
  "Return the CCI session buffer for the emacsclient connection that
opened the current buffer, resolved via the connecting client's
working directory, or nil.
`emacsclient' sends its cwd as a `-dir' protocol command (so relative
filenames resolve correctly); `server.el' stores it as the
`server-client-directory' process property.  That directory is
matched against `claude-code-ide--routing-tokens' the same way
`le::cci-prompt2--dir-for-cci-buffer' does, even though the temp file
itself carries no session-identifying information."
  (when-let* ((client (car server-buffer-clients))
              (dir (process-get client 'server-client-directory))
              (token (gethash dir claude-code-ide--routing-tokens))
              (session (gethash token claude-code-ide-mcp-server--sessions))
              (buf (plist-get session :buffer))
              ((buffer-live-p buf)))
    buf))

(defun le::cci-prompt2--server-edit-with-message (message-text)
  "Call `server-edit', showing MESSAGE-TEXT instead of its own \"When
done...\" hint for this connection.
`server-visit-files' always prints that hint *after*
`server-switch-hook' (and thus this function) returns, so a plain
`message' called from inside the hook -- or even just after this call
returns -- is always clobbered by it; suppress it for this connection
via `server-client-instructions', then queue a timer to fire once the
current command finishes (guaranteed to run after that clobbering
print) to show MESSAGE-TEXT and restore `server-client-instructions'."
  (let ((orig server-client-instructions))
    (setq server-client-instructions nil)
    (run-at-time 0 nil (lambda ()
                          (setq server-client-instructions orig)
                          (message "%s" message-text))))
  (server-edit))

;;;###autoload
(defun le::cci-prompt2-claude-prompt-file-setup ()
  "When visiting a Claude CLI prompt temp file, redirect editing to
`le::cci-prompt2-edit' instead of editing the temp file directly.

Captures the file's current text, opens the prompt edit buffer
prefilled with it (targeting the CCI session identified via
`le::cci-prompt2--session-buffer-for-client', falling back to
`le::cci-prompt2-edit''s own resolution when that's unavailable),
then blanks and saves the temp file (so the CLI resumes with an empty
prompt) and completes the emacsclient session for this buffer
\(equivalent to \\[server-edit]).  The blanking happens only once the
edit buffer exists: if `le::cci-prompt2-edit' aborts (say, no
.le-playground/ in the project), the CLI's text is still in the temp
file rather than gone.

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
           (cci-buf (le::cci-prompt2--session-buffer-for-client))
           (text (buffer-string)))
      ;; Focus the CCI window before opening the editor: emacsclient just
      ;; popped this temp file into the selected window, but the real origin
      ;; of a C-M-g is the session buffer.  `le::cci-prompt2-edit' captures
      ;; the selected window as the spot to restore focus to on exit, so
      ;; selecting the session here is what makes that generic restore land
      ;; back on Claude -- no return-buffer plumbing in commit/cancel.  This
      ;; is also why the edit is opened before the temp file is blanked
      ;; (below): were `le::cci-prompt2-edit' to abort, the CLI's text would
      ;; still be in the temp file rather than gone.  Its return value isn't
      ;; needed -- the edit buffer captures its own target session into state.
      (when-let* ((cci-buf)
                  (win (get-buffer-window cci-buf)))
        (select-window win))
      (if cci-buf
          (with-current-buffer cci-buf
            (le::cci-prompt2-edit nil text))
        (le::cci-prompt2-edit nil text))
      (with-current-buffer prompt-buf
        (erase-buffer)
        (save-buffer))
      (when (buffer-live-p prompt-buf)
        (with-current-buffer prompt-buf
          (le::cci-prompt2--server-edit-with-message
           "Editing with le::cci-prompt2-edit"))))))

;;;###autoload
(add-hook 'server-switch-hook #'le::cci-prompt2-claude-prompt-file-setup)

;;;###autoload
(defun le::cci-prompt2-vterm-send-C-g ()
  "Send a real C-g to the vterm process (e.g. to trigger Claude CLI's
edit-in-$EDITOR flow).  v2 prompts always open a fresh heading, so
there is no draft-clobbering hazard to guard against (v1's guarded
command needed one because it reused a single draft buffer; removed
at 2f0617e)."
  (interactive)
  (vterm-send-key (kbd "C-g")))

;;;###autoload
(defun le::cci-prompt2-ghostel-send-C-g ()
  "Forward a real C-g to the ghostel terminal.
See `le::cci-prompt2-vterm-send-C-g' for why no draft guard is needed."
  (interactive)
  (ghostel-send-C-g))

(provide 'le-cci-prompt2-cli)
;;; le-cci-prompt2-cli.el ends here
