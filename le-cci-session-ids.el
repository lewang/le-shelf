;;; le-cci-session-ids.el --- CLI conversation ids per Claude Code instance  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Le Wang
;; Keywords: tools
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Records which of Claude Code's own conversations each cci terminal has been
;; on, so consumers can scope per instance rather than per project.
;;
;; Two id namespaces meet here and they are easy to confuse.  The cci session
;; id (`claude-code-ide-mcp-session-session-id', exported as
;; CLAUDE_CODE_IDE_SESSION_ID) is minted by Emacs and names an INSTANCE.  The
;; CLI mints its own conversation uuid, which names a TRANSCRIPT: the
;; `sessionId' field in ~/.claude/history.jsonl and the basename of
;; ~/.claude/projects/<slug>/<uuid>.jsonl.
;;
;; Emacs cannot observe the latter.  It is not passed on the command line, the
;; CLI holds no file descriptor on its transcript, the ~/.claude/ide lock file
;; carries only Emacs's own pid, and the `ide_connected' notification reports
;; only the CLI's pid.  A hook is the one place both ids exist -- its stdin
;; carries the CLI's, its environment carries Emacs's -- so
;; ~/.claude/bin/cci-session-id declares the pair through emacsclient.
;;
;; That hook fires on SessionStart, which re-fires on /clear and on resume, so
;; an instance accumulates a LIST rather than holding a field: one terminal is
;; one instance but many conversations over its life.  Resume is also why the
;; declaration has to come from the CLI at all -- `claude -r' wraps a new
;; instance around a conversation the user picks in the CLI's own picker, which
;; Emacs never sees.

;;; Code:

(declare-function claude-code-ide-mcp--get-session-by-id "claude-code-ide-mcp" (session-id))
(declare-function claude-code-ide-mcp-session-buffer "claude-code-ide-mcp" (session))

(defvar-local le::cci-session-ids nil
  "Claude Code CONVERSATION uuids this terminal has been on, newest first.

These are the CLI's own ids -- transcript basenames under
~/.claude/projects/ and the `sessionId' field in ~/.claude/history.jsonl
-- and NOT `claude-code-ide-mcp-session-session-id', which names the
Emacs-managed instance.  Recorded by `le::cci-session-id-record' from the
SessionStart hook; read with `le::cci-session-ids-for-buffer'.")

(defun le::cci-session-ids--add (ids id)
  "Return IDS with ID at the front, appearing once.
Newest first, so the head is the conversation the terminal is on now.  A
nil or empty ID leaves IDS unchanged, and IDS itself is never modified."
  (if (and (stringp id) (not (string-empty-p id)))
      (cons id (delete id (copy-sequence ids)))
    ids))

;;;###autoload
(defun le::cci-session-id-record (session-id cli-session-id)
  "Record CLI-SESSION-ID as the current conversation of instance SESSION-ID.

SESSION-ID is the Emacs-minted cci session id naming the instance;
CLI-SESSION-ID is the conversation uuid the CLI minted for itself.  Called
from ~/.claude/bin/cci-session-id via emacsclient.

Returns the instance buffer's updated `le::cci-session-ids', or nil when
SESSION-ID names no live instance -- a CLI started outside Emacs has no
buffer to record against, and an instance is registered in the session
table a moment before its terminal buffer is attached."
  (when-let* ((session (claude-code-ide-mcp--get-session-by-id session-id))
              (buffer (claude-code-ide-mcp-session-buffer session))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (setq le::cci-session-ids
            (le::cci-session-ids--add le::cci-session-ids cli-session-id)))))

(defun le::cci-session-ids-for-buffer (&optional buffer)
  "Return the CLI conversation uuids recorded for BUFFER, newest first.
BUFFER defaults to the current buffer and should be a cci terminal."
  (buffer-local-value 'le::cci-session-ids (or buffer (current-buffer))))

(provide 'le-cci-session-ids)
;;; le-cci-session-ids.el ends here
