;;; le-cci-task-sessions.el --- CLI conversations recorded on a task heading  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Le Wang
;; Keywords: tools
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The task-side mirror of `le-cci-session-ids' (the terminal side).  That
;; file records, per cci terminal, which of Claude Code's conversations it has
;; been on; this one records, per org task heading, which conversations that
;; task was worked on -- a CC_SESSIONS property, newest first.  Together they
;; let a prompt composed from a task heading reach the instance that task was
;; actually worked on, rather than whichever instance the project's MRU
;; heuristic happens to name.
;;
;; The two lists are read ASYMMETRICALLY, and that is load-bearing.  A task's
;; list is read at its HEAD only: the head was the target instance's current
;; conversation at the moment it was recorded.  An instance's list is searched
;; in FULL: a /clear since then pushed that uuid deeper into the terminal's own
;; list without changing which terminal it is.  Matching head-to-head would
;; lose the instance on its first /clear.
;;
;; A task is identified by a MARKER, not by its `code=path' id -- a heading
;; need not carry one yet, and markers track edits that a file and line number
;; do not.  The marker is validated against the heading title captured at
;; compose time, so a marker that drifted to some other heading records
;; nothing.

;;; Code:

(require 'org)
(require 'le-cci-session-ids)

(declare-function claude-code-ide-mcp--active-sessions "claude-code-ide-mcp" ())
(declare-function claude-code-ide-mcp-session-buffer "claude-code-ide-mcp" (session))

(defconst le::cci-task-sessions-property "CC_SESSIONS"
  "Org property naming the CLI conversations a task was worked on.
Whitespace-separated uuids, newest first.  Distinct from the singular
CC_SESSION on a prompt-log heading, which names the one conversation
that one prompt was sent to.")

;;;; Pure core

(defun le::cci-task-sessions--parse (value)
  "Return the conversation uuids in VALUE, an org property string, in order.
Nil, empty, and all-whitespace VALUEs give nil."
  (when (stringp value)
    (split-string value nil t)))

(defun le::cci-task-sessions--format (ids)
  "Return IDS as an org property value, or nil when IDS is empty.
Nil rather than an empty string, so a caller can tell \"write nothing\"
from \"write a blank property\"."
  (when ids
    (string-join ids " ")))

(defun le::cci-task-sessions--match (head candidates)
  "Return the buffer in CANDIDATES whose instance has been on HEAD, or nil.
CANDIDATES is an alist of (BUFFER . IDS).  HEAD matches at ANY position
in a candidate's IDS, not just its head: HEAD was that instance's current
conversation when the task recorded it, and every /clear since has pushed
it further down without moving the instance.  The first match wins."
  (when (and (stringp head) (not (string-empty-p head)))
    (car (seq-find (lambda (cell) (member head (cdr cell))) candidates))))

;;;; Shell

(defun le::cci-task-sessions--candidates ()
  "Return (BUFFER . IDS) for every live cci terminal.
IDS is that instance's `le::cci-session-ids'; instances that never
declared a conversation appear with nil and simply never match.  Built
here rather than from `le::cci-prompt2--live-cci-sessions' so this file
stays independent of prompt2-core, which requires it."
  (delq nil
        (mapcar (lambda (session)
                  (let ((buffer (claude-code-ide-mcp-session-buffer session)))
                    (when (buffer-live-p buffer)
                      (cons buffer (le::cci-session-ids-for-buffer buffer)))))
                (claude-code-ide-mcp--active-sessions))))

(defun le::cci-task-sessions--heading-matches-p (title)
  "Return non-nil when point is on a TODO heading titled TITLE.
`org-at-heading-p' exactly at point, never `org-back-to-heading': a
marker that drifted off its heading must fail this check rather than
silently resolve to whatever heading encloses it now.  TITLE is compared
against the heading with stars, keyword, priority, and tags stripped --
the same form the prompt composer captured it in."
  (and (derived-mode-p 'org-mode)
       (org-at-heading-p)
       (org-get-todo-state)
       (stringp title)
       (equal title
              (string-trim
               (substring-no-properties (org-get-heading t t t t))))))

(defun le::cci-task-sessions-head (marker)
  "Return the conversation uuid most recently recorded at MARKER, or nil.
Only the head: see the asymmetry described in this file's commentary."
  (when (and (markerp marker) (buffer-live-p (marker-buffer marker)))
    (with-current-buffer (marker-buffer marker)
      (org-with-wide-buffer
       (goto-char marker)
       (when (and (derived-mode-p 'org-mode) (org-at-heading-p))
         (car (le::cci-task-sessions--parse
               (org-entry-get nil le::cci-task-sessions-property))))))))

(defun le::cci-task-sessions-record (marker title uuid)
  "Record UUID as the conversation the task at MARKER was last worked on.
MARKER must still sit on a TODO heading titled TITLE, and UUID must be a
non-empty string; any guard failing records nothing and returns nil.
UUID moves to the head of the CC_SESSIONS property, deduplicated, and the
task's buffer is saved -- writing the buffer and leaving disk behind
would diverge the two, and a later `dnc' write reverts only an unmodified
buffer.  Returns the new property value on success."
  (when (and (markerp marker)
             (buffer-live-p (marker-buffer marker))
             (stringp uuid)
             (not (string-empty-p uuid)))
    (with-current-buffer (marker-buffer marker)
      (org-with-wide-buffer
       (goto-char marker)
       (when (le::cci-task-sessions--heading-matches-p title)
         (let* ((existing (le::cci-task-sessions--parse
                           (org-entry-get nil le::cci-task-sessions-property)))
                (value (le::cci-task-sessions--format
                        (le::cci-session-ids--add existing uuid))))
           (org-entry-put nil le::cci-task-sessions-property value)
           (save-buffer)
           value))))))

(provide 'le-cci-task-sessions)
;;; le-cci-task-sessions.el ends here
