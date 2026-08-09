;;; le-cci-prompt2-history.el --- CCI prompt editor: prompt history  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Le Wang
;; Keywords: tools
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; M-p/M-n prompt history for the Claude Code prompt composer (see
;; `le-cci-prompt2').  Reads the project's prompt-log file directly and
;; merges it with Claude Code's own ~/.claude/history.jsonl (so prompts
;; typed straight into the CLI are recallable too), deduping the twin a
;; commit leaves in both stores, and drives the in-buffer navigation.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-src)
(require 'org-duration)
(require 'le-cci-prompt2-core)
(require 'le-cci-prompt2-log)
(require 'le-cci-prompt2-paste)

;;;; History navigation

(defconst le::cci-prompt2--dedup-window 5.0
  "Max |COMMITTED flip - CLI entry timestamp| seconds to call twins.
A prompt committed from the edit buffer also lands in
`le::cci-prompt2--claude-history-file' moments later; same text
inside this window means same send, and the org entry wins.")

(defun le::cci-prompt2--busy-positions (src-buf)
  "Return SRC-BUF positions of blocks open in OTHER src edit buffers.
Each is the block-begin marker position of a live `org-src-mode' edit
buffer, excluding the current one."
  (let (positions)
    (dolist (buf (buffer-list))
      (unless (eq buf (current-buffer))
        (with-current-buffer buf
          (when (and (org-src-edit-buffer-p)
                     (eq (marker-buffer org-src--beg-marker) src-buf))
            (push (marker-position org-src--beg-marker) positions)))))
    positions))

(defun le::cci-prompt2--subtree-block-text (beg end)
  "Return the body of the first src block between BEG and END.
nil when the region holds no block.  The parser's :value is already
comma-unescaped (`org-element--unescape-substring'); unescaping again
here would corrupt prompts containing literal \",*\"/\",#+\" lines.
Trailing whitespace is trimmed -- block bodies always carry a final
newline."
  (save-excursion
    (goto-char beg)
    (when (re-search-forward "^[ \t]*#\\+begin_src" end t)
      (let ((el (org-element-at-point)))
        (when (eq (org-element-type el) 'src-block)
          (string-trim-right
           (or (org-element-property :value el) "")))))))

(defun le::cci-prompt2--stamp-time (stamp)
  "Return bracketed STAMP's time as float epoch seconds, or nil.
STAMP looks like \"[2026-07-14 Tue 17:47:45]\"; the seconds field is
optional.  Parsed by hand: org's own timestamp parser drops the
seconds these stamps carry (see
`le::cci-prompt2--state-stamp-format'), and for a COMMITTED heading
those seconds are what keep its flip time inside
`le::cci-prompt2--dedup-window'.  Minute-precision stamps (from
before seconds shipped) still parse -- they order fine, they just
cannot dedupe."
  (when (string-match
         (concat "\\`\\[\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)"
                 " [[:alpha:]]+ \\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)"
                 "\\(?::\\([0-9]\\{2\\}\\)\\)?\\]\\'")
         stamp)
    (float-time
     (encode-time
      (list (if (match-string 6 stamp)
                (string-to-number (match-string 6 stamp))
              0)
            (string-to-number (match-string 5 stamp))
            (string-to-number (match-string 4 stamp))
            (string-to-number (match-string 3 stamp))
            (string-to-number (match-string 2 stamp))
            (string-to-number (match-string 1 stamp))
            nil -1 nil)))))

(defun le::cci-prompt2--collect-entries ()
  "Collect entries from this edit buffer's log file.
Returns plists (:id ID :state STATE :text TEXT :ts TS :session
SESSION), newest first.  SESSION is the CLI conversation the heading
was composed for (its `le::cci-prompt2--session-property' property),
nil for headings written before instances were recorded -- the key
session-scoped history filters on.
ID is the headline's bracketed stamp; TS is its parsed time -- the
heading's most recent state change, since the headline mirrors the
top LOGBOOK line (for a COMMITTED heading that flip time is the
dedup key against Claude's own history).  The current heading is
excluded; EDITING headings whose block is open in another edit
buffer are skipped with a message, since recalling them here would
fork an in-progress draft."
  (let* ((st le::cci-prompt2--st)
         (own-id (le::cci-prompt2--state-heading-id st))
         (src-buf (marker-buffer org-src--beg-marker)))
    (unless (buffer-live-p src-buf)
      (user-error "Log file buffer was killed; reopen %s"
                  (le::cci-prompt2--state-file-path st)))
    (let ((busy (le::cci-prompt2--busy-positions src-buf))
          (skipped 0)
          entries)
      (with-current-buffer src-buf
        (save-excursion
          (save-restriction
            (widen)
            (goto-char (point-min))
            (while (re-search-forward
                    "^\\* \\([A-Z]+\\) \\(\\[[^]\n]+\\]\\)" nil t)
              (let ((state (match-string-no-properties 1))
                    (id (match-string-no-properties 2))
                    (heading-beg (line-beginning-position))
                    (subtree-end (save-excursion
                                   (org-end-of-subtree t t)
                                   (point))))
                (cond
                 ((equal id own-id))
                 ((and (equal state "EDITING")
                       (seq-some (lambda (pos)
                                   (and (>= pos heading-beg) (< pos subtree-end)))
                                 busy))
                  (cl-incf skipped))
                 (t
                  (when-let* ((text (le::cci-prompt2--subtree-block-text
                                     heading-beg subtree-end))
                              (ts (le::cci-prompt2--stamp-time id)))
                    (push (list :id id :state state :text text :ts ts
                                :session (org-entry-get
                                          heading-beg
                                          le::cci-prompt2--session-property))
                          entries))))
                (goto-char subtree-end))))))
      (when (> skipped 0)
        (message "Skipped %d EDITING heading(s) open in other edit buffers" skipped))
      entries)))

(defun le::cci-prompt2--merge-history (org-entries cli-entries)
  "Merge ORG-ENTRIES and CLI-ENTRIES into one newest-first list.
A prompt committed from the edit buffer also lands in Claude's own
history: a CLI entry with `equal' text whose timestamp falls within
`le::cci-prompt2--dedup-window' of an org entry's COMMITTED flip is
that same send -- drop it, the org entry wins.  The sort is stable,
so same-:ts entries keep their source order.

A twin also carries the conversation it was sent to, so an org entry
that has none inherits it.  Without that, dropping the twin would lose
the only attribution the pair had and hide the prompt from session
scope entirely -- which is every heading written before headings
recorded their instance.  Being the same send, the twin's session is
that heading's by construction.  The inheritance is an in-place
`plist-put': both collectors always emit :session, nil or not, so the
key is there to set."
  (dolist (org-entry org-entries)
    (when (equal (plist-get org-entry :state) "COMMITTED")
      (let ((text (string-trim (plist-get org-entry :text)))
            (ts (plist-get org-entry :ts))
            twin)
        (setq cli-entries
              (cl-delete-if
               (lambda (cli-entry)
                 (when (and (equal (plist-get cli-entry :text) text)
                            (<= (abs (- (plist-get cli-entry :ts) ts))
                                le::cci-prompt2--dedup-window))
                   (setq twin cli-entry)
                   t))
               cli-entries))
        (when (and twin (null (plist-get org-entry :session)))
          (plist-put org-entry :session (plist-get twin :session))))))
  (sort (append org-entries cli-entries)
        (lambda (a b) (> (plist-get a :ts) (plist-get b :ts)))))

;;;; Scope: this instance, or the whole project

(defun le::cci-prompt2--target-session-uuids (st)
  "Return the CLI conversation uuids of ST's target instance, or nil.
Read live rather than captured at open: a /clear mid-edit mints a fresh
conversation in that terminal, and the draft should still recall what
the terminal did before it."
  (let ((buf (le::cci-prompt2--state-cci-buffer st)))
    (and (buffer-live-p buf)
         (le::cci-prompt2--cci-session-uuids buf))))

(defun le::cci-prompt2--scope-description (scope uuids)
  "Return the header-line phrase describing history SCOPE.
UUIDS is what the target instance has declared; with none there is
nothing to scope to, so SCOPE is moot and the phrase says why rather
than offering a toggle that would not change the list."
  (cond ((null uuids) "(project -- no session recorded)")
        ((eq scope 'session) "(session -- C-c C-s widens)")
        (t "(project -- C-c C-s narrows)")))

(defun le::cci-prompt2--collect-history (st)
  "Return the M-p/M-n history for ST's edit buffer, newest first.
Org-file entries merged with Claude's own history for the project,
deduped, then narrowed to the target instance's conversations when
ST's scope is `session'.  An instance that declared none is not
narrowable, so its history stays project-wide.

The narrowing happens after the merge, never before: the dedup that
drops a committed prompt's CLI twin has to see both stores whole."
  (let ((entries (le::cci-prompt2--merge-history
                  (le::cci-prompt2--collect-entries)
                  (le::cci-prompt2--claude-history-entries
                   (le::cci-prompt2--state-root st))))
        (uuids (le::cci-prompt2--target-session-uuids st)))
    (if (and uuids (eq (le::cci-prompt2--state-hist-scope st) 'session))
        (le::cci-prompt2--entries-for-sessions entries uuids)
      entries)))

(defun le::cci-prompt2--age-string (ts)
  "Return a relative age like \"2:46 ago\" for TS, float epoch seconds.
Sub-minute ages read \"less than 1 min ago\"; longer ages go through
`org-duration-from-minutes' (h:mm by default, days per
`org-duration-format')."
  (let ((mins (floor (/ (- (float-time) ts) 60))))
    (if (< mins 1)
        "less than 1 min ago"
      (format "%s ago" (org-duration-from-minutes mins)))))

(defun le::cci-prompt2--show-history-entry ()
  "Display the current history entry in the edit buffer."
  (let* ((st le::cci-prompt2--st)
         (pos (le::cci-prompt2--state-hist-position st))
         (entries (le::cci-prompt2--state-hist-entries st))
         (entry (nth pos entries)))
    (erase-buffer)
    (insert (plist-get entry :text))
    (setq header-line-format
          (format " History [%d/%d] %s %s    (M-p older, M-n newer)    %s"
                  (1+ pos) (length entries)
                  (plist-get entry :state)
                  (le::cci-prompt2--age-string (plist-get entry :ts))
                  (le::cci-prompt2--scope-description
                   (le::cci-prompt2--state-hist-scope st)
                   (le::cci-prompt2--target-session-uuids st))))))

(defun le::cci-prompt2-history-previous ()
  "Show the previous (older) prompt from the merged history
\(log file + Claude's own ~/.claude/history.jsonl), scoped to the
instance this draft targets.
The first invocation stashes the in-progress draft;
\\[le::cci-prompt2-history-next] past the newest entry restores it, and
\\[le::cci-prompt2-history-toggle-scope] widens the list to every
instance in the project."
  (interactive)
  (let ((st le::cci-prompt2--st))
    (unless (and st (org-src-edit-buffer-p))
      (user-error "Not in a CCI prompt edit buffer"))
    (cond
     ((null (le::cci-prompt2--state-hist-position st))
      (let ((entries (le::cci-prompt2--collect-history st)))
        (if (null entries)
            (message "No history")
          (setf (le::cci-prompt2--state-hist-entries st) entries)
          (setf (le::cci-prompt2--state-hist-saved-input st) (buffer-string))
          (setf (le::cci-prompt2--state-hist-position st) 0)
          (le::cci-prompt2--show-history-entry))))
     ((>= (1+ (le::cci-prompt2--state-hist-position st))
          (length (le::cci-prompt2--state-hist-entries st)))
      (message "End of history"))
     (t
      (cl-incf (le::cci-prompt2--state-hist-position st))
      (le::cci-prompt2--show-history-entry)))))

(defun le::cci-prompt2-history-next ()
  "Show the next (newer) prompt, or restore the stashed draft.
Returning to the draft clears the collected entries, so the next
\\[le::cci-prompt2-history-previous] re-reads the log file."
  (interactive)
  (let ((st le::cci-prompt2--st))
    (unless (and st (org-src-edit-buffer-p))
      (user-error "Not in a CCI prompt edit buffer"))
    (cond
     ((null (le::cci-prompt2--state-hist-position st))
      (message "End of history"))
     ((zerop (le::cci-prompt2--state-hist-position st))
      (erase-buffer)
      (insert (or (le::cci-prompt2--state-hist-saved-input st) ""))
      (setf (le::cci-prompt2--state-hist-position st) nil)
      (setf (le::cci-prompt2--state-hist-entries st) nil)
      (setf (le::cci-prompt2--state-hist-saved-input st) nil)
      (setq header-line-format
            (le::cci-prompt2--state-header-line-default st)))
     (t
      (cl-decf (le::cci-prompt2--state-hist-position st))
      (le::cci-prompt2--show-history-entry)))))

(defun le::cci-prompt2--rescope-history (st)
  "Re-collect ST's history under its current scope, keeping the shown entry.
Returns non-nil when the new scope has any prompts; nil leaves the
navigation untouched, for the caller to undo its scope change with.

The entry on screen is looked up in the new list rather than the
position being kept: scoping renumbers everything, so an index means
nothing across the change.  A narrowing that drops the shown entry --
it belonged to a sibling instance -- lands on the newest instead."
  (let* ((current (nth (le::cci-prompt2--state-hist-position st)
                       (le::cci-prompt2--state-hist-entries st)))
         (entries (le::cci-prompt2--collect-history st)))
    (when entries
      (setf (le::cci-prompt2--state-hist-entries st) entries)
      (setf (le::cci-prompt2--state-hist-position st)
            (or (le::cci-prompt2--entry-position entries current) 0))
      (le::cci-prompt2--show-history-entry)
      t)))

(defun le::cci-prompt2-history-toggle-scope ()
  "Toggle M-p/M-n between this instance's prompts and the project's.
A project runs several Claude Code instances and they share one
prompt-log file and one project in Claude's own history, so the
unscoped list is every sibling's prompts too.  Session scope -- the
default -- keeps to the conversations the target terminal has been on.

The scope sticks for the rest of the edit session, including across a
return to the draft.  Mid-navigation it re-collects at once, keeping
the prompt on screen where it can.  Refuses when the target instance
has declared no conversation \(one started before the SessionStart
hook shipped): there is nothing to narrow to, and its history is
already project-wide."
  (interactive)
  (let ((st le::cci-prompt2--st))
    (unless (and st (org-src-edit-buffer-p))
      (user-error "Not in a CCI prompt edit buffer"))
    (unless (le::cci-prompt2--target-session-uuids st)
      (user-error "No conversation recorded for this instance; history is project-wide"))
    (let ((previous (le::cci-prompt2--state-hist-scope st)))
      (setf (le::cci-prompt2--state-hist-scope st)
            (if (eq previous 'session) 'project 'session))
      (cond
       ((null (le::cci-prompt2--state-hist-position st))
        (message "Prompt history scope: %s"
                 (le::cci-prompt2--state-hist-scope st)))
       ((not (le::cci-prompt2--rescope-history st))
        (message "No prompts at %s scope"
                 (le::cci-prompt2--state-hist-scope st))
        (setf (le::cci-prompt2--state-hist-scope st) previous))))))

(provide 'le-cci-prompt2-history)
;;; le-cci-prompt2-history.el ends here
