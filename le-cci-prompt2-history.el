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

;;;; Claude CLI history (~/.claude/history.jsonl)

(defvar le::cci-prompt2--claude-history-file "~/.claude/history.jsonl"
  "Claude Code CLI's own prompt-history file.
One JSON object per line for every prompt submitted in any project:
display, pastedContents, timestamp (epoch ms), project (the expanded
root, no trailing slash).  Merged into M-p/M-n so prompts typed
straight into the CLI -- or predating the org log file -- are
still recallable.")

(defconst le::cci-prompt2--dedup-window 5.0
  "Max |COMMITTED flip - CLI entry timestamp| seconds to call twins.
A prompt committed from the edit buffer also lands in
`le::cci-prompt2--claude-history-file' moments later; same text
inside this window means same send, and the org entry wins.")

(defun le::cci-prompt2--pasted-substitute (display pasted)
  "Return DISPLAY with \"[Pasted text #N ...]\" placeholders expanded.
PASTED is the entry's pastedContents value: a hash table keyed by the
placeholder number as a string.  Placeholders without a text-type
entry -- and all placeholders when PASTED is empty or not a table --
stay as-is."
  (if (not (and (hash-table-p pasted) (> (hash-table-count pasted) 0)))
      display
    (replace-regexp-in-string
     "\\[Pasted text #\\([0-9]+\\)\\(?: \\+[0-9]+ lines\\)?\\]"
     (lambda (match)
       (let ((obj (gethash (match-string 1 match) pasted)))
         (if (and (hash-table-p obj)
                  (equal (gethash "type" obj) "text")
                  (stringp (gethash "content" obj)))
             (gethash "content" obj)
           match)))
     display t t)))

(defun le::cci-prompt2--claude-history-entries (root)
  "Collect project ROOT's prompts from Claude Code's own history.
Returns plists (:id nil :state \"CLI\" :text TEXT :ts TS), newest
first, TS in float epoch seconds; nil when
`le::cci-prompt2--claude-history-file' is unreadable.  The file holds
every project's prompts, so a raw `search-forward' on the exact
project field prefilters the few MB before any JSON parsing; the
parsed object's own project is then compared exactly (the needle can
also hit prompt text that quotes it, or a longer path sharing the
prefix)."
  (let ((file (expand-file-name le::cci-prompt2--claude-history-file))
        (project (directory-file-name (expand-file-name root)))
        entries)
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (let ((needle (format "\"project\":\"%s\"" project)))
          (while (search-forward needle nil t)
            (let* ((line (buffer-substring-no-properties
                          (line-beginning-position) (line-end-position)))
                   (obj (ignore-errors (json-parse-string line)))
                   (ts (and (hash-table-p obj)
                            (equal (gethash "project" obj) project)
                            (gethash "timestamp" obj)))
                   (text (and (numberp ts)
                              (stringp (gethash "display" obj))
                              (string-trim
                               (le::cci-prompt2--pasted-substitute
                                (gethash "display" obj)
                                (gethash "pastedContents" obj))))))
              (when (and text (not (string-empty-p text)))
                (push (list :id nil :state "CLI" :text text :ts (/ ts 1000.0))
                      entries)))
            ;; One entry per line -- don't re-hit the same line when the
            ;; needle also appears inside its display text.
            (forward-line 1)))))
    entries))

;;;; History navigation

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
Returns plists (:id ID :state STATE :text TEXT :ts TS), newest first.
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
                    (push (list :id id :state state :text text :ts ts)
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
so same-:ts entries keep their source order."
  (dolist (org-entry org-entries)
    (when (equal (plist-get org-entry :state) "COMMITTED")
      (let ((text (string-trim (plist-get org-entry :text)))
            (ts (plist-get org-entry :ts)))
        (setq cli-entries
              (cl-delete-if
               (lambda (cli-entry)
                 (and (equal (plist-get cli-entry :text) text)
                      (<= (abs (- (plist-get cli-entry :ts) ts))
                          le::cci-prompt2--dedup-window)))
               cli-entries)))))
  (sort (append org-entries cli-entries)
        (lambda (a b) (> (plist-get a :ts) (plist-get b :ts)))))

(defun le::cci-prompt2--collect-history ()
  "Return the full M-p/M-n history for this edit buffer:
org-file entries merged with Claude's own history for the project,
deduped, newest first."
  (le::cci-prompt2--merge-history
   (le::cci-prompt2--collect-entries)
   (le::cci-prompt2--claude-history-entries
    (le::cci-prompt2--state-root le::cci-prompt2--st))))

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
          (format " History [%d/%d] %s %s    (M-p older, M-n newer)"
                  (1+ pos) (length entries)
                  (plist-get entry :state)
                  (le::cci-prompt2--age-string (plist-get entry :ts))))))

(defun le::cci-prompt2-history-previous ()
  "Show the previous (older) prompt from the merged history
\(log file + Claude's own ~/.claude/history.jsonl).
The first invocation stashes the in-progress draft;
\\[le::cci-prompt2-history-next] past the newest entry restores it."
  (interactive)
  (let ((st le::cci-prompt2--st))
    (unless (and st (org-src-edit-buffer-p))
      (user-error "Not in a CCI prompt edit buffer"))
    (cond
     ((null (le::cci-prompt2--state-hist-position st))
      (let ((entries (le::cci-prompt2--collect-history)))
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

(provide 'le-cci-prompt2-history)
;;; le-cci-prompt2-history.el ends here
