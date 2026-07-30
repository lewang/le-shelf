;;; le-cci-prompt2-paste.el --- CCI prompt editor: Claude CLI history reader  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Le Wang
;; Keywords: tools
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Reads Claude Code's own ~/.claude/history.jsonl into the plists
;; `le-cci-prompt2-history' merges into M-p/M-n, expanding the
;; "[Pasted text #N]" placeholders its display strings carry.
;;
;; Recalling a prompt with C-p and re-sending it stores the placeholder
;; without its payload, so an entry's own pastedContents cannot always
;; answer.  Three sources are tried in turn: the entry's inline payload,
;; Claude's content-addressed paste cache, then the same paste id's
;; earlier submission in the same session.  What none of them answers is
;; marked rather than dropped -- a prompt that never reached Claude is
;; still history worth recalling, and a marker fails visibly on re-send
;; where a bare placeholder reads as a legitimate prompt.
;;
;; Separate from `le-cci-prompt2-history' because the parsing here needs
;; nothing but cl-lib, subr-x and the built-in JSON reader, while the
;; navigation side pulls in org, denote and w -- so this half is
;; testable in batch.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defvar le::cci-prompt2--claude-history-file "~/.claude/history.jsonl"
  "Claude Code CLI's own prompt-history file.
One JSON object per line for every prompt submitted in any project:
display, pastedContents, timestamp (epoch ms), project (the expanded
root, no trailing slash).  Merged into M-p/M-n so prompts typed
straight into the CLI -- or predating the org log file -- are
still recallable.")

;;;; Reading the records

(defun le::cci-prompt2--claude-history-records (root)
  "Read project ROOT's raw prompt records from Claude Code's own history.
Returns plists (:display :pasted :ts :session), newest first, TS in
float epoch seconds and :pasted the entry's own pastedContents; nil
when `le::cci-prompt2--claude-history-file' is unreadable.  Paste
placeholders in :display are left alone -- resolving one can need a
payload recorded on a *different* line, so expansion waits for the
whole set (see `le::cci-prompt2--claude-history-entries').

The file holds every project's prompts, so a raw `search-forward' on
the exact project field prefilters the few MB before any JSON
parsing; the parsed object's own project is then compared exactly (the
needle can also hit prompt text that quotes it, or a longer path
sharing the prefix)."
  (let ((file (expand-file-name le::cci-prompt2--claude-history-file))
        (project (directory-file-name (expand-file-name root)))
        records)
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
                            (gethash "timestamp" obj))))
              (when (and (numberp ts) (stringp (gethash "display" obj)))
                (push (list :display (gethash "display" obj)
                            :pasted (gethash "pastedContents" obj)
                            :ts (/ ts 1000.0)
                            :session (gethash "sessionId" obj))
                      records)))
            ;; One entry per line -- don't re-hit the same line when the
            ;; needle also appears inside its display text.
            (forward-line 1)))))
    records))

;;;; Expanding the placeholders

(defconst le::cci-prompt2--paste-placeholder-re
  "\\[Pasted text #\\([0-9]+\\)\\(?: \\+\\([0-9]+\\) lines\\)?\\]"
  "Matches one \"[Pasted text #N +K lines]\" placeholder in a display string.
Group 1 is the paste id, group 2 the K of the \"+K lines\" suffix when
it is present.  K counts the lines after the first, so a payload
answering the placeholder has K+1 of them.")

(defun le::cci-prompt2--paste-expected-lines (k)
  "Return the payload line count a \"+K lines\" suffix implies.
K is the suffix's digits as a string, or nil for a suffix-less
placeholder -- which stands for a single line."
  (if k (1+ (string-to-number k)) 1))

(defun le::cci-prompt2--paste-content (obj)
  "Return OBJ's inline paste payload, or nil.
OBJ is one pastedContents value.  A text-type entry usually carries
its payload under `content', but an elided one carries only a
`contentHash' -- there is nothing inline to return."
  (and (hash-table-p obj)
       (equal (gethash "type" obj) "text")
       (let ((content (gethash "content" obj)))
         (and (stringp content) content))))

(defun le::cci-prompt2--expand-pastes (display pasted lookup)
  "Return DISPLAY with its \"[Pasted text #N ...]\" placeholders expanded.
PASTED is the entry's own pastedContents: a hash table keyed by the
paste id as a string.  Each placeholder takes the first source that
answers -- PASTED's own inline payload, then LOOKUP -- and is left
verbatim when neither does.

LOOKUP is nil, or a function of (ID EXPECTED-LINES) returning a
payload string or nil.  EXPECTED-LINES is what the placeholder's \"+K
lines\" suffix implies, so a LOOKUP reaching outside this entry can
reject a candidate whose length disagrees."
  (replace-regexp-in-string
   le::cci-prompt2--paste-placeholder-re
   (lambda (match)
     (let ((id (match-string 1 match)))
       (or (and (hash-table-p pasted)
                (le::cci-prompt2--paste-content (gethash id pasted)))
           (and lookup
                (funcall lookup id
                         (le::cci-prompt2--paste-expected-lines
                          (match-string 2 match))))
           match)))
   display t t))

(defun le::cci-prompt2--paste-index (records)
  "Index every inline paste payload in RECORDS for cross-entry lookup.
Returns a hash table keying \"SESSION\\0ID\" to a list of (TS
. CONTENT), one pair per payload recorded under that paste id in that
session.  The key is session-scoped because paste ids number from one
per session and are reused within a long-lived one, so an id alone
identifies nothing."
  (let ((index (make-hash-table :test #'equal)))
    (dolist (record records)
      (let ((pasted (plist-get record :pasted))
            (session (plist-get record :session))
            (ts (plist-get record :ts)))
        (when (and (hash-table-p pasted) (stringp session))
          (maphash
           (lambda (id obj)
             (when-let* ((content (le::cci-prompt2--paste-content obj)))
               (push (cons ts content)
                     (gethash (concat session "\0" id) index))))
           pasted))))
    index))

;;;; Claude's paste cache

(defvar le::cci-prompt2--paste-cache-dir "~/.claude/paste-cache/"
  "Claude Code's content-addressed store for elided paste payloads.
A pastedContents entry that carries a `contentHash' instead of its
`content' has its payload in <hash>.txt here.  Retention is unknown
and nothing depends on it: a miss falls through to the next source.")

(defun le::cci-prompt2--paste-hash (obj)
  "Return OBJ's contentHash, or nil.
OBJ is one pastedContents value; only the elided shape carries one.
Non-hex is refused -- the value names a file, so it must not be able
to reach out of `le::cci-prompt2--paste-cache-dir'."
  (and (hash-table-p obj)
       (let ((hash (gethash "contentHash" obj)))
         (and (stringp hash)
              (string-match-p "\\`[0-9a-f]+\\'" hash)
              hash))))

(defun le::cci-prompt2--paste-cache-content (hash)
  "Return the payload HASH names in Claude's paste cache, or nil.
nil when the file is absent or unreadable -- an evicted cache entry is
ordinary, not an error."
  (let ((file (expand-file-name (concat hash ".txt")
                                le::cci-prompt2--paste-cache-dir)))
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (buffer-string)))))

;;;; Borrowing from an earlier submission

(defun le::cci-prompt2--paste-line-count (content)
  "Return CONTENT's line count the way a \"+K lines\" suffix counts.
Measured against every inline payload in the history: all 434 agree
that a \"+K lines\" placeholder stands for K+1 of these."
  (1+ (cl-count ?\n content)))

(defun le::cci-prompt2--paste-sibling (index session id ts expected-lines)
  "Return a payload borrowed from an earlier submission, or nil.
Searches INDEX for a payload recorded under paste id ID in session
SESSION, strictly before TS and EXPECTED-LINES lines long, and returns
the most recent of those.

Earlier is the only direction that can hold one: recalling a prompt is
what loses its paste map, so the payload was recorded when that prompt
was first sent.  The length test and the recency each rule out a wrong
answer the other would admit -- ids are reused within a long session,
and two payloads of equal length can sit under one id."
  (let (best)
    (dolist (candidate (gethash (concat session "\0" id) index))
      (when (and (< (car candidate) ts)
                 (= (le::cci-prompt2--paste-line-count (cdr candidate))
                    expected-lines)
                 (or (null best) (> (car candidate) (car best))))
        (setq best candidate)))
    (cdr best)))

(defun le::cci-prompt2--paste-marker (id expected-lines)
  "Return the text standing in for unrecoverable paste ID.
EXPECTED-LINES is the length its placeholder claimed.  Deliberately
not prose: the entry is kept, because a prompt that never reached
Claude is still history, but re-sending it must fail visibly instead
of reading as a legitimate prompt."
  (format "[paste #%s unrecoverable: %d line%s]"
          id expected-lines (if (= expected-lines 1) "" "s")))

(defun le::cci-prompt2--paste-lookup (record index)
  "Return a LOOKUP for `le::cci-prompt2--expand-pastes' over RECORD.
Tries Claude's paste cache when RECORD's own pastedContents elided the
payload to a `contentHash', then an earlier submission of the same
paste id in the same session (`le::cci-prompt2--paste-sibling' over
INDEX).  Falls back to `le::cci-prompt2--paste-marker', so the
returned function never declines: every placeholder ends up either
expanded or explicitly marked, none left bare."
  (let ((pasted (plist-get record :pasted))
        (session (plist-get record :session))
        (ts (plist-get record :ts)))
    (lambda (id expected-lines)
      (or (when-let* ((hash (le::cci-prompt2--paste-hash
                             (and (hash-table-p pasted) (gethash id pasted)))))
            (le::cci-prompt2--paste-cache-content hash))
          (and (stringp session)
               (le::cci-prompt2--paste-sibling index session id ts
                                               expected-lines))
          (le::cci-prompt2--paste-marker id expected-lines)))))

;;;; The merged entry list

(defun le::cci-prompt2--claude-history-entries (root)
  "Collect project ROOT's prompts from Claude Code's own history.
Returns plists (:id nil :state \"CLI\" :text TEXT :ts TS), newest
first, TS in float epoch seconds.  TEXT carries its paste placeholders
expanded from the three sources `le::cci-prompt2--paste-lookup'
describes, or marked unrecoverable; an entry whose text is empty is
dropped.

A borrowed payload can only come from the same session, and a session
belongs to one project, so `le::cci-prompt2--claude-history-records'
prefiltering on ROOT never hides a candidate."
  (let* ((records (le::cci-prompt2--claude-history-records root))
         (index (le::cci-prompt2--paste-index records))
         entries)
    (dolist (record records)
      (let ((text (string-trim
                   (le::cci-prompt2--expand-pastes
                    (plist-get record :display)
                    (plist-get record :pasted)
                    (le::cci-prompt2--paste-lookup record index)))))
        (unless (string-empty-p text)
          (push (list :id nil :state "CLI" :text text
                      :ts (plist-get record :ts))
                entries))))
    (nreverse entries)))

(provide 'le-cci-prompt2-paste)
;;; le-cci-prompt2-paste.el ends here
