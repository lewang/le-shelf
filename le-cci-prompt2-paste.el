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
;; Separate from `le-cci-prompt2-history' because the parsing here needs
;; nothing but subr-x and the built-in JSON reader, while the navigation
;; side pulls in org, denote and w -- so this half is testable in batch.

;;; Code:

(require 'subr-x)

(defvar le::cci-prompt2--claude-history-file "~/.claude/history.jsonl"
  "Claude Code CLI's own prompt-history file.
One JSON object per line for every prompt submitted in any project:
display, pastedContents, timestamp (epoch ms), project (the expanded
root, no trailing slash).  Merged into M-p/M-n so prompts typed
straight into the CLI -- or predating the org log file -- are
still recallable.")

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

(provide 'le-cci-prompt2-paste)
;;; le-cci-prompt2-paste.el ends here
