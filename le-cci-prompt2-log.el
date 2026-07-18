;;; le-cci-prompt2-log.el --- CCI prompt editor: log file + headings  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Le Wang
;; Keywords: tools
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Prompt-log file management and per-heading bookkeeping for the Claude
;; Code prompt composer (see `le-cci-prompt2').  Creates/locates the
;; project's denote-backed prompt-log org file, and appends/flips/moves
;; the level-1 headings that record each prompt's EDITING -> COMMITTED /
;; ABANDONED lifecycle, writing the LOGBOOK "- State" lines directly (org's
;; own note machinery cannot run cleanly in this never-displayed buffer).

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'denote)

;;;; Log file management

(defun le::cci-prompt2--create-log-file (dir)
  "Create a new prompt-log org file in DIR via `denote'; return its path.
DIR is the prompt-log/ subdirectory of the playground's denote silo.
`denote-directory' is let-bound to that silo so DIR resolves inside
it -- otherwise `denote' silently falls back to the global notes
directory.  The per-file `#+todo:' state sequence rides in as denote's
TEMPLATE string.  `denote-kill-buffers' makes denote save and kill the
buffer it visits: denote visits the file *before* inserting the front
matter, so a surviving buffer would have initialized `org-mode'
without the `#+todo:' line and its EDITING/COMMITTED/ABANDONED
keywords would be dead -- the caller's own `find-file-noselect'
re-visit picks them up fresh.  The `save-window-excursion' contains
that visit's window switch (`denote' uses `find-file').  Dropping
`keywords' from the always-present components removes the empty
`#+filetags:' line -- the silo's location already identifies the
project, so the file carries no tags."
  (let ((denote-directory (file-name-directory (directory-file-name dir)))
        (denote-kill-buffers 'on-creation)
        (denote-front-matter-components-present-even-if-empty-value
         '(title date identifier)))
    (save-window-excursion
      ;; Human-readable TITLE goes in the front matter verbatim; denote
      ;; sluggifies it to claude-code-prompt-log for the file name.
      (denote "Claude Code prompt log" nil 'org dir nil
              "#+todo: EDITING(e!) | COMMITTED(c!) ABANDONED(a!)\n"))))

(defun le::cci-prompt2--log-file (root)
  "Return the newest prompt-log file for project ROOT, creating one
when none exists.  `.le-playground/' and its denote/ silo are hard
prerequisites (both bootstrapped by /playground-setup); the
prompt-log/ subdirectory is created on demand.  Newest is by
filename -- denote-style timestamp prefixes sort chronologically."
  (let ((playground (expand-file-name ".le-playground" root)))
    (unless (file-directory-p playground)
      (user-error "No .le-playground/ in %s -- run /playground-setup first" root))
    (let ((silo (expand-file-name "denote" playground)))
      (unless (file-directory-p silo)
        (user-error "No denote silo in %s -- re-run /playground-setup to add it"
                    playground))
      (let ((dir (expand-file-name "prompt-log" silo)))
        (make-directory dir t)
        (or (car (last (directory-files
                        dir t
                        "\\`[0-9]\\{8\\}T[0-9]\\{6\\}--claude-code-prompt-log\\.org\\'")))
            (le::cci-prompt2--create-log-file dir))))))

;;;; Heading bookkeeping (all run in the log file buffer)

(defconst le::cci-prompt2--state-stamp-format "[%Y-%m-%d %a %H:%M:%S]"
  "`format-time-string' format for LOGBOOK \"- State\" line stamps.
Carries seconds -- the COMMITTED flip time is deduped against
~/.claude/history.jsonl entry timestamps, and minute precision would
eat most of `le::cci-prompt2--dedup-window'.  Rendered directly by
`le::cci-prompt2--insert-state-line'; under org's own logging this
precision needed a global let of `org-time-stamp-formats', because
`org-store-log-note' renders stamps in the *Org Note* scratch buffer
where the log file's buffer-local variables are out of scope.")

(defun le::cci-prompt2--heading-flip-stamp ()
  "Return the bracketed timestamp of the most recent state flip
recorded in the LOGBOOK of the heading at point, brackets included --
e.g. \"[2026-07-14 Tue 17:47:45]\" -- or nil when the drawer's top
line is not a \"- State\" entry.  Only the top line is examined:
state lines are prepended (`le::cci-prompt2--insert-state-line'), so
right after a flip the line at `org-log-beginning' is that flip's.
This is what the headline mirrors -- after every flip the headline's stamp is
rewritten to this value, so headline time == latest state change by
construction, with no same-second formatting race."
  (save-excursion
    (org-back-to-heading t)
    (goto-char (org-log-beginning))
    (when (looking-at "[ \t]*- State \"[A-Z]+\".*\\(\\[[^]\n]+\\]\\)")
      (match-string-no-properties 1))))

(defun le::cci-prompt2--insert-state-line (state prev-state)
  "Prepend the LOGBOOK \"- State\" line for a flip to STATE of the
heading at point.  PREV-STATE is the keyword the heading left, nil
when it had none.  The line is rendered from `org-log-note-headings''
`state' template and inserted at `org-log-beginning' -- the same
template and insertion point `org-store-log-note' uses -- so it is
what org itself would have logged, except the stamp carries seconds
\(see `le::cci-prompt2--state-stamp-format').  Point is preserved.

Callers bind `org-inhibit-logging' around `org-todo' and call this
instead, because org's own logging cannot run cleanly in a background
buffer.  `org-todo' defers the line to `org-add-log-note', which
opens with the window dance only an interactive `@' note needs --
`pop-to-buffer' the log buffer full-frame, then an *Org Note* buffer
-- before noticing that a `!'-style flip stores immediately (org
9.8.7, org.el:11064).  Selecting a window records its buffer, and the
window configuration org restores afterwards (org.el:11182) covers
neither the frame's `buffer-list' parameter, nor the global buffer
list, nor the selected window's `prev-buffers' -- so every flip of
this never-displayed file promoted it to the front of all three,
making the prompt log the `C-x b' default and polluting
`switch-to-prev-buffer' history.  Writing the line directly sidesteps
that machinery entirely; it also renders the stamp in this buffer,
ending the global `org-time-stamp-formats' let that seconds precision
used to require."
  (org-fold-core-ignore-modifications
    (org-with-wide-buffer
     (let ((line (org-replace-escapes
                  (cdr (assq 'state org-log-note-headings))
                  (list (cons "%t" (format-time-string
                                    le::cci-prompt2--state-stamp-format))
                        (cons "%T" (format-time-string
                                    "<%Y-%m-%d %a %H:%M:%S>"))
                        (cons "%d" (format-time-string "[%Y-%m-%d %a]"))
                        (cons "%D" (format-time-string "<%Y-%m-%d %a>"))
                        (cons "%s" (if state (format "\"%s\"" state) ""))
                        (cons "%S" (if prev-state
                                       (format "\"%s\"" prev-state)
                                     ""))
                        (cons "%u" (user-login-name))
                        (cons "%U" user-full-name)))))
       (goto-char (org-log-beginning t))
       ;; `org-store-log-note's own fixups: the line gets a row of its
       ;; own, list-indented like the sibling notes.
       (cond ((not (bolp)) (insert-and-inherit "\n"))
             ((looking-at "[ \t]*\\S-")
              (save-excursion (insert-and-inherit "\n"))))
       (let ((itemp (org-in-item-p)))
         (if itemp
             (indent-line-to
              (let ((struct (save-excursion
                              (goto-char itemp) (org-list-struct))))
                (org-list-get-ind (org-list-get-top-point struct) struct)))
           (org-indent-line)))
       (insert-and-inherit (org-list-bullet-string "-") line)))))

(defun le::cci-prompt2--extract-subject (text)
  "Return the subject from TEXT's leading \"re: SUBJECT\" line, or nil."
  (when (string-match "\\`re: \\(.+\\)$" text)
    (string-trim (match-string 1 text))))

(defun le::cci-prompt2--insert-heading (file-buf subject text)
  "Append a new EDITING heading to FILE-BUF and save the file.
The headline's title is the bracketed stamp of its own EDITING flip,
read back via `le::cci-prompt2--heading-flip-stamp' from the LOGBOOK
line `org-todo' just wrote, so headline time and latest state change
are identical by construction.  That stamp is also the heading's id
for the rest of the edit session.  SUBJECT, when non-nil, follows it
as \"re: SUBJECT\".
The heading is inserted without a keyword and put into EDITING via
`org-todo' with logging inhibited; the LOGBOOK line is written by
`le::cci-prompt2--insert-state-line' (see it for why org's own note
machinery is unusable in this never-displayed buffer).
`org-loop-over-headlines-in-active-region' is
disabled around the flip: invoked from the log file's own buffer with
an active region, `org-todo' would otherwise loop over the region's
headings instead of flipping the one at point -- leaving this heading
keyword-less and landing the flip on whichever heading the region
loop found.
TEXT seeds the src block body, escaped, with a trailing newline
guaranteed.  The block carries the -i (preserve indentation) switch --
without it, write-back would indent every line by
`org-edit-src-content-indentation'.  The save is the crash-recovery
point: the draft is on disk before the edit buffer even opens.
Appending at the end of the buffer also keeps the EDITING zone -- the
file's tail (see `le::cci-prompt2--move-heading-to-final-zone') --
ascending by stamp.
Returns (ID . BODY-BEG): the new heading's stamp id and the buffer
position of the block body's start."
  (with-current-buffer file-buf
    (widen)
    (save-excursion
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert "* \n")
      (forward-line -1)
      (let ((org-inhibit-logging t)  ; line written below, not by org
            (org-loop-over-headlines-in-active-region nil))
        (org-todo "EDITING"))
      (le::cci-prompt2--insert-state-line "EDITING" nil)
      ;; Belt: `org-todo' can move point; the new heading is the
      ;; buffer's last.
      (goto-char (point-max))
      (org-back-to-heading t)
      (let ((id (or (le::cci-prompt2--heading-flip-stamp)
                    ;; Reading back the just-written line can only fail
                    ;; if it is malformed; stamp "now" as a safety net.
                    (format-time-string
                     le::cci-prompt2--state-stamp-format))))
        (org-edit-headline (if subject (format "%s re: %s" id subject) id))
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert "#+begin_src le::cci-prompt2 -i\n")
        (let ((body-beg (point)))
          (unless (string-empty-p text)
            (insert (org-escape-code-in-string
                     (if (string-suffix-p "\n" text) text (concat text "\n")))))
          (insert "#+end_src\n")
          (save-buffer)
          (cons id body-beg))))))

(defun le::cci-prompt2--goto-own-heading (marker id)
  "Move point to the heading for ID in the current log-file buffer.
MARKER (a copy of the edit buffer's block-begin marker) is the fast
path: when it still points into this buffer and its heading carries
ID, go there.  Falls back to searching for ID from the top -- the
heading may have been hand-moved, or the marker dropped.  Two drafts
opened within the same second share a stamp id (nothing bumps them
apart); the marker keeps each session on its own heading, and the
top-scan fallback hitting the twin instead needs the marker dead too
-- an accepted, vanishingly rare edge.  Returns non-nil iff point
ends up on the heading; point is unchanged otherwise."
  (widen)
  (let ((regexp (concat "^\\* [A-Z]+ " (regexp-quote id)))
        target)
    (when (and marker (eq (marker-buffer marker) (current-buffer)))
      (save-excursion
        (goto-char marker)
        (when (and (ignore-errors (org-back-to-heading t) t)
                   (looking-at regexp))
          (setq target (point)))))
    (unless target
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward regexp nil t)
          (setq target (line-beginning-position)))))
    (when target
      (goto-char target)
      t)))

(defun le::cci-prompt2--set-heading-subject (id subject)
  "Refresh the headline at point to ID plus \"re: SUBJECT\" when non-nil.
`org-edit-headline' rewrites only the title, leaving the TODO keyword
alone."
  (org-edit-headline (if subject (format "%s re: %s" id subject) id)))

(defun le::cci-prompt2--move-heading-to-final-zone ()
  "Move the terminal heading at point to the end of the final-state zone.
The log file keeps two zones, each ascending by headline stamp:
terminal headings (COMMITTED/ABANDONED) first, EDITING drafts at the
tail.  A terminal flip always mints the file's newest terminal stamp,
so re-inserting the subtree just before the first EDITING heading --
or at the end of the buffer when there is none -- keeps both zones
sorted without ever running a general sort.  Concurrent drafts are
what make the move real: committing draft B while an older draft A is
still EDITING lifts B's subtree above A.  Insertion lands at another
heading's line start, strictly before any live edit-session marker
\(those point at block bodies), so markers shift instead of
detaching.  No-op when the subtree is already in place.  Point ends
on the relocated heading."
  (org-back-to-heading t)
  (let* ((beg (point))
         (end (save-excursion (org-end-of-subtree t t) (point)))
         (target (save-excursion
                   (goto-char (point-min))
                   (if (re-search-forward "^\\* EDITING " nil t)
                       (line-beginning-position)
                     (point-max)))))
    (unless (= target end)
      (let ((subtree (buffer-substring-no-properties beg end)))
        (unless (string-suffix-p "\n" subtree)
          (setq subtree (concat subtree "\n")))
        (delete-region beg end)
        (goto-char (if (> target end) (- target (- end beg)) target))
        (unless (bolp) (insert "\n"))
        (insert subtree)
        (goto-char (- (point) (length subtree)))))))

(defun le::cci-prompt2--flip-state-and-save (state)
  "Set the heading at point to STATE, write its LOGBOOK line, refresh
the headline's stamp to the new flip's, move the subtree into the
final-state zone, and save.
`org-todo' flips the keyword with logging inhibited; the state-change
line is written by `le::cci-prompt2--insert-state-line' (see it for
why org's own note machinery is unusable in this never-displayed
buffer).  `org-loop-over-headlines-in-active-region' pins `org-todo'
to the heading at point even when the buffer has an active region
\(see `le::cci-prompt2--insert-heading').  Afterward the headline's
leading bracketed stamp is rewritten to the just-written flip's,
preserving any \"re: SUBJECT\" tail -- the headline always shows the
most recent state change, identical to the top LOGBOOK line.  STATE
is always terminal here, so the subtree then moves to the final-state
zone's end (see `le::cci-prompt2--move-heading-to-final-zone')."
  (org-back-to-heading t)
  (let ((prev (substring-no-properties (or (org-get-todo-state) ""))))
    (let ((org-inhibit-logging t)  ; line written below, not by org
          (org-loop-over-headlines-in-active-region nil))
      (org-todo state))
    (org-back-to-heading t)
    (le::cci-prompt2--insert-state-line
     state (and (not (string-empty-p prev)) prev)))
  (when-let* ((stamp (le::cci-prompt2--heading-flip-stamp)))
    (org-edit-headline
     (replace-regexp-in-string "\\`\\[[^]\n]+\\]"
                               stamp
                               (substring-no-properties
                                (org-get-heading t t t t))
                               t t)))
  (le::cci-prompt2--move-heading-to-final-zone)
  (save-buffer))

(defun le::cci-prompt2--count-editing-siblings (file-buf own-id)
  "Count EDITING headings in FILE-BUF other than OWN-ID's."
  (with-current-buffer file-buf
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (let ((count 0))
          (while (re-search-forward
                  "^\\* EDITING \\(\\[[^]\n]+\\]\\)" nil t)
            (unless (equal (match-string-no-properties 1) own-id)
              (cl-incf count)))
          count)))))

(provide 'le-cci-prompt2-log)
;;; le-cci-prompt2-log.el ends here
