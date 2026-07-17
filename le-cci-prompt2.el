;;; le-cci-prompt2.el --- Claude Code prompt editor with an org-file prompt log  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Le Wang
;; Keywords: tools
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; v2 of the Claude Code prompt composer (v1: le-cci-edit-prompt.el,
;; removed at 2f0617e).  Every prompt is a level-1 org heading
;; in the project's prompt-log file --
;; .le-playground/denote/prompt-log/<ts>--claude-code-prompt-log.org --
;; written to disk the moment editing starts, so drafts survive
;; crashes and the log survives restarts.  Per-file TODO states record
;; each prompt's fate: EDITING -> COMMITTED (sent) or ABANDONED (kept
;; on cancel); a cancelled draft can also be deleted outright.  Each
;; headline is titled with the bracketed timestamp of its latest
;; state flip, mirroring the top LOGBOOK line.  Headings sit in two
;; zones, each ascending by that stamp: terminal ones
;; (COMMITTED/ABANDONED) first, EDITING drafts at the file's tail --
;; new drafts append, terminal flips relocate to the zone boundary.
;; The log file is a real denote note: `denote' creates it (denote is
;; a hard dependency now) inside the playground's denote silo,
;; .le-playground/denote/, which /playground-setup bootstraps along
;; with a .dir-locals.el scoping interactive denote commands there.
;;
;; Editing happens in a real `org-edit-special' src-edit buffer: the
;; block language `le::cci-prompt2' resolves to `le::cci-prompt2-mode'
;; via `org-src-get-lang-mode's "-mode" suffixing, with no
;; `org-src-lang-modes' entry.  C-c ' commits (send + COMMITTED),
;; C-c C-k cancels -- the same muscle memory as org-src, whose own
;; bindings are shadowed per-buffer via
;; `minor-mode-overriding-map-alist' (no advice anywhere).  M-p/M-n
;; recall earlier prompts from the log file, merged with the
;; project's entries in Claude Code's own ~/.claude/history.jsonl
;; (pseudo-state CLI) so prompts typed straight into the CLI are
;; recallable too.  A prompt committed here lands in both stores; the
;; merge drops the CLI twin when its timestamp falls within seconds
;; of the heading's COMMITTED flip -- which is why the LOGBOOK stamps
;; carry second precision.  `yank-media' pastes a clipboard image as
;; a denote attachment beside the log file (<ts>--screenshot.png):
;; the prompt text gets a plain "@<path>" mention the CLI resolves at
;; submit, and the log heading a [[denote:<ts>]] line after its
;; block.
;;
;; The CCI-session targeting, region/subject capture, and window-focus
;; behavior are copied from v1 rather than shared, so v1 can later be
;; deleted without untangling.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-src)
(require 'org-duration)
(require 'denote)

(declare-function claude-code-ide--get-working-directory "claude-code-ide")
(declare-function claude-code-ide--get-buffer-name "claude-code-ide")
(declare-function claude-code-ide-send-prompt "claude-code-ide")
(declare-function vterm-send-key "vterm")
(declare-function ghostel-send-C-g "ghostel")
(declare-function server-edit "server")

(defvar claude-code-ide-mcp-server--sessions)
(defvar claude-code-ide--routing-tokens)
(defvar server-buffer-clients)
(defvar server-client-instructions)

;;;; Target CCI session resolution

(defvar-local le::cci-prompt2--target-cci-buffer nil
  "Buffer-local override for the target CCI buffer.
Set when the user picks a CCI session for a buffer that has no
natural mapping.  Persists across `le::cci-prompt2-edit' calls.")

(defun le::cci-prompt2--live-cci-buffers ()
  "Return alist of (BUFFER-NAME . DIRECTORY) for all live CCI sessions."
  (let (result)
    (maphash (lambda (dir tok)
               (when-let* ((session (gethash tok claude-code-ide-mcp-server--sessions))
                            (buf (plist-get session :buffer))
                            ((buffer-live-p buf)))
                 (push (cons (buffer-name buf) dir) result)))
             claude-code-ide--routing-tokens)
    result))

(defun le::cci-prompt2--dir-for-cci-buffer (cci-buf)
  "Return the directory associated with CCI-BUF, or nil."
  (catch 'found
    (maphash (lambda (dir tok)
               (when-let* ((session (gethash tok claude-code-ide-mcp-server--sessions))
                            (buf (plist-get session :buffer))
                            ((eq buf cci-buf)))
                 (throw 'found dir)))
             claude-code-ide--routing-tokens)))

(defun le::cci-prompt2--choose-cci-buffer ()
  "Prompt the user to pick a live CCI buffer.
Default is the one visible in the current frame, if any.
If only one CCI buffer exists, return it without prompting."
  (let* ((alist (le::cci-prompt2--live-cci-buffers))
         (names (mapcar #'car alist)))
    (unless names
      (user-error "No live CCI sessions"))
    (if (= (length names) 1)
        (get-buffer (car names))
      (let* ((visible (seq-find (lambda (entry)
                                  (get-buffer-window (car entry)))
                                alist))
             (default (car visible))
             (chosen (completing-read "CCI session: " names nil t nil nil default)))
        (get-buffer chosen)))))

(defun le::cci-prompt2--resolve-cci-target (force-choose)
  "Resolve the target CCI buffer and its root directory.
When FORCE-CHOOSE is non-nil, always prompt.  When the current
buffer is itself a CCI session buffer, it is the target.
Returns (ROOT . CCI-BUFFER) or signals an error."
  (let (cci-buf)
    (cond
     ;; C-u: always prompt and save
     (force-choose
      (setq cci-buf (le::cci-prompt2--choose-cci-buffer))
      (setq le::cci-prompt2--target-cci-buffer cci-buf))
     ;; Invoked from a CCI session buffer? It is the target.  Resolved via
     ;; the routing-token table, not `default-directory', which is not
     ;; authoritative in terminal buffers.
     ((le::cci-prompt2--dir-for-cci-buffer (current-buffer))
      (setq cci-buf (current-buffer)))
     ;; Saved override still live?
     ((and le::cci-prompt2--target-cci-buffer
           (buffer-live-p le::cci-prompt2--target-cci-buffer))
      (setq cci-buf le::cci-prompt2--target-cci-buffer))
     ;; Default-directory maps to a session?
     (t
      (let* ((root (claude-code-ide--get-working-directory))
             (buf-name (claude-code-ide--get-buffer-name root))
             (buf (get-buffer buf-name)))
        (if buf
            (setq cci-buf buf)
          ;; No match — prompt and save
          (setq cci-buf (le::cci-prompt2--choose-cci-buffer))
          (setq le::cci-prompt2--target-cci-buffer cci-buf)))))
    (let ((dir (le::cci-prompt2--dir-for-cci-buffer cci-buf)))
      (unless dir
        (user-error "Cannot find directory for %s" (buffer-name cci-buf)))
      (cons dir cci-buf))))

;;;; Active-region file reference (@path#beg-end @-mention)

(defun le::cci-prompt2--capture-region-ref ()
  "If the current buffer has an active region and visits a file,
return (:file F :beg B :end E :subject S) with 1-based inclusive line numbers,
else nil.  FILE may not yet exist on disk (e.g. an unsaved buffer) — the
caller is responsible for offering to save it before referencing it.
SUBJECT is the org heading title of the entry containing the region
start (stars, TODO keyword, priority, and tags stripped), or nil when the
buffer is not org-mode or the region starts above the first heading."
  (when (and (use-region-p) buffer-file-name)
    (let* ((rb (region-beginning))
           (re (region-end))
           (beg (line-number-at-pos rb))
           (end (line-number-at-pos re))
           (subject (when (derived-mode-p 'org-mode)
                      (save-excursion
                        (goto-char rb)
                        (unless (org-before-first-heading-p)
                          (org-back-to-heading t)
                          (let ((h (string-trim
                                    (substring-no-properties
                                     (org-get-heading t t t t)))))
                            (unless (string-empty-p h) h)))))))
      ;; If the region ends exactly at a line start, that trailing line isn't
      ;; really selected — exclude it (standard idiom).
      (when (and (> re rb) (save-excursion (goto-char re) (bolp)))
        (setq end (1- end)))
      (list :file buffer-file-name :beg beg :end end :subject subject))))

(defun le::cci-prompt2--point-todo-doing-ancestors ()
  "Return (SUBJECT . POINT) for each heading enclosing point whose TODO
keyword is TODO or DOING, closest first.  SUBJECT has stars, keyword,
priority, and tags stripped; POINT is the heading's buffer position,
used to derive a location reference to it.  Walks from the heading
containing point up through each ancestor to the top, filtering to
just the qualifying ones -- an ancestor without a TODO/DOING keyword is
skipped but does not stop the walk.  Empty (nil) when the buffer isn't
org-mode, point is above the first heading, or no ancestor qualifies."
  (when (and (derived-mode-p 'org-mode) (not (org-before-first-heading-p)))
    (save-excursion
      (org-back-to-heading t)
      (let (candidates)
        (while (progn
                 (when (member (org-get-todo-state) '("TODO" "DOING"))
                   (push (cons (string-trim
                                (substring-no-properties
                                 (org-get-heading t t t t)))
                               (point))
                         candidates))
                 (org-up-heading-safe)))
        (nreverse candidates)))))

(defun le::cci-prompt2--capture-point-subject ()
  "Return a region-ref-shaped plist for the TODO/DOING heading enclosing
point, or nil if none qualifies.  When more than one ancestor heading
qualifies, prompts for which one, defaulting to the closest.  Includes
:file/:beg/:end (the heading's own line, so the eventual @-mention
points straight at it) when the buffer visits a file, else just
:subject.  Used as the fallback to `le::cci-prompt2--capture-region-ref'
when no region is active, so a \"re: SUBJECT\" line and a location
reference still get added based on what task point is currently in."
  (when-let* ((candidates (le::cci-prompt2--point-todo-doing-ancestors)))
    (let* ((chosen (if (cdr candidates)
                        (completing-read "Subject heading: " candidates nil t
                                         nil nil (caar candidates))
                      (caar candidates)))
           (pos (cdr (assoc chosen candidates))))
      (if buffer-file-name
          (let ((line (line-number-at-pos pos)))
            (list :file buffer-file-name :beg line :end line :subject chosen))
        (list :subject chosen)))))

(defun le::cci-prompt2--file-reference (file beg end root)
  "Return the Claude Code @-mention \"@PATH#RANGE\" for FILE lines BEG..END.
PATH is relative to ROOT when FILE is under it, absolute otherwise.
RANGE collapses to a single number when BEG = END."
  (let* ((abs (expand-file-name file))
         (root* (file-name-as-directory (expand-file-name root)))
         (path (if (string-prefix-p root* abs) (substring abs (length root*)) abs))
         (range (if (= beg end) (number-to-string beg) (format "%d-%d" beg end))))
    (format "@%s#%s" path range)))

(defun le::cci-prompt2--compose-prompt-content (base-text region-ref)
  "Return (STRING . POINT) for a prompt buffer's initial content.
BASE-TEXT seeds the composing line (e.g. captured CLI prompt text, an
existing draft, or the empty string).  REGION-REF, when non-nil, is a
plist as returned by `le::cci-prompt2--capture-region-ref' with an
added :ref-string key (the already-formatted @-mention) — its
:subject, if any, becomes a leading \"re: SUBJECT\" line, and
:ref-string is appended after BASE-TEXT (trimmed of trailing
whitespace) separated by a blank line.  POINT lands on the composing
line: after the \"re:\" line when present, otherwise at the top; with
no REGION-REF, POINT is at the end of BASE-TEXT."
  (if region-ref
      (let* ((subject (plist-get region-ref :subject))
             (ref (plist-get region-ref :ref-string))
             (heading (if subject (concat "re: " subject "\n\n") ""))
             (body (concat heading (string-trim-right base-text) "\n\n" ref)))
        (cons body (1+ (length heading))))
    (cons base-text (1+ (length base-text)))))

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

(defconst le::cci-prompt2--time-stamp-formats
  '("%Y-%m-%d %a" . "%Y-%m-%d %a %H:%M:%S")
  "`org-time-stamp-formats' with seconds on the date-time format.
Let-bound around each state flip so the LOGBOOK \"- State\" stamps
carry seconds -- the COMMITTED flip time is deduped against
~/.claude/history.jsonl entry timestamps, and minute precision would
eat most of `le::cci-prompt2--dedup-window'.  A buffer-local or
file-local value cannot do this: `org-store-log-note' renders the
stamp while the *Org Note* scratch buffer is current, where the
log file's local variables are out of scope.")

(defun le::cci-prompt2--heading-flip-stamp ()
  "Return the bracketed timestamp of the most recent state flip
recorded in the LOGBOOK of the heading at point, brackets included --
e.g. \"[2026-07-14 Tue 17:47:45]\" -- or nil when the drawer's top
line is not a \"- State\" entry.  Only the top line is examined:
org prepends the newest note, so right after a flip's log flush the
line at `org-log-beginning' is that flip's.  This is what the
headline mirrors -- after every flip the headline's stamp is
rewritten to this value, so headline time == latest state change by
construction, with no same-second formatting race."
  (save-excursion
    (org-back-to-heading t)
    (goto-char (org-log-beginning))
    (when (looking-at "[ \t]*- State \"[A-Z]+\".*\\(\\[[^]\n]+\\]\\)")
      (match-string-no-properties 1))))

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
`org-todo', so org's state-change machinery records the initial state
in the LOGBOOK per the file's `(e!)' spec; literal keyword text would
bypass the logging.  `org-loop-over-headlines-in-active-region' is
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
      (let ((org-time-stamp-formats le::cci-prompt2--time-stamp-formats)
            (org-loop-over-headlines-in-active-region nil))
        (org-todo "EDITING")
        (when (bound-and-true-p org-log-setup)
          (org-add-log-note)))
      ;; The log-note flush may move point; the new heading is the
      ;; buffer's last.
      (goto-char (point-max))
      (org-back-to-heading t)
      (let ((id (or (le::cci-prompt2--heading-flip-stamp)
                    ;; No log line only when the flip went unlogged
                    ;; (logging misconfigured); stamp "now" instead.
                    (format-time-string
                     (format "[%s]"
                             (cdr le::cci-prompt2--time-stamp-formats))))))
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
  "Set the heading at point to STATE, flush its log entry, refresh
the headline's stamp to the new flip's, move the subtree into the
final-state zone, and save.
With `!' logging, `org-todo' defers the state-change line to the
global `post-command-hook' via `org-add-log-note' -- which never
fires inside a command, so flush it by hand or the save would miss
it.  `org-log-setup' is the pending-note flag; checking
`post-command-hook' membership instead would be defeated by this
buffer's buffer-local hook value shadowing the global one.
The lets give the stamp second precision (see
`le::cci-prompt2--time-stamp-formats') and pin `org-todo' to the
heading at point even when the buffer has an active region (see
`le::cci-prompt2--insert-heading').  Afterward the headline's leading
bracketed stamp is rewritten to the just-logged flip's, preserving
any \"re: SUBJECT\" tail -- the headline always shows the most recent
state change, identical to the top LOGBOOK line.  STATE is always
terminal here, so the subtree then moves to the final-state zone's
end (see `le::cci-prompt2--move-heading-to-final-zone')."
  (org-back-to-heading t)
  (let ((heading (point-marker)))
    (let ((org-time-stamp-formats le::cci-prompt2--time-stamp-formats)
          (org-loop-over-headlines-in-active-region nil))
      (org-todo state)
      (when (bound-and-true-p org-log-setup)
        (org-add-log-note)))
    ;; The log-note flush may move point; the marker survives the
    ;; drawer insertion below the headline.
    (goto-char heading)
    (set-marker heading nil))
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

;;;; Major mode and keymaps

(defun le::cci-prompt2--yank-media-handler (mimetype data)
  "Save image DATA beside the log file and reference it from both sides.
Registered for image/.* in `le::cci-prompt2-mode', replacing the
handler inherited from org, which is doubly wrong here: its
`org-yank-image-save-method' machinery (yours: attach) needs a
file-backed heading this edit buffer doesn't have, and it inserts an
org link the Claude CLI can't read.  The image becomes a denote
attachment of the prompt log -- saved next to the log file as
<id>--screenshot.<ext>, MIMETYPE picking the extension, the id minted
by `denote-get-identifier-function' with silo-wide bump-a-second
uniqueness.  Two references are minted together: the plain-text
\"@<root-relative-path>\" inserted at point, which the CLI resolves
at submit like any other @-mention (`default-directory' here is the
project root), and a [[denote:<id>]] line appended after the
heading's #+end_src in the log file -- outside the block, where
write-back never touches it -- so the log entry stays org-followable.
The log file is saved; the draft body is not (write-back happens at
C-x C-s or commit, as always)."
  (let* ((st (or le::cci-prompt2--st
                 (user-error "Not in a prompt edit buffer")))
         (root (le::cci-prompt2--state-root st))
         (dir (file-name-directory (le::cci-prompt2--state-file-path st)))
         ;; Denote's used-id scan reads `denote-directory'; scope it to
         ;; the playground silo (prompt-log/'s parent), as
         ;; `le::cci-prompt2--create-log-file' does.
         (denote-directory (file-name-directory (directory-file-name dir)))
         (id (funcall denote-get-identifier-function nil (current-time)))
         (subtype (cadr (split-string (symbol-name mimetype) "/")))
         (ext (concat "." (if (equal subtype "svg+xml") "svg" subtype)))
         (path (denote-format-file-name dir id nil "screenshot" ext nil))
         (mk org-src--beg-marker))
    (let ((coding-system-for-write 'no-conversion))
      (write-region data nil path nil 'silent))
    (with-current-buffer (marker-buffer mk)
      (org-with-wide-buffer
       (goto-char mk)
       (org-back-to-heading t)
       (let ((bound (save-excursion (org-end-of-subtree t t) (point))))
         (goto-char mk)
         (unless (re-search-forward "^[ \t]*#\\+end_src" bound t)
           (user-error "No #+end_src under this draft's heading"))
         (forward-line 1)
         (unless (bolp) (insert "\n"))
         ;; Past earlier pastes' link lines: chronological order.
         (while (looking-at "\\[\\[denote:") (forward-line 1))
         (insert (format "[[denote:%s]]\n" id))
         (save-buffer))))
    (unless (or (bolp) (memq (char-before) '(?\s ?\t)))
      (insert " "))
    (insert "@" (file-relative-name path root) " ")))

(defvar-keymap le::cci-prompt2-mode-map
  :doc "Keymap for `le::cci-prompt2-mode'."
  "M-p" #'le::cci-prompt2-history-previous
  "M-n" #'le::cci-prompt2-history-next)

(define-derived-mode le::cci-prompt2-mode org-mode "CCI-Prompt2"
  "Major mode for composing Claude Code prompts in org src blocks.
The block language `le::cci-prompt2' resolves here via
`org-src-get-lang-mode's \"-mode\" suffixing, so `org-edit-src-code'
picks this mode with no `org-src-lang-modes' entry.  C-c \\=' commits
\(sends the prompt), C-c C-k cancels;
\\[le::cci-prompt2-history-previous]/\\[le::cci-prompt2-history-next] \
browse earlier prompts (log file merged with CLI history).
\\[yank-media] pastes a clipboard image as a denote attachment beside
the log file and @-mentions it in the prompt."
  ;; Same TYPES key as org's own registration, so this *replaces* the
  ;; inherited image handler in these buffers (`yank-media-handler'
  ;; does setf alist-get on equal keys); org-mode buffers keep theirs.
  (yank-media-handler "image/.*" #'le::cci-prompt2--yank-media-handler))

(defvar-keymap le::cci-prompt2--edit-map
  :doc "Overrides for `org-src-mode-map' in prompt edit buffers.
Installed per-buffer via `minor-mode-overriding-map-alist' -- as a
minor-mode map, `org-src-mode-map' would otherwise shadow any
major-mode binding of these keys.  The parent keeps the rest of
org-src-mode's bindings (notably C-x C-s = `org-edit-src-save', i.e.
write back and save the log file mid-edit) reachable."
  :parent org-src-mode-map
  "C-c '" #'le::cci-prompt2-commit
  "C-c C-k" #'le::cci-prompt2-cancel)

(defun le::cci-prompt2--src-mode-setup ()
  "Shadow org-src-mode's exit keys in prompt edit buffers.
On `org-src-mode-hook'; a no-op for src buffers of any other language."
  (when (derived-mode-p 'le::cci-prompt2-mode)
    (push (cons 'org-src-mode le::cci-prompt2--edit-map)
          minor-mode-overriding-map-alist)))

(add-hook 'org-src-mode-hook #'le::cci-prompt2--src-mode-setup)

;;;; Buffer-local state

(cl-defstruct (le::cci-prompt2--state (:copier nil))
  "State for a prompt edit buffer."
  (cci-buffer nil :documentation "Target CCI session buffer.")
  (origin-window nil :documentation "Window selected when this edit buffer opened; focus returns here on commit/cancel.  For a C-M-g handoff the setup hook selects the CCI window first, so this captures that rather than the throwaway temp-file window.")
  (root nil :documentation "Expanded project root the prompt targets.")
  (heading-id nil :documentation "Bracketed flip-stamp id of this prompt's heading, e.g. \"[2026-07-14 Tue 17:47:45]\" -- its EDITING flip time, stable for the whole edit session (the headline only changes again at the terminal flip, after which nothing looks it up).")
  (file-path nil :documentation "Path of the log file the heading lives in.")
  (hist-entries nil :documentation "History plists (:id :state :text :ts), newest first; nil when not navigating.")
  (hist-position nil :documentation "Current index into hist-entries, or nil.")
  (hist-saved-input nil :documentation "Draft text stashed before history navigation.")
  (header-line-default nil :documentation "Default header line, restored after history navigation."))

(defvar-local le::cci-prompt2--st nil
  "Prompt edit buffer state, a `le::cci-prompt2--state' struct.")

(defun le::cci-prompt2--header-line (cci-name siblings)
  "Return the default header line for CCI-NAME's prompt edit buffer.
SIBLINGS is the number of other EDITING headings at open time."
  (format " Prompt for %s -- %d other EDITING    (C-c ' send, C-c C-k cancel, M-p/M-n history)"
          cci-name siblings))

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
`le::cci-prompt2--time-stamp-formats'), and for a COMMITTED heading
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

;;;; Commit / cancel

(defun le::cci-prompt2--restore-window (window)
  "Select WINDOW if it is still live; no-op otherwise.
WINDOW is whatever was selected when the edit buffer opened, captured
by `le::cci-prompt2-edit'.  Restoring it on commit or cancel lands
focus back where you were before composing -- the source buffer you
invoked from, or the CCI session -- with no CCI-specific logic here:
the one rule is \"put focus back\".  (A C-M-g handoff pre-selects the
CCI window in `le::cci-prompt2--claude-prompt-file-setup' so the
captured window is the session, not the throwaway temp-file window.)
This is `select-window' on a window object, never
`set-window-configuration', so the restored window's `window-start' is
untouched -- which is what keeps the live CCI window from being
scrolled back on send.  When WINDOW is dead (deleted since capture),
focus is left where the exit put it."
  (when (window-live-p window)
    (select-window window)))

(defun le::cci-prompt2--edit-context ()
  "Validate the current buffer as a live prompt edit buffer and
snapshot everything commit/cancel need: a plist of the state fields
:cci-buf :root :id :origin-window, the trimmed draft
:text, and :marker -- a fresh copy of the block-begin marker, taken
now because org nils its own markers on exit.  The caller clears the
copy when done."
  (let ((st le::cci-prompt2--st))
    (unless (and (derived-mode-p 'le::cci-prompt2-mode)
                 (org-src-edit-buffer-p)
                 st)
      (user-error "Not in a CCI prompt edit buffer"))
    (unless (buffer-live-p (marker-buffer org-src--beg-marker))
      (user-error "Log file buffer was killed; draft only exists in this buffer"))
    (list :cci-buf (le::cci-prompt2--state-cci-buffer st)
          :root (le::cci-prompt2--state-root st)
          :id (le::cci-prompt2--state-heading-id st)
          :origin-window (le::cci-prompt2--state-origin-window st)
          :text (string-trim (buffer-string))
          :marker (copy-marker org-src--beg-marker))))

(defun le::cci-prompt2-commit ()
  "Send the draft to the CCI session and mark its heading COMMITTED.
The write-back and file save happen first, while the heading is still
EDITING, so a failed send leaves a durable draft; only a successful
send flips the state."
  (interactive)
  (let* ((ctx (le::cci-prompt2--edit-context))
         (text (plist-get ctx :text))
         (cci-buf (plist-get ctx :cci-buf))
         (origin-window (plist-get ctx :origin-window))
         (root (plist-get ctx :root))
         (id (plist-get ctx :id))
         (mk (plist-get ctx :marker))
         (subject (le::cci-prompt2--extract-subject text)))
    (when (string-empty-p text)
      (set-marker mk nil)
      (user-error "Empty prompt -- C-c C-k to discard"))
    (let ((org-src-window-setup 'current-window))
      (org-edit-src-exit))
    ;; Current buffer is now the log file, write-back applied.
    (let* ((log-buf (current-buffer))
           (found (le::cci-prompt2--goto-own-heading mk id)))
      (if found
          (le::cci-prompt2--set-heading-subject id subject)
        (message "Heading %s not found in log file; state not updated" id))
      (save-buffer)
      ;; Restore the pre-edit window *before* the send.  `send-prompt' does
      ;; a `sit-for' that forces a redisplay mid-command; restoring first
      ;; means that redisplay already shows the window you came from instead
      ;; of flashing through the log-file window this exit left selected.
      (le::cci-prompt2--restore-window origin-window)
      (condition-case err
          (progn
            (if (buffer-live-p cci-buf)
                (with-current-buffer cci-buf
                  (claude-code-ide-send-prompt text))
              (let ((default-directory root))
                (claude-code-ide-send-prompt text)))
            (with-current-buffer log-buf
              (when (and found (le::cci-prompt2--goto-own-heading mk id))
                (le::cci-prompt2--flip-state-and-save "COMMITTED")))
            (message "Prompt sent to %s"
                     (if (buffer-live-p cci-buf) (buffer-name cci-buf) root)))
        (error
         (message "Send failed (%s); draft kept as EDITING -- M-p recalls it"
                  (error-message-string err)))))
    (set-marker mk nil)))

(defun le::cci-prompt2-cancel ()
  "Cancel the edit.
A non-empty draft prompts for its fate: `a' abandons it, keeping the
heading in the log as ABANDONED; `k' kills it, deleting the heading
outright.  An empty draft is always killed.  The question comes
before any teardown, so \\[keyboard-quit] aborts with the edit
buffer untouched."
  (interactive)
  (let* ((ctx (le::cci-prompt2--edit-context))
         (text (plist-get ctx :text))
         (id (plist-get ctx :id))
         (mk (plist-get ctx :marker))
         (subject (le::cci-prompt2--extract-subject text))
         (keep (and (not (string-empty-p text))
                    (eq ?a (read-char-choice
                            "Draft: [a]bandon (keep in log) or [k]ill (delete)? "
                            '(?a ?k))))))
    (if keep
        (progn
          (let ((org-src-window-setup 'current-window))
            (org-edit-src-exit))
          (if (le::cci-prompt2--goto-own-heading mk id)
              (progn
                (le::cci-prompt2--set-heading-subject id subject)
                (le::cci-prompt2--flip-state-and-save "ABANDONED"))
            (message "Heading %s not found in log file; state not updated" id)
            (save-buffer)))
      (let ((org-src-window-setup 'current-window))
        (org-edit-src-abort))
      (with-current-buffer (marker-buffer mk)
        (if (le::cci-prompt2--goto-own-heading mk id)
            (progn
              (delete-region (point)
                             (save-excursion (org-end-of-subtree t t) (point)))
              (save-buffer))
          (message "Heading %s not found in log file; nothing deleted" id))))
    (set-marker mk nil)
    (le::cci-prompt2--restore-window (plist-get ctx :origin-window))))

;;;; Main entry point

;;;###autoload
(defun le::cci-prompt2-edit (force-choose &optional initial-text)
  "Open an org src edit buffer for composing a Claude Code prompt.
Every invocation appends a fresh EDITING heading to the project's
prompt-log file (created on demand under
.le-playground/denote/prompt-log/) and opens its block via
`org-edit-src-code' -- the draft is on disk from the first moment.
C-c \\=' sends it and marks the heading COMMITTED; C-c C-k cancels,
optionally keeping it as ABANDONED; M-p/M-n recall earlier prompts
\(the log merged with CLI history).

With prefix argument FORCE-CHOOSE, prompt for a CCI session and save
the choice as a buffer-local override.  INITIAL-TEXT, if non-nil,
seeds the draft.  Returns the edit buffer."
  (interactive "P")
  (let* ((origin-window (selected-window))
         (src-buf (current-buffer))
         (region-ref (or (le::cci-prompt2--capture-region-ref)
                         (le::cci-prompt2--capture-point-subject)))
         (target (le::cci-prompt2--resolve-cci-target force-choose))
         (root (car target))
         (cci-buf (cdr target)))
    (when-let* ((file (plist-get region-ref :file)))
      ;; Offer to save the source buffer before referencing it on disk —
      ;; whether it's never been saved, or has unsaved edits.  Absent
      ;; entirely (not just nil) when region-ref came from
      ;; `le::cci-prompt2--capture-point-subject', which has no file range
      ;; to offer.
      (with-current-buffer src-buf
        (cond
         ((not (file-exists-p file))
          (when (y-or-n-p (format "%s is not saved to disk.  Save now to add the reference? " (buffer-name)))
            (save-buffer)))
         ((buffer-modified-p)
          (when (y-or-n-p (format "Save %s before referencing? " (buffer-name)))
            (save-buffer)))))
      (if (file-exists-p file)
          (setq region-ref
                (plist-put region-ref :ref-string
                           (le::cci-prompt2--file-reference
                            file
                            (plist-get region-ref :beg)
                            (plist-get region-ref :end)
                            root)))
        (setq region-ref nil)))
    (let* ((content (le::cci-prompt2--compose-prompt-content
                     (or initial-text "") region-ref))
           (subject (le::cci-prompt2--extract-subject (car content)))
           (path (le::cci-prompt2--log-file root))
           (file-buf (find-file-noselect path))
           (inserted (le::cci-prompt2--insert-heading
                      file-buf subject (car content)))
           (id (car inserted))
           (body-pos (cdr inserted))
           (siblings (le::cci-prompt2--count-editing-siblings file-buf id))
           edit-buf)
      (with-current-buffer file-buf
        (goto-char body-pos)
        (let ((org-src-window-setup 'other-window))
          (org-edit-src-code nil (format "*cci-prompt2: %s*"
                                         (file-name-nondirectory
                                          (directory-file-name
                                           (buffer-local-value
                                            'default-directory cci-buf))))))
        ;; `org-edit-src-code' pops to the edit buffer, making it current
        ;; for the rest of this body.
        (setq edit-buf (current-buffer)))
      (with-current-buffer edit-buf
        (setq default-directory root)
        (let ((hdr (le::cci-prompt2--header-line (buffer-name cci-buf) siblings)))
          (setq le::cci-prompt2--st
                (make-le::cci-prompt2--state
                 :cci-buffer cci-buf
                 :origin-window origin-window
                 :root root
                 :heading-id id
                 :file-path path
                 :header-line-default hdr))
          (setq header-line-format hdr))
        (goto-char (min (cdr content) (point-max))))
      edit-buf)))

;;;; C-M-g pipeline (Claude CLI temp-file handoff)

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
(defun le::cci-prompt2--claude-prompt-file-setup ()
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
(add-hook 'server-switch-hook #'le::cci-prompt2--claude-prompt-file-setup)

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

(provide 'le-cci-prompt2)
;;; le-cci-prompt2.el ends here
