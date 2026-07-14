;;; le-cci-prompt2.el --- Claude Code prompt editor with org-file history  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Le Wang
;; Keywords: tools
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; v2 of the Claude Code prompt composer (v1: le-cci-edit-prompt.el,
;; kept in-tree as reference).  Every prompt is a level-1 org heading
;; in the project's .le-playground/prompts/<ts>--claude-code-prompt.org
;; file, written to disk the moment editing starts, so drafts survive
;; crashes and history survives restarts.  Per-file TODO states record
;; each prompt's fate: EDITING -> COMMITTED (sent) or ABANDONED (kept
;; on cancel); a cancelled draft can also be deleted outright.
;;
;; Editing happens in a real `org-edit-special' src-edit buffer: the
;; block language `le::cci-prompt2' resolves to `le::cci-prompt2-mode'
;; via `org-src-get-lang-mode's "-mode" suffixing, with no
;; `org-src-lang-modes' entry.  C-c ' commits (send + COMMITTED),
;; C-c C-k cancels -- the same muscle memory as org-src, whose own
;; bindings are shadowed per-buffer via
;; `minor-mode-overriding-map-alist' (no advice anywhere).  M-p/M-n
;; recall earlier prompts from the history file.
;;
;; The CCI-session targeting, region/subject capture, and window-focus
;; behavior are copied from v1 rather than shared, so v1 can later be
;; deleted without untangling.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-src)

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

;;;; History file management

(defun le::cci-prompt2--denote-id (&optional time)
  "Return a denote-style timestamp ID for TIME (default: now)."
  (format-time-string "%Y%m%dT%H%M%S" time))

(defun le::cci-prompt2--filetag (root)
  "Return an org-tag-safe token identifying ROOT.
Org tags allow only alphanumerics, _, @, #, and % -- every other
character of the expanded path is replaced with an underscore."
  (replace-regexp-in-string "[^[:alnum:]_@#%]" "_"
                            (directory-file-name (expand-file-name root))))

(defun le::cci-prompt2--create-history-file (dir root)
  "Create a new prompt-history org file in DIR for project ROOT.
Writes denote-style front matter by hand (denote itself is not a
dependency), including the per-file `#+todo:' state sequence so the
EDITING/COMMITTED/ABANDONED keywords are live as soon as the file is
first visited.  Returns the new file's path."
  (let* ((id (le::cci-prompt2--denote-id))
         (path (expand-file-name (concat id "--claude-code-prompt.org") dir)))
    (with-temp-file path
      (insert "#+title:      claude-code-prompt\n"
              (format "#+date:       %s\n"
                      (format-time-string "[%Y-%m-%d %a %H:%M]"))
              (format "#+filetags:   :%s:\n" (le::cci-prompt2--filetag root))
              (format "#+identifier: %s\n" id)
              "#+todo: EDITING(e!) | COMMITTED(c!) ABANDONED(a!)\n\n"))
    path))

(defun le::cci-prompt2--history-file (root)
  "Return the newest prompt-history file for project ROOT, creating one
when none exists.  `.le-playground/' is a hard prerequisite; the
prompts/ subdirectory is created on demand.  Newest is by filename --
denote-style timestamp prefixes sort chronologically."
  (let ((playground (expand-file-name ".le-playground" root)))
    (unless (file-directory-p playground)
      (user-error "No .le-playground/ in %s -- run /playground-setup first" root))
    (let ((dir (expand-file-name "prompts" playground)))
      (make-directory dir t)
      (or (car (last (directory-files
                      dir t
                      "\\`[0-9]\\{8\\}T[0-9]\\{6\\}--claude-code-prompt\\.org\\'")))
          (le::cci-prompt2--create-history-file dir root)))))

;;;; Heading bookkeeping (all run in the history file buffer)

(defun le::cci-prompt2--unique-heading-id (file-buf)
  "Return a denote-style ID unused by any heading in FILE-BUF.
Same-second invocations collide on the timestamp; bump one second
forward until the ID is free."
  (with-current-buffer file-buf
    (save-excursion
      (save-restriction
        (widen)
        (let* ((time (current-time))
               (id (le::cci-prompt2--denote-id time)))
          (goto-char (point-min))
          (while (re-search-forward
                  (format "^\\* [A-Z]+ %s\\_>" (regexp-quote id)) nil t)
            (setq time (time-add time 1)
                  id (le::cci-prompt2--denote-id time))
            (goto-char (point-min)))
          id)))))

(defun le::cci-prompt2--extract-subject (text)
  "Return the subject from TEXT's leading \"re: SUBJECT\" line, or nil."
  (when (string-match "\\`re: \\(.+\\)$" text)
    (string-trim (match-string 1 text))))

(defun le::cci-prompt2--insert-heading (file-buf id subject text)
  "Append a new EDITING heading for ID to FILE-BUF and save the file.
SUBJECT, when non-nil, goes onto the headline as \"re: SUBJECT\".
The heading is inserted without a keyword and put into EDITING via
`org-todo', so org's state-change machinery records the initial state
in the LOGBOOK per the file's `(e!)' spec; literal keyword text would
bypass the logging.
TEXT seeds the src block body, escaped, with a trailing newline
guaranteed.  The block carries the -i (preserve indentation) switch --
without it, write-back would indent every line by
`org-edit-src-content-indentation'.  The save is the crash-recovery
point: the draft is on disk before the edit buffer even opens.
Returns the buffer position of the block body's start."
  (with-current-buffer file-buf
    (widen)
    (save-excursion
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert (format "* %s%s\n" id
                      (if subject (concat " re: " subject) "")))
      (save-excursion
        (forward-line -1)
        (org-todo "EDITING")
        (when (bound-and-true-p org-log-setup)
          (org-add-log-note)))
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert "#+begin_src le::cci-prompt2 -i\n")
      (let ((body-beg (point)))
        (unless (string-empty-p text)
          (insert (org-escape-code-in-string
                   (if (string-suffix-p "\n" text) text (concat text "\n")))))
        (insert "#+end_src\n")
        (save-buffer)
        body-beg))))

(defun le::cci-prompt2--goto-own-heading (marker id)
  "Move point to the heading for ID in the current history-file buffer.
MARKER (a copy of the edit buffer's block-begin marker) is the fast
path: when it still points into this buffer and its heading carries
ID, go there.  Falls back to searching for ID from the top -- the
heading may have been hand-moved, or the marker dropped.  Returns
non-nil iff point ends up on the heading; point is unchanged
otherwise."
  (widen)
  (let ((regexp (format "^\\* [A-Z]+ %s\\_>" (regexp-quote id)))
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

(defun le::cci-prompt2--flip-state-and-save (state)
  "Set the heading at point to STATE, flush its log entry, and save.
With `!' logging, `org-todo' defers the state-change line to the
global `post-command-hook' via `org-add-log-note' -- which never
fires inside a command, so flush it by hand or the save would miss
it.  `org-log-setup' is the pending-note flag; checking
`post-command-hook' membership instead would be defeated by this
buffer's buffer-local hook value shadowing the global one."
  (org-todo state)
  (when (bound-and-true-p org-log-setup)
    (org-add-log-note))
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
                  "^\\* EDITING \\([0-9]\\{8\\}T[0-9]\\{6\\}\\)\\_>" nil t)
            (unless (equal (match-string-no-properties 1) own-id)
              (cl-incf count)))
          count)))))

;;;; Major mode and keymaps

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
browse other prompts in the history file.")

(defvar-keymap le::cci-prompt2--edit-map
  :doc "Overrides for `org-src-mode-map' in prompt edit buffers.
Installed per-buffer via `minor-mode-overriding-map-alist' -- as a
minor-mode map, `org-src-mode-map' would otherwise shadow any
major-mode binding of these keys.  The parent keeps the rest of
org-src-mode's bindings (notably C-x C-s = `org-edit-src-save', i.e.
write back and save the history file mid-edit) reachable."
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
  (root nil :documentation "Expanded project root the prompt targets.")
  (heading-id nil :documentation "Denote-style ID of this prompt's heading.")
  (file-path nil :documentation "Path of the history file the heading lives in.")
  (hist-entries nil :documentation "History plists (:id :state :text), newest first; nil when not navigating.")
  (hist-position nil :documentation "Current index into hist-entries, or nil.")
  (hist-saved-input nil :documentation "Draft text stashed before history navigation.")
  (saved-winconf nil :documentation "Window configuration saved before the edit buffer opened.")
  (header-line-default nil :documentation "Default header line, restored after history navigation."))

(defvar-local le::cci-prompt2--st nil
  "Prompt edit buffer state, a `le::cci-prompt2--state' struct.")

(defvar-local le::cci-prompt2--return-buffer nil
  "Buffer to reselect on commit/cancel, in preference to the saved
window-configuration restore, when that buffer still has a live
window.  nil for edit buffers opened the normal interactive way.
Set externally by callers that pop this buffer up over an unrelated
window layout (e.g. `le::cci-prompt2--claude-prompt-file-setup', which
resolves this from a `claude-prompt-*.md' temp file with no
meaningful window configuration of its own).")

(defun le::cci-prompt2--header-line (cci-name siblings)
  "Return the default header line for CCI-NAME's prompt edit buffer.
SIBLINGS is the number of other EDITING headings at open time."
  (format " Prompt for %s -- %d other EDITING    (C-c ' send, C-c C-k cancel, M-p/M-n history)"
          cci-name siblings))

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

(defun le::cci-prompt2--collect-entries ()
  "Collect history entries from this edit buffer's history file.
Returns plists (:id ID :state STATE :text TEXT), newest first.  The
current heading is excluded; EDITING headings whose block is open in
another edit buffer are skipped with a message, since recalling them
here would fork an in-progress draft."
  (let* ((st le::cci-prompt2--st)
         (own-id (le::cci-prompt2--state-heading-id st))
         (src-buf (marker-buffer org-src--beg-marker)))
    (unless (buffer-live-p src-buf)
      (user-error "History file buffer was killed; reopen %s"
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
                    "^\\* \\([A-Z]+\\) \\([0-9]\\{8\\}T[0-9]\\{6\\}\\)\\_>" nil t)
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
                                     heading-beg subtree-end)))
                    (push (list :id id :state state :text text) entries))))
                (goto-char subtree-end))))))
      (when (> skipped 0)
        (message "Skipped %d EDITING heading(s) open in other edit buffers" skipped))
      entries)))

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
                  (plist-get entry :state) (plist-get entry :id)))))

(defun le::cci-prompt2-history-previous ()
  "Show the previous (older) prompt from the history file.
The first invocation stashes the in-progress draft;
\\[le::cci-prompt2-history-next] past the newest entry restores it."
  (interactive)
  (let ((st le::cci-prompt2--st))
    (unless (and st (org-src-edit-buffer-p))
      (user-error "Not in a CCI prompt edit buffer"))
    (cond
     ((null (le::cci-prompt2--state-hist-position st))
      (let ((entries (le::cci-prompt2--collect-entries)))
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
\\[le::cci-prompt2-history-previous] re-reads the history file."
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

(defun le::cci-prompt2--restore-window-on-exit (return-buf winconf)
  "Select RETURN-BUF's window if it still has one, else restore WINCONF.
Called after a commit or cancel has torn down the edit buffer."
  (if-let* ((return-buf)
            ((buffer-live-p return-buf))
            (win (get-buffer-window return-buf)))
      (select-window win)
    (when winconf (set-window-configuration winconf))))

(defun le::cci-prompt2--edit-context ()
  "Validate the current buffer as a live prompt edit buffer and
snapshot everything commit/cancel need: a plist of the state fields
:cci-buf :root :id :winconf, plus :return-buf, the trimmed draft
:text, and :marker -- a fresh copy of the block-begin marker, taken
now because org nils its own markers on exit.  The caller clears the
copy when done."
  (let ((st le::cci-prompt2--st))
    (unless (and (derived-mode-p 'le::cci-prompt2-mode)
                 (org-src-edit-buffer-p)
                 st)
      (user-error "Not in a CCI prompt edit buffer"))
    (unless (buffer-live-p (marker-buffer org-src--beg-marker))
      (user-error "History file buffer was killed; draft only exists in this buffer"))
    (list :cci-buf (le::cci-prompt2--state-cci-buffer st)
          :root (le::cci-prompt2--state-root st)
          :id (le::cci-prompt2--state-heading-id st)
          :winconf (le::cci-prompt2--state-saved-winconf st)
          :return-buf le::cci-prompt2--return-buffer
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
         (root (plist-get ctx :root))
         (id (plist-get ctx :id))
         (mk (plist-get ctx :marker))
         (subject (le::cci-prompt2--extract-subject text)))
    (when (string-empty-p text)
      (set-marker mk nil)
      (user-error "Empty prompt -- C-c C-k to discard"))
    (let ((org-src-window-setup 'current-window))
      (org-edit-src-exit))
    ;; Current buffer is now the history file, write-back applied.
    (let ((found (le::cci-prompt2--goto-own-heading mk id)))
      (if found
          (le::cci-prompt2--set-heading-subject id subject)
        (message "Heading %s not found in history file; state not updated" id))
      (save-buffer)
      (condition-case err
          (progn
            (if (buffer-live-p cci-buf)
                (with-current-buffer cci-buf
                  (claude-code-ide-send-prompt text))
              (let ((default-directory root))
                (claude-code-ide-send-prompt text)))
            (when (and found (le::cci-prompt2--goto-own-heading mk id))
              (le::cci-prompt2--flip-state-and-save "COMMITTED"))
            (message "Prompt sent to %s"
                     (if (buffer-live-p cci-buf) (buffer-name cci-buf) root)))
        (error
         (message "Send failed (%s); draft kept as EDITING -- M-p recalls it"
                  (error-message-string err)))))
    (set-marker mk nil)
    (le::cci-prompt2--restore-window-on-exit
     (plist-get ctx :return-buf) (plist-get ctx :winconf))))

(defun le::cci-prompt2-cancel ()
  "Cancel the edit.
A non-empty draft can be kept in the history file as ABANDONED;
answering no -- or an empty draft -- deletes its heading outright.
The question comes before any teardown, so \\[keyboard-quit] aborts
with the edit buffer untouched."
  (interactive)
  (let* ((ctx (le::cci-prompt2--edit-context))
         (text (plist-get ctx :text))
         (id (plist-get ctx :id))
         (mk (plist-get ctx :marker))
         (subject (le::cci-prompt2--extract-subject text))
         (keep (and (not (string-empty-p text))
                    (y-or-n-p "Keep draft in history as ABANDONED? (n deletes it) "))))
    (if keep
        (progn
          (let ((org-src-window-setup 'current-window))
            (org-edit-src-exit))
          (if (le::cci-prompt2--goto-own-heading mk id)
              (progn
                (le::cci-prompt2--set-heading-subject id subject)
                (le::cci-prompt2--flip-state-and-save "ABANDONED"))
            (message "Heading %s not found in history file; state not updated" id)
            (save-buffer)))
      (let ((org-src-window-setup 'current-window))
        (org-edit-src-abort))
      (with-current-buffer (marker-buffer mk)
        (if (le::cci-prompt2--goto-own-heading mk id)
            (progn
              (delete-region (point)
                             (save-excursion (org-end-of-subtree t t) (point)))
              (save-buffer))
          (message "Heading %s not found in history file; nothing deleted" id))))
    (set-marker mk nil)
    (le::cci-prompt2--restore-window-on-exit
     (plist-get ctx :return-buf) (plist-get ctx :winconf))))

;;;; Main entry point

;;;###autoload
(defun le::cci-prompt2-edit (force-choose &optional initial-text)
  "Open an org src edit buffer for composing a Claude Code prompt.
Every invocation appends a fresh EDITING heading to the project's
prompt-history file (created on demand under .le-playground/prompts/)
and opens its block via `org-edit-src-code' -- the draft is on disk
from the first moment.  C-c \\=' sends it and marks the heading
COMMITTED; C-c C-k cancels, optionally keeping it as ABANDONED;
M-p/M-n recall other prompts from the file.

With prefix argument FORCE-CHOOSE, prompt for a CCI session and save
the choice as a buffer-local override.  INITIAL-TEXT, if non-nil,
seeds the draft.  Returns the edit buffer."
  (interactive "P")
  (let* ((src-buf (current-buffer))
         (winconf (current-window-configuration))
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
           (path (le::cci-prompt2--history-file root))
           (file-buf (find-file-noselect path))
           (id (le::cci-prompt2--unique-heading-id file-buf))
           (body-pos (le::cci-prompt2--insert-heading
                      file-buf id subject (car content)))
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
                 :root root
                 :heading-id id
                 :file-path path
                 :saved-winconf winconf
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
           (text (buffer-string))
           (editor-buf (if cci-buf
                           (with-current-buffer cci-buf
                             (le::cci-prompt2-edit nil text))
                         (le::cci-prompt2-edit nil text))))
      (with-current-buffer prompt-buf
        (erase-buffer)
        (save-buffer))
      (when (and cci-buf (buffer-live-p editor-buf))
        (with-current-buffer editor-buf
          (setq-local le::cci-prompt2--return-buffer cci-buf)))
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
there is no draft-clobbering hazard to guard against (unlike
`le::vterm-send-C-g')."
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
