;;; le-denote.el --- Denote helpers: media capture + org-subtree export  -*- lexical-binding: t; -*-

;;; Commentary:

;; Clipboard media -> a denote-named file, factored into a kernel that two
;; callers share.
;;
;; `le::denote-write-media' is the kernel: given a directory, a MIME type, and
;; the raw selection data, it mints a denote identifier, picks the extension
;; from the MIME subtype, writes the bytes to <id>--<title>.<ext>, and returns
;; (PATH . ID).  It is scope-agnostic: the identifier's used-id "bump-a-second"
;; scan reads `denote-directory', so a caller that wants the scan scoped to a
;; particular silo simply `let'-binds `denote-directory' around the call.
;;
;; `le::denote-yank-media' is the org-facing command built on the kernel: it
;; reads a clipboard image, saves it BESIDE the current file (that file's own
;; directory, not `denote-directory'), and inserts a [[denote:<id>]] link at
;; point.  The link resolves wherever the current file's directory is covered
;; by the active `denote-directory' (i.e. inside a denote silo).
;;
;; `le-cci-prompt2's own `yank-media-handler' calls the same kernel and then
;; does its own two-sided referencing (an @-mention for the Claude CLI plus a
;; [[denote:<id>]] line in the log file); see le-cci-prompt2.el.
;;
;; `le::denote-plan-path' is a pure NAME ORACLE for the `dnc' CLI: given a
;; target directory it mints an id and returns the sluggified denote path,
;; writing nothing.  The CLI (which finds the silo itself) passes an absolute
;; directory so naming never depends on this session's ambient project; `dnc'
;; authors the file body.
;;
;; `le::denote-logbook-append' is the org-surgery half of `dnc logbook': it
;; appends a well-formed, 🤖-tagged org-tasks LOGBOOK entry (a note or a state
;; transition) to a task heading, editing the file's disk contents in a temp
;; buffer so a visiting buffer is never disturbed.  Emacs owns the timestamp and
;; the drawer format; `dnc' only resolves the `code=path' id to a file.

;;; Code:

(require 'denote)
(require 'yank-media)
(require 'org)

(declare-function le::org-codepath-escape "le-org-codepath")

(defun le::denote-write-media (dir mimetype data &optional title)
  "Write media DATA into DIR as a denote file; return (PATH . ID).
MIMETYPE is a symbol like `image/png'; its subtype picks the extension
\(with the svg+xml -> svg exception).  TITLE is the denote title component,
sluggified by denote (default \"screenshot\"), so the file is named
<id>--<title>.<ext>.  ID is minted by `denote-get-identifier-function', whose
used-id scan reads `denote-directory' -- `let'-bind it around this call to
scope the scan.  DATA is written verbatim (`no-conversion')."
  (let* ((dir (file-name-as-directory dir))   ; denote-format-file-name insists on a trailing /
         (id (funcall denote-get-identifier-function nil (current-time)))
         (subtype (cadr (split-string (symbol-name mimetype) "/")))
         (ext (concat "." (if (equal subtype "svg+xml") "svg" subtype)))
         (path (denote-format-file-name dir id nil (or title "screenshot") ext nil)))
    (let ((coding-system-for-write 'no-conversion))
      (write-region data nil path nil 'silent))
    (cons path id)))

;;;###autoload
(defun le::denote-plan-path (dir title &optional keywords signature)
  "Return the denote path for a plan named TITLE in DIR; write NOTHING.
This is the pure name-oracle half of the `dnc plan' write verb: `dnc'
locates the project's denote silo, passes DIR (the absolute `plan/'
subdirectory) here, gets back a sluggified
`<id>==<signature>--<title>__<kw>.md' path, and authors the file body
itself -- the body never crosses into Emacs.
DIR is created if absent.  KEYWORDS is a list of keyword strings (nil for
none).  SIGNATURE is the plan's task `code=path' (nil for none), filling
denote's signature slot so the plan file links back to its task -- the
same slot `task/' files carry.  The minted identifier's used-id scan is
scoped to the silo (DIR's parent) by `let'-binding `denote-directory', so
ids stay unique there.  Passing DIR explicitly keeps naming independent of
this session's ambient project (the live buffer may belong to a different
one)."
  (let* ((dir (file-name-as-directory (expand-file-name dir)))
         (denote-directory (file-name-directory (directory-file-name dir)))
         (id (funcall denote-get-identifier-function nil (current-time))))
    (make-directory dir t)
    (denote-format-file-name dir id keywords title ".md" signature)))

(defun le::denote--logbook-indent (text)
  "Indent every line of TEXT by two spaces for a LOGBOOK note continuation."
  (mapconcat (lambda (l) (concat "  " l)) (split-string text "\n") "\n"))

;;;###autoload
(defun le::denote-logbook-append (file code kind &optional text to-state)
  "Append an org-tasks LOGBOOK entry to task CODE in FILE; return the entry text.
This is the Emacs half of the `dnc logbook' write verb.  FILE is the absolute
task file (`dnc' resolves CODE -> file by scanning the corpus); CODE is the
heading's `code=path' id, located here by its token so the edit is
content-addressed, not line-addressed.  KIND is \"note\" (a plain note) or
\"state\" (a state transition); TEXT is the accompanying prose (nil/empty for a
bare transition); TO-STATE is the new keyword for KIND \"state\".

When TEXT is present it follows the head after a `\\\\' line-break marker,
indented beneath -- the same shape for a note body and a state-change reason.

The edit runs in a temp buffer over the file's DISK contents and is written
back with `write-region' -- the file's visiting buffer, if any, is never
touched, so a user's unsaved edits are left for them to reconcile (an
unmodified buffer auto-reverts; a modified one trips the save-time modtime
guard).  The heading keyword is flipped by explicit text replacement, not
`org-todo', so org's own state-logging hooks never fire and double-write.  The
entry is inserted newest-first via `org-log-beginning', which creates the
LOGBOOK drawer if absent.  Every entry is time-stamped here and tagged 🤖 to
mark it machine-authored."
  (with-temp-buffer
    (insert-file-contents file)
    (delay-mode-hooks (org-mode))
    (goto-char (point-min))
    (unless (re-search-forward
             (concat "^\\*+[ \t]+[A-Z]+[ \t]+" (regexp-quote code) "\\(?:[ \t]\\|$\\)")
             nil t)
      (error "No task heading for code %s in %s" code file))
    (beginning-of-line)
    (let* ((org-log-into-drawer "LOGBOOK")
           (ts (format-time-string "[%Y-%m-%d %a %H:%M]"))
           (old-state
            (when (equal kind "state")
              (unless (looking-at "^\\*+[ \t]+\\([A-Z]+\\)[ \t]")
                (error "Heading for %s has no state keyword" code))
              (prog1 (match-string 1)
                (replace-match to-state t t nil 1))))
           (head (if (equal kind "state")
                     (format "State %-12s from %-12s %s 🤖"
                             (format "\"%s\"" to-state)
                             (format "\"%s\"" old-state) ts)
                   (format "Note taken on %s 🤖" ts)))
           (entry (if (and text (not (string-empty-p text)))
                      ;; A body (note text or a state-change reason) follows the
                      ;; \\ line-break marker, indented beneath -- the same shape
                      ;; org itself uses, for both kinds.
                      (format "- %s \\\\\n%s\n" head
                              (le::denote--logbook-indent (le::org-codepath-escape text)))
                    (format "- %s\n" head))))
      (org-back-to-heading t)
      (goto-char (org-log-beginning t))
      (insert entry)
      (let ((coding-system-for-write 'utf-8))
        (write-region (point-min) (point-max) file nil 'silent))
      entry)))

;;;###autoload
(defun le::denote-add-heading (file anchor newcode level state title &optional body)
  "Create task NEWCODE beneath ANCHOR in FILE; return the new heading line.
This is the Emacs half of the `dnc add' write verb.  FILE is the absolute
journal file; `dnc' has already done the arithmetic -- ANCHOR is the code of the
heading at whose subtree-tail the new heading is inserted, NEWCODE is the
computed `code=path' id, LEVEL the star count, STATE the birth keyword
(REFINING or TODO), TITLE the description, and optional BODY initial prose.

ANCHOR is located by its code token so the edit is content-addressed, not
line-addressed; the new heading is placed at the end of ANCHOR's subtree, which
serves both a new sibling (top-level member, ANCHOR = the group's last member)
and a new child (ANCHOR = the parent).  A birth LOGBOOK note is seeded newest-in
via `org-log-beginning' (empty \"from\", 🤖-tagged, time-stamped here), and BODY,
when present, follows the drawer.

Like `le::denote-logbook-append', the edit runs in a temp buffer over the file's
DISK contents and is written back with `write-region' -- the visiting buffer, if
any, is never touched."
  (with-temp-buffer
    (insert-file-contents file)
    (delay-mode-hooks (org-mode))
    (goto-char (point-min))
    (unless (re-search-forward
             (concat "^\\*+[ \t]+[A-Z]+[ \t]+" (regexp-quote anchor) "\\(?:[ \t]\\|$\\)")
             nil t)
      (error "No task heading for anchor %s in %s" anchor file))
    (org-back-to-heading t)
    (org-end-of-subtree t t)
    (let* ((org-log-into-drawer "LOGBOOK")
           (ts (format-time-string "[%Y-%m-%d %a %H:%M]"))
           (heading (concat (make-string level ?*) " " state " " newcode " "
                            (le::org-codepath-escape title))))
      (unless (bolp) (insert "\n"))
      (let ((head-start (point)))
        (insert heading "\n")
        (when (and body (not (string-empty-p body)))
          (insert (le::org-codepath-escape body) "\n"))
        (goto-char head-start)
        (org-back-to-heading t)
        (goto-char (org-log-beginning t))
        (insert (format "- State %-12s from %-12s %s 🤖\n"
                        (format "\"%s\"" state) "" ts)))
      (let ((coding-system-for-write 'utf-8))
        (write-region (point-min) (point-max) file nil 'silent))
      heading)))

;;;###autoload
(defun le::denote-yank-media (&optional prompt-title)
  "Paste a clipboard image beside the current file as a denote attachment.
Save the image as <id>--screenshot.<ext> in the current file's own directory
\(NOT `denote-directory') and insert a [[denote:<id>]] link at point.  With
PROMPT-TITLE (\\[universal-argument]), read a denote title to use in place of
\"screenshot\".

Signals a `user-error' if the buffer is not visiting a file, or if the
clipboard holds no image Emacs can handle.  The denote link resolves only
where the file's directory is covered by the active `denote-directory' (i.e.
inside a denote silo)."
  (interactive "P")
  (unless buffer-file-name
    (user-error "Buffer is not visiting a file"))
  (let ((type (car (funcall yank-media-autoselect-function
                            (yank-media--find-matching-media "image/.*")))))
    (unless type
      (user-error "No image on the clipboard"))
    (let* ((data (yank-media--get-selection type))
           (dir (file-name-directory buffer-file-name))
           (title (if prompt-title (read-string "Denote title: ") "screenshot"))
           ;; No `denote-directory' override: the silo's dir-local value scopes
           ;; both the id-uniqueness scan and the [[denote:id]] link's lookup.
           (id (cdr (le::denote-write-media dir type data title))))
      (insert (format "[[denote:%s]]" id)))))

;;;; Export an org subtree into denote task files

;; `le::denote-export-subtree' relocates already-coded task nodes out of a
;; journal subtree into their own files under <silo>/task/, one file per
;; code=path (node-per-file fan-out).  Each new file keeps the org heading
;; verbatim (promoted to level 1) plus its LOGBOOK and body; #+title is the
;; heading's description and #+signature is its code=path.  It is the inverse
;; of `denote-org-extract-org-subtree', which demotes the heading into #+title
;; and drops the TODO state.
;;
;; A region selects several subtrees for one run: it is expanded outward to
;; whole subtrees at the shallowest heading level it touches
;; (`le::denote-export--region-roots'), so a partial selection never yields a
;; partial file.  A root heading without a TODO state aborts the run -- it has
;; no destination file, and the source spans are deleted wholesale, so
;; descending into it would drop its text on the floor.
;;
;; Three refusals run before anything is written, because the export is
;; all-or-nothing and identity is never invented: a task node without a
;; code=path, one code=path claimed twice within the selection, and a code=path
;; that already names a file in task/.  The last one matters because denote will
;; not catch it -- it mints a fresh identifier per file, so a re-export lands a
;; second file with the same signature, which the `dnc' corpus then reads as an
;; ambiguous code.
;;
;; Functional core (`le::denote-export--scan' over one span,
;; `le::denote-export--plan' over many) returns a plan with no side effects; the
;; interactive shell previews it, confirms, then creates files (flushed to disk)
;; BEFORE deleting the source subtrees.

(defun le::denote-export--parse-code (headline)
  "Split HEADLINE \"code=path description\" into (CODE . DESC).
CODE is a code=path id: a dash-free [a-z0-9]+ stem followed by at least one
=<integer> segment, so an uncoded headline like \"refactor parser\" returns
nil.  DESC is the remaining text (empty string when absent).  Return nil when
HEADLINE carries no code=path."
  (when (string-match
         (rx bos
             (group (+ (any "a-z0-9")) (+ "=" (+ (any "0-9"))))
             (opt (+ (any " \t")) (group (* nonl)))
             eos)
         headline)
    (cons (match-string 1 headline)
          (or (match-string 2 headline) ""))))

(defun le::denote-export--promote (text)
  "Promote org subtree TEXT so its top heading sits at level 1.
Strip (top-level - 1) leading stars from every heading line, shifting the
whole subtree up uniformly.  Descendant headings stay strictly below their
new level-1 root."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (let ((drop (if (looking-at "\\*+") (1- (length (match-string 0))) 0)))
      (when (> drop 0)
        (while (re-search-forward "^\\(\\*+\\) " nil t)
          (replace-match
           (concat (make-string (max 1 (- (length (match-string 1)) drop)) ?*) " ")
           t t))))
    ;; Collapse the blank runs left by cut task children, and end in one newline.
    (concat (string-trim-right
             (replace-regexp-in-string "\n\\{3,\\}" "\n\n" (buffer-string)))
            "\n")))

(defun le::denote-export--deepest-task ()
  "Return the position of a maximum-level TODO heading in the buffer, or nil."
  (let (best best-level)
    (org-map-entries
     (lambda ()
       (when (org-get-todo-state)
         (let ((lvl (org-current-level)))
           (when (or (null best-level) (> lvl best-level))
             (setq best-level lvl best (point)))))))
    best))

(defun le::denote-export--extract-contents (subtree-text)
  "Return an alist (CODE . CONTENT) for each task node in SUBTREE-TEXT.
CONTENT is the node's subtree promoted to level 1 -- its own heading, drawers
and body plus any non-task descendants, but not its task descendants, which
are cut first (deepest-first) into their own entries."
  (let ((result '()))
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      (insert subtree-text)
      (let (target)
        (while (setq target (le::denote-export--deepest-task))
          (goto-char target)
          (org-back-to-heading t)
          (let* ((code (car (le::denote-export--parse-code (org-get-heading t t t t))))
                 (beg (point))
                 (end (save-excursion (org-end-of-subtree t) (point)))
                 (text (buffer-substring-no-properties beg end)))
            (push (cons code (le::denote-export--promote text)) result)
            (delete-region beg end)))))
    result))

(defun le::denote-export--region-roots (beg end)
  "Return whole-subtree spans ((BEG . END) ...) covering the region BEG..END.
The region is expanded outward to the shallowest heading level it touches: the
heading at or before BEG anchors the walk, the minimum level among that heading
and every heading starting before END sets the root level, and the walk begins
at the anchor's ancestor at that level.  A selection that clips a subtree
therefore still exports it whole, and one that reaches across a shallower
heading pulls in that heading instead of splitting under it.  Spans come back
in buffer order and never overlap."
  (save-excursion
    (goto-char beg)
    (org-back-to-heading t)
    (let ((level (org-current-level)))
      (save-excursion
        (while (and (outline-next-heading) (< (point) end))
          (setq level (min level (org-current-level)))))
      (while (and (> (org-current-level) level) (org-up-heading-safe)))
      (let (roots)
        (while (and (org-at-heading-p) (< (point) end))
          (let ((s (point))
                (e (save-excursion (org-end-of-subtree t) (point))))
            (push (cons s e) roots)
            (goto-char e)
            ;; A trailing subtree ends inside its own last line rather than on
            ;; the next heading's bol, and `org-at-heading-p' is line-based --
            ;; so it would answer t there and the walk would never advance.
            ;; Only stay put when parked at the very start of a heading.
            (unless (and (bolp) (org-at-heading-p))
              (outline-next-heading))))
        (nreverse roots)))))

(defun le::denote-export--non-task-roots (roots)
  "Return the headlines of the ROOTS spans whose heading carries no TODO state."
  (let (result)
    (save-excursion
      (dolist (root roots)
        (goto-char (car root))
        (unless (org-get-todo-state)
          (push (org-get-heading t t t t) result))))
    (nreverse result)))

(defun le::denote-export--scan (beg end)
  "Scan the subtree spanning BEG..END and return an export plan plist.
BEG must be the start of the subtree's root heading.  The plan has:
  :region  (BEG . END) of the source subtree in the current buffer;
  :uncoded a list of headline strings for task nodes lacking a code=path;
  :nodes   node plists (:code :state :title :keywords :content), present only
           when :uncoded is empty.
No side effects: the content pass runs on a copy of the subtree text."
  (save-excursion
    (goto-char beg)
    (let ((subtree-text (buffer-substring-no-properties beg end))
          (meta '())
          (uncoded '()))
      (org-map-entries
       (lambda ()
         (when (org-get-todo-state)
           (let* ((headline (org-get-heading t t t t))
                  (parsed (le::denote-export--parse-code headline)))
             (if parsed
                 (push (list :code (car parsed)
                             :state (org-get-todo-state)
                             :title (cdr parsed)
                             :keywords (mapcar #'substring-no-properties (org-get-tags)))
                       meta)
               (push headline uncoded)))))
       nil 'tree)
      (setq meta (nreverse meta)
            uncoded (nreverse uncoded))
      (if uncoded
          (list :region (cons beg end) :uncoded uncoded)
        (let ((contents (le::denote-export--extract-contents subtree-text)))
          (list :region (cons beg end)
                :nodes (mapcar
                        (lambda (m)
                          (append m (list :content
                                          (cdr (assoc (plist-get m :code) contents)))))
                        meta)))))))

(defun le::denote-export--duplicate-codes (nodes)
  "Return the code=paths claimed by more than one node in NODES, in order.
Two nodes sharing a code would take the same content -- the content lookup in
`le::denote-export--scan' is an `assoc', which sees only the first -- and would
land two task files under one identity."
  (let ((seen '()) (dups '()))
    (dolist (node nodes)
      (let ((code (plist-get node :code)))
        (cond ((member code dups))
              ((member code seen) (push code dups))
              (t (push code seen)))))
    (nreverse dups)))

(defun le::denote-export--plan (roots)
  "Fold the per-root scans of the ROOTS spans into one export plan plist.
ROOTS is a list of (BEG . END) subtree spans in buffer order.  The plan has:
  :regions    the ROOTS spans, buffer order;
  :uncoded    every uncoded task headline across ROOTS;
  :nodes      every node across ROOTS in buffer order, present only when
              :uncoded is empty;
  :duplicates code=paths claimed by more than one node."
  (let ((uncoded '()) (nodes '()))
    (dolist (root roots)
      (let ((plan (le::denote-export--scan (car root) (cdr root))))
        (setq uncoded (append uncoded (plist-get plan :uncoded))
              nodes (append nodes (plist-get plan :nodes)))))
    (if uncoded
        (list :regions roots :uncoded uncoded)
      (list :regions roots
            :nodes nodes
            :duplicates (le::denote-export--duplicate-codes nodes)))))

(defun le::denote-export--claimed-codes (task-dir)
  "Return an alist (CODE . FILENAME) of signatures already used in TASK-DIR.
Non-recursive, and scoped to TASK-DIR rather than the silo: `plan/' files carry
their task's code=path in the same signature slot, so a wider scan would read
a plan as a claim on its own task's identity."
  (when (file-directory-p task-dir)
    (delq nil
          (mapcar (lambda (name)
                    (when-let* ((sig (denote-retrieve-filename-signature name)))
                      (cons sig name)))
                  (directory-files task-dir nil directory-files-no-dot-files-regexp)))))

(defun le::denote-export--collisions (nodes claimed)
  "Return (CODE . FILENAME) for each node in NODES whose code is in CLAIMED.
CLAIMED is an alist as returned by `le::denote-export--claimed-codes'."
  (delq nil
        (mapcar (lambda (node) (assoc (plist-get node :code) claimed))
                nodes)))

(defun le::denote-export--preview (nodes task-dir root-count)
  "Show, in *denote-export*, the files NODES will create in TASK-DIR.
ROOT-COUNT is how many source subtrees the nodes came from.  The buffer is in
`special-mode', so `q' buries it."
  ;; One identifier for every row: each mint scans the whole silo, and the rows
  ;; are illustrative anyway -- nothing is written, so they would all match.
  (let ((id (funcall denote-get-identifier-function nil (current-time))))
    (with-current-buffer (get-buffer-create "*denote-export*")
      (special-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "denote export: %d task node(s) from %d subtree(s) -> %s\n\n"
                        (length nodes) root-count task-dir))
        (dolist (node nodes)
          (insert (format "  %-16s ->  %s\n"
                          (plist-get node :code)
                          (file-name-nondirectory
                           (denote-format-file-name
                            task-dir
                            id
                            (plist-get node :keywords)
                            (or (plist-get node :title) "")
                            ".org"
                            (plist-get node :code))))))
        (insert "\n(identifiers above are illustrative; real ones are minted at creation)\n")
        (insert "The source subtree(s) will then be deleted from the journal buffer.\n"))
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

;;;###autoload
(defun le::denote-export-subtree (&optional beg end)
  "Export org task subtrees into denote task files, one file per code=path.
Without a region, point must be on a task-state heading and its subtree is the
lone source.  With a region -- BEG..END, taken from the active region when
called interactively -- the selection is expanded outward to whole subtrees at
the shallowest heading level it touches and every one of them is exported in a
single run.  A root heading without a TODO state aborts: the source spans are
deleted wholesale, and such a heading has no file to carry its text into.

Every heading carrying a TODO state is a node; each becomes a file in
<`denote-directory'>/task/ named by its code=path, keeping the heading verbatim
\(promoted to level 1), its LOGBOOK and body, and any non-task descendants.

Refuse the whole run, writing nothing, when a task node lacks a code=path, when
one code=path is claimed twice within the selection, or when a code=path already
names a file in the task directory.  Preview first; when more than one file will
be created, confirm before writing.  Files are created and flushed to disk
BEFORE the source subtrees are deleted from the buffer."
  (interactive (when (use-region-p) (list (region-beginning) (region-end))))
  (unless (derived-mode-p 'org-mode)
    (user-error "Not an Org buffer"))
  (let* ((journal-buf (current-buffer))
         (roots (if (and beg end)
                    (le::denote-export--region-roots beg end)
                  (progn
                    (unless (and (org-at-heading-p) (org-get-todo-state))
                      (user-error "Point must be on a task-state heading"))
                    (list (cons (org-entry-beginning-position)
                                (save-excursion (org-end-of-subtree t) (point))))))))
    (unless roots
      (user-error "No subtree in the region"))
    (when-let* ((non-tasks (le::denote-export--non-task-roots roots)))
      (user-error "Refusing to export: %d non-task root heading(s): %s"
                  (length non-tasks) (string-join non-tasks "; ")))
    (let* ((plan (le::denote-export--plan roots))
           (uncoded (plist-get plan :uncoded)))
      (when uncoded
        (user-error "Refusing to export: %d uncoded task node(s): %s"
                    (length uncoded) (string-join uncoded "; ")))
      (let* ((nodes (plist-get plan :nodes))
             (duplicates (plist-get plan :duplicates))
             (regions (plist-get plan :regions))
             (task-dir (file-name-as-directory
                        (expand-file-name "task" (car (denote-directories)))))
             (collisions (le::denote-export--collisions
                          nodes (le::denote-export--claimed-codes task-dir))))
        (unless nodes (user-error "No task nodes to export"))
        (when duplicates
          (user-error "Refusing to export: code=path claimed twice: %s"
                      (string-join duplicates "; ")))
        (when collisions
          (user-error "Refusing to export: code=path already filed in %s: %s"
                      task-dir
                      (mapconcat (lambda (c) (format "%s -> %s" (car c) (cdr c)))
                                 collisions "; ")))
        (le::denote-export--preview nodes task-dir (length roots))
        (let ((proceed (or (= (length nodes) 1)
                           (yes-or-no-p (format "Export %d task nodes into %s? "
                                                (length nodes) task-dir)))))
          ;; Dismiss the preview once the user has answered (bury, don't kill).
          (quit-windows-on "*denote-export*")
          (when proceed
            (make-directory task-dir t)
            ;; `denote' opens each new file with `find-file'; keep that churn from
            ;; disturbing the user's window layout.
            (save-window-excursion
              (let ((denote-kill-buffers t))
                (dolist (node nodes)
                  ;; `denote' drifts `current-buffer' (find-file then kill), so
                  ;; re-assert the journal buffer each call: it carries the silo's
                  ;; buffer-local `denote-directory', which is what validates
                  ;; TASK-DIR.  Without this, calls after the first fall back to the
                  ;; global `denote-directory' and land in the wrong silo.
                  (with-current-buffer journal-buf
                    (denote (or (plist-get node :title) "")
                            (plist-get node :keywords)
                            'org
                            task-dir
                            nil
                            (plist-get node :content)
                            (plist-get node :code))))))
            (with-current-buffer journal-buf
              ;; Back-to-front: deleting a later span leaves the earlier ones'
              ;; positions untouched.
              (dolist (region (reverse regions))
                (delete-region (car region) (cdr region))
                ;; A span stops inside its own last line, so its closing newline
                ;; outlives it; take that and the blank lines that separated the
                ;; subtree from what follows, or each export widens a gap.
                (goto-char (car region))
                (when (looking-at "\\(?:[ \t]*\n\\)+")
                  (delete-region (point) (match-end 0))))
              (save-buffer))
            (deactivate-mark)
            (message "Exported %d task node(s) from %d subtree(s) to %s"
                     (length nodes) (length roots) task-dir)))))))

(provide 'le-denote)
;;; le-denote.el ends here
