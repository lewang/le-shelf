;;; le-denote.el --- Denote helpers: media capture + settled-tree sweep  -*- lexical-binding: t; -*-

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
(require 'seq)

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
and a new child (ANCHOR = the parent).  A nil or empty ANCHOR instead appends the
new heading at end of file -- the fallback `dnc' takes for a top-level follow-on
whose code-group is fully promoted, leaving no journal member to anchor against
(the journal itself is resolved by `le::denote-tasks-resolve-journal').  A birth
LOGBOOK note is seeded newest-in
via `org-log-beginning' (empty \"from\", 🤖-tagged, time-stamped here), and BODY,
when present, follows the drawer.

Like `le::denote-logbook-append', the edit runs in a temp buffer over the file's
DISK contents and is written back with `write-region' -- the visiting buffer, if
any, is never touched."
  (with-temp-buffer
    (insert-file-contents file)
    (delay-mode-hooks (org-mode))
    (if (and anchor (not (string-empty-p anchor)))
        (progn
          (goto-char (point-min))
          (unless (re-search-forward
                   (concat "^\\*+[ \t]+[A-Z]+[ \t]+" (regexp-quote anchor) "\\(?:[ \t]\\|$\\)")
                   nil t)
            (error "No task heading for anchor %s in %s" anchor file))
          (org-back-to-heading t)
          (org-end-of-subtree t t))
      (goto-char (point-max)))
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

;;;; Sweep settled org trees into denote task files

;; `le::denote-export-settled' sweeps a journal buffer and relocates its settled
;; trees into their own files under <silo>/task/, one file per code=path
;; (node-per-file fan-out).  Each new file keeps the org heading verbatim
;; (promoted to level 1) plus its LOGBOOK and body; #+title is the heading's
;; description and #+signature is its code=path.  It is the inverse of
;; `denote-org-extract-org-subtree', which demotes the heading into #+title and
;; drops the TODO state.
;;
;; The level-1 tree is the unit of filing: `le::denote-export--region-roots'
;; takes every level-1 tree the swept span touches, and each is filed whole or
;; skipped whole.  A finished child therefore waits for its level-1 ancestor to
;; close -- one invariant with no exceptions, which matters because the source
;; spans are deleted wholesale and a partially filed tree would drop text.
;;
;; Nothing aborts a sweep.  Five conditions skip a tree and let the rest through:
;; a root heading with no TODO state, a task heading whose state is not in
;; `org-done-keywords', a task heading without a code=path, a code=path claimed
;; by two trees at once, and a code=path that already names a file in task/.  The
;; last matters because denote will not catch it -- it mints a fresh identifier
;; per file, so a re-export lands a second file with the same signature, which
;; the `dnc' corpus then reads as an ambiguous code.  Duplicate and collision are
;; judged over the candidate trees only, so an open tree's codes never block a
;; settled tree that shares one.
;;
;; Functional core (`le::denote-export--scan' judges one tree,
;; `le::denote-export--plan' folds many into a per-tree plan,
;; `le::denote-export--report' renders it) has no side effects; the interactive
;; shell displays the report, confirms, then creates files (flushed to disk)
;; BEFORE deleting the source trees.

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
  "Return the spans of the level-1 trees the region BEG..END touches.
Each span is a (BEG . END) pair.  A tree is touched when its span intersects
the half-open region, so a selection clipping a tree still files it whole and a
selection inside a descendant still yields that descendant's level-1 ancestor.
Level 1 is the only root level: the tree is the unit of filing, so a finished
child is never taken out of a live parent.  Spans come back in buffer order and
never overlap."
  (save-excursion
    (let (roots)
      ;; Scope nil, not `file': the latter maps nothing in a buffer with no file,
      ;; which is where the tests live.  Both honour a narrowing.
      (org-map-entries
       (lambda ()
         (when (= (org-current-level) 1)
           (let ((s (point))
                 (e (save-excursion (org-end-of-subtree t) (point))))
             (when (and (< s end) (> e beg))
               (push (cons s e) roots))))))
      (nreverse roots))))

(defun le::denote-export--scan (beg end)
  "Judge the tree spanning BEG..END and return a plist describing it.
BEG must be the start of the tree's root heading.  The plist has:
  :region   (BEG . END) of the tree in the current buffer;
  :headline the root heading's text, for the report;
  :state    the root heading's TODO state, nil when it is not a task;
  :nodes    node plists (:code :state :title :keywords) for the task headings
            carrying a code=path, in buffer order;
  :open     \"STATE headline\" for each task heading whose state is not in
            `org-done-keywords';
  :uncoded  the headlines of the task headings lacking a code=path.
Judgement only: no content is extracted, so nothing pays for a tree that turns
out to be skipped."
  (save-excursion
    (goto-char beg)
    (let ((headline (org-get-heading t t t t))
          (state (org-get-todo-state))
          (nodes '())
          (open '())
          (uncoded '()))
      (org-map-entries
       (lambda ()
         (when-let* ((node-state (org-get-todo-state)))
           (let* ((node-headline (org-get-heading t t t t))
                  (parsed (le::denote-export--parse-code node-headline)))
             (unless (member node-state org-done-keywords)
               (push (format "%s %s" node-state node-headline) open))
             (if parsed
                 (push (list :code (car parsed)
                             :state node-state
                             :title (cdr parsed)
                             :keywords (mapcar #'substring-no-properties (org-get-tags)))
                       nodes)
               (push node-headline uncoded)))))
       nil 'tree)
      (list :region (cons beg end)
            :headline headline
            :state state
            :nodes (nreverse nodes)
            :open (nreverse open)
            :uncoded (nreverse uncoded)))))

(defun le::denote-export--duplicate-codes (nodes)
  "Return the code=paths claimed by more than one node in NODES, in order.
Two nodes sharing a code would take the same content -- the content lookup in
`le::denote-export--with-content' is an `assoc', which sees only the first --
and would land two task files under one identity."
  (let ((seen '()) (dups '()))
    (dolist (node nodes)
      (let ((code (plist-get node :code)))
        (cond ((member code dups))
              ((member code seen) (push code dups))
              (t (push code seen)))))
    (nreverse dups)))

(defun le::denote-export--candidate-p (scan)
  "Return non-nil when the tree SCAN describes is settled, coded and a task."
  (and (plist-get scan :state)
       (null (plist-get scan :open))
       (null (plist-get scan :uncoded))))

(defun le::denote-export--judge (scan duplicates collisions)
  "Return the tree SCAN describes, carrying its skip reason.
DUPLICATES and COLLISIONS are as computed across the candidate trees.  The tree
is (:region :headline :nodes :skip REASON-or-nil :detail LIST); REASON is the
first of `:not-task', `:open', `:uncoded', `:duplicate' and `:collision' that
applies, so the reasons partition the trees and each count in the report stands
for a distinct set.  DETAIL holds the strings that tripped the reason."
  (let* ((nodes (plist-get scan :nodes))
         (codes (mapcar (lambda (node) (plist-get node :code)) nodes))
         (dups (seq-intersection codes duplicates))
         (hits (seq-filter (lambda (hit) (member (car hit) codes)) collisions))
         (base (list :region (plist-get scan :region)
                     :headline (plist-get scan :headline)
                     :state (plist-get scan :state)
                     :nodes nodes)))
    (append base
            (cond
             ((null (plist-get scan :state)) (list :skip :not-task :detail nil))
             ((plist-get scan :open)
              (list :skip :open :detail (plist-get scan :open)))
             ((plist-get scan :uncoded)
              (list :skip :uncoded :detail (plist-get scan :uncoded)))
             (dups (list :skip :duplicate :detail dups))
             (hits (list :skip :collision
                         :detail (mapcar (lambda (hit)
                                           (format "%s -> %s" (car hit) (cdr hit)))
                                         hits)))
             (t (list :skip nil :detail nil))))))

(defun le::denote-export--with-content (tree)
  "Return TREE with each of its nodes carrying the :content it will be filed as."
  (let* ((region (plist-get tree :region))
         (contents (le::denote-export--extract-contents
                    (buffer-substring-no-properties (car region) (cdr region)))))
    (plist-put (copy-sequence tree)
               :nodes
               (mapcar (lambda (node)
                         (append node
                                 (list :content
                                       (cdr (assoc (plist-get node :code) contents)))))
                       (plist-get tree :nodes)))))

(defun le::denote-export--plan (roots claimed)
  "Fold the ROOTS spans into a per-tree export plan, as (:trees TREES).
ROOTS is a list of level-1 (BEG . END) spans in buffer order; CLAIMED is an
alist as returned by `le::denote-export--claimed-codes', passed in so the
collision judgement stays out of the silo.  Each tree is as
`le::denote-export--judge' returns it; the survivors additionally carry their
nodes' :content.

Duplicates and collisions are judged over the candidate trees only -- those
clearing the first three reasons -- so an open tree's codes never block a
settled tree that shares one."
  (let* ((scans (mapcar (lambda (root)
                          (le::denote-export--scan (car root) (cdr root)))
                        roots))
         (candidate-nodes
          (apply #'append
                 (mapcar (lambda (scan) (plist-get scan :nodes))
                         (seq-filter #'le::denote-export--candidate-p scans))))
         (duplicates (le::denote-export--duplicate-codes candidate-nodes))
         (collisions (le::denote-export--collisions candidate-nodes claimed)))
    (list :trees
          (mapcar (lambda (scan)
                    (let ((tree (le::denote-export--judge scan duplicates collisions)))
                      (if (plist-get tree :skip)
                          tree
                        (le::denote-export--with-content tree))))
                  scans))))

(defun le::denote-export--filed (trees)
  "Return the TREES that carry no skip reason, in order."
  (seq-remove (lambda (tree) (plist-get tree :skip)) trees))

(defun le::denote-export--nodes (trees)
  "Return every node of TREES, in tree order."
  (apply #'append (mapcar (lambda (tree) (plist-get tree :nodes)) trees)))

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

(defconst le::denote-export--skip-labels
  '((:open      . "not settled")
    (:uncoded   . "no code=path")
    (:duplicate . "code=path claimed twice")
    (:collision . "code=path already filed")
    (:not-task  . "not a task"))
  "The skip reasons in report order, each with the line it heads.")

(defun le::denote-export--root-line (tree)
  "Return TREE's root heading as it reads in the report, state included."
  (if-let* ((state (plist-get tree :state)))
      (format "%s %s" state (plist-get tree :headline))
    (plist-get tree :headline)))

(defun le::denote-export--report (trees task-dir id)
  "Return the report for TREES as a string.
TASK-DIR is where the survivors will be filed and ID the illustrative identifier
their preview rows share.  The summary counts the trees seen, those to file, and
each skip reason that occurred; the sections below it name every skipped tree
and the detail that tripped it.  `:not-task' trees are counted but never listed
-- a journal holds prose and reference headings, and a section that fires on
every sweep is noise."
  (let* ((filed (le::denote-export--filed trees))
         (nodes (le::denote-export--nodes filed)))
    (with-temp-buffer
      (insert (format "denote export -> %s\n\n" task-dir))
      (insert (format "  %d tree(s) seen; %d to file, %d task node(s)\n"
                      (length trees) (length filed) (length nodes)))
      (dolist (entry le::denote-export--skip-labels)
        (let ((count (seq-count (lambda (tree) (eq (plist-get tree :skip) (car entry)))
                                trees)))
          (when (> count 0)
            (insert (format "  %d skipped: %s\n" count (cdr entry))))))
      (when nodes
        (insert "\nto file:\n")
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
        (insert "The trees filed above will then be deleted from the journal buffer.\n"))
      (dolist (entry le::denote-export--skip-labels)
        (unless (eq (car entry) :not-task)
          (when-let* ((skipped (seq-filter
                                (lambda (tree) (eq (plist-get tree :skip) (car entry)))
                                trees)))
            (insert (format "\nskipped -- %s:\n" (cdr entry)))
            (dolist (tree skipped)
              (let ((root (le::denote-export--root-line tree)))
                (insert (format "  %s\n" root))
                (dolist (detail (plist-get tree :detail))
                  ;; A tree tripped by its own root already reads on the line
                  ;; above; only descendants earn a line of their own.
                  (unless (member detail (list root (plist-get tree :headline)))
                    (insert (format "      %s\n" detail)))))))))
      (buffer-string))))

(defun le::denote-export--preview (trees task-dir)
  "Show the report for TREES in *denote-export*, and return that buffer.
TASK-DIR is where the survivors will be filed.  The buffer is in `special-mode',
so `q' buries it."
  ;; One identifier for every row: each mint scans the whole silo, and the rows
  ;; are illustrative anyway -- nothing is written, so they would all match.
  (let ((report (le::denote-export--report
                 trees task-dir
                 (funcall denote-get-identifier-function nil (current-time)))))
    (with-current-buffer (get-buffer-create "*denote-export*")
      (special-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert report))
      (goto-char (point-min))
      (display-buffer (current-buffer))
      (current-buffer))))

;;;###autoload
(defun le::denote-export-settled (&optional beg end)
  "Sweep settled level-1 trees out of an org journal into denote task files.
BEG..END is the active region when called interactively, else the whole
\(possibly narrowed) buffer; every level-1 tree the span touches is swept.  The
tree is the unit: it is filed whole or skipped whole, so a finished child waits
for its level-1 ancestor to close.

A tree is filed only when its root is a task, every task heading in it is closed
\(its state is in `org-done-keywords') and carries a code=path, and no code=path
is claimed twice in the sweep or already names a file in
<`denote-directory'>/task/.  Each task heading becomes its own file there, named
by its code=path and keeping the heading verbatim (promoted to level 1), its
LOGBOOK and body, and any non-task descendants.

Nothing aborts the sweep: a tree failing any of those tests is skipped and the
rest are filed.  The report in *denote-export* summarises the run and names
every skipped tree with what tripped it; confirmation is unconditional.  Signal
a `user-error' only when no tree survives.  Files are created and flushed to
disk BEFORE the source trees are deleted from the buffer."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list (point-min) (point-max))))
  (unless (derived-mode-p 'org-mode)
    (user-error "Not an Org buffer"))
  (let* ((journal-buf (current-buffer))
         (beg (or beg (point-min)))
         (end (or end (point-max)))
         (task-dir (file-name-as-directory
                    (expand-file-name "task" (car (denote-directories)))))
         (roots (le::denote-export--region-roots beg end)))
    (unless roots
      (user-error "No level-1 tree in the region"))
    (let* ((trees (plist-get (le::denote-export--plan
                              roots (le::denote-export--claimed-codes task-dir))
                             :trees))
           (filed (le::denote-export--filed trees))
           (nodes (le::denote-export--nodes filed)))
      ;; Report before refusing: an all-skipped sweep is the normal outcome, and
      ;; the reasons are the whole point of running it.
      (le::denote-export--preview trees task-dir)
      (unless filed
        (user-error "Nothing settled to file: all %d tree(s) skipped" (length trees)))
      (let ((proceed (yes-or-no-p
                      (format "File %d task node(s) from %d of %d tree(s) into %s? "
                              (length nodes) (length filed) (length trees) task-dir))))
        ;; Dismiss the report once the user has answered (bury, don't kill).
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
            (dolist (tree (reverse filed))
              (let ((region (plist-get tree :region)))
                (delete-region (car region) (cdr region))
                ;; A span stops inside its own last line, so its closing newline
                ;; outlives it; take that and the blank lines that separated the
                ;; tree from what follows, or each sweep widens a gap.
                (goto-char (car region))
                (when (looking-at "\\(?:[ \t]*\n\\)+")
                  (delete-region (point) (match-end 0)))))
            (save-buffer))
          (deactivate-mark)
          (message "Filed %d task node(s) from %d of %d tree(s) to %s"
                   (length nodes) (length filed) (length trees) task-dir))))))

(provide 'le-denote)
;;; le-denote.el ends here
