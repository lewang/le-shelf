;;; le-denote-attach.el --- Retire org-attach data/ trees into denote documents  -*- lexical-binding: t; -*-

;;; Commentary:

;; An `[[attachment:FILE]]' link resolves through the enclosing heading's `:ID:'
;; AND through `org-attach-id-dir' (`data/') taken relative to THE ORG FILE'S OWN
;; DIRECTORY.  Both halves are static, so moving a heading between files silently
;; moves its attachment lookup with it -- and breaks it.  A playground that has
;; had headings exported or promoted therefore accumulates several `data/' trees
;; at different levels, only some of whose links still resolve.
;;
;; `le::denote-migrate-attachments' retires the whole arrangement in one
;; playground-wide run.  Denote identifiers are position-independent, so an asset
;; converted into a denote document survives every later move.
;;
;;   still referenced -> a denote document in the destination, and every
;;                       `[[attachment:F]]' aimed at it becomes `[[denote:ID]]'
;;   no longer referenced -> moved under <silo>/_tobedeleted/, mirroring its path
;;                       relative to the playground so provenance survives
;;   then                 -> the emptied `data/' trees are deleted
;;
;; The identifier is the CAPTURE TIMESTAMP already embedded in the file name and
;; the title is `screenshot', so `clipboard-20260703T134035.png' becomes
;; `20260703T134035--screenshot.png' -- the shape `le::denote-yank-media' writes,
;; with capture time preserved rather than replaced by migration time.
;;
;; "Referenced" is the BROADEST test: any mention of the asset's filename
;; anywhere in the playground, every file type, links and prose alike.  That is
;; deliberate.  A displaced heading's `attachment:' link cannot be resolved by
;; org-attach from its new home, so resolving links properly would report exactly
;; the references that most need finding as absent.  Matching the filename finds
;; them; the cost is that the corpus also catches mentions the rewrite cannot
;; repair (prose, hand-written `file:' paths), and those are reported for a manual
;; fix rather than hidden.
;;
;; The destination defaults to <silo>/taskdata/ -- deliberately not <silo>/data/,
;; which would itself be one of the trees this command discovers and delete, and
;; a name org-attach never looks for cannot be mistaken for an attachment store.
;;
;; Assets are COPIED and VERIFIED before any `data/' tree is deleted, and a tree
;; holding anything the run did not act on survives and is reported.
;;
;; The functional core (`--plan' and the string helpers under it) decides
;; everything from an asset inventory plus the playground's text handed in as
;; already-read (FILE . TEXT) pairs; it touches no filesystem.  The shell
;; discovers, reads, previews, copies and deletes.

;;; Code:

(require 'denote)
(require 'seq)
(require 'subr-x)

;;; Pure core --------------------------------------------------------------

(defconst le::denote-attach--identifier-rx
  (rx (group (= 8 digit) "T" (= 6 digit)))
  "Regexp for a denote identifier embedded in a file name.")

(defun le::denote-attach--name-identifier (basename)
  "The denote identifier embedded in BASENAME, or nil.
A clipboard capture is named `clipboard-<YYYYMMDDTHHMMSS>.png', and that
timestamp is already exactly a denote identifier -- so it becomes the converted
document's id and the capture time survives the migration."
  (when (string-match le::denote-attach--identifier-rx basename)
    (match-string 1 basename)))

(defun le::denote-attach--bump-identifier (id)
  "The denote identifier one second after ID."
  (format-time-string
   "%Y%m%dT%H%M%S"
   (time-add (encode-time (string-to-number (substring id 13 15))
                          (string-to-number (substring id 11 13))
                          (string-to-number (substring id 9 11))
                          (string-to-number (substring id 6 8))
                          (string-to-number (substring id 4 6))
                          (string-to-number (substring id 0 4)))
             1)))

(defun le::denote-attach--identifier (basename fallback taken)
  "A denote identifier for BASENAME not present in TAKEN.
Prefer the timestamp embedded in BASENAME; fall back to FALLBACK (the caller
derives it from the file's modification time) when the name carries none.  A
collision bumps one second at a time, the same way denote resolves its own."
  (let ((id (or (le::denote-attach--name-identifier basename) fallback)))
    (while (member id taken)
      (setq id (le::denote-attach--bump-identifier id)))
    id))

(defun le::denote-attach--link-regexp (basename)
  "Regexp matching an Org `attachment:' link whose target is exactly BASENAME.
Group 1 is the link's description, when it has one.  The target is bracketed by
`[[attachment:' and either `]]' or `][', so an asset whose name is a SUBSTRING
of another's can never be caught by the wrong rewrite."
  (rx "[[attachment:" (literal basename)
      (opt "][" (group (*? nonl)))
      "]]"))

(defun le::denote-attach--rewrite-text (text basename id)
  "Rewrite TEXT's `attachment:' links to BASENAME as denote links to ID.
Return (NEW-TEXT . COUNT).  Both forms are handled: `[[attachment:F]]' becomes
`[[denote:ID]]', and `[[attachment:F][DESC]]' becomes `[[denote:ID][DESC]]' --
the description is the reader's label for the asset, so it is kept."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (let ((re (le::denote-attach--link-regexp basename))
          (count 0))
      (while (re-search-forward re nil t)
        (let ((desc (match-string 1)))
          (replace-match (if desc
                             (format "[[denote:%s][%s]]" id desc)
                           (format "[[denote:%s]]" id))
                         t t))
        (setq count (1+ count)))
      (cons (buffer-string) count))))

(defun le::denote-attach--link-occurrences (text basename)
  "Every `attachment:' link to BASENAME in TEXT, as (LINE . LINK-TEXT) pairs.
Located rather than merely counted, so the report can show each edit at the
`file:line' it will happen -- this run deletes directories, and a summary of
counts is not enough to review it by."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (let ((re (le::denote-attach--link-regexp basename))
          occurrences)
      (while (re-search-forward re nil t)
        (push (cons (line-number-at-pos (match-beginning 0)) (match-string 0))
              occurrences))
      (nreverse occurrences))))

(defun le::denote-attach--mention-occurrences (text basename)
  "Every mention of BASENAME in TEXT outside an `attachment:' link to it.
Each is (LINE . LINE-TEXT).  These keep an asset alive but cannot be
rewritten -- prose, or a hand-written `[[file:data/...]]' path -- so they are
what the report asks a human to fix.

The rewritable links are blanked to spaces of equal width first: what remains is
exactly the residue, with every line number still intact."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (let ((re (le::denote-attach--link-regexp basename)))
      (while (re-search-forward re nil t)
        (replace-match (make-string (length (match-string 0)) ?\s) t t)))
    (goto-char (point-min))
    (let (occurrences)
      (while (search-forward basename nil t)
        (push (cons (line-number-at-pos (match-beginning 0))
                    (string-trim (buffer-substring-no-properties
                                  (line-beginning-position) (line-end-position))))
              occurrences))
      (nreverse occurrences))))

(defun le::denote-attach--tree-parent (tree)
  "The directory TREE's `attachment:' links resolve relative to.
`org-attach-id-dir' is relative to the referring file's own directory, so a link
resolves only from a file sitting directly in this directory."
  (directory-file-name (file-name-directory (directory-file-name tree))))

(defun le::denote-attach--references (asset corpus)
  "The references to ASSET found in CORPUS, as (:links ALIST :mentions ALIST).
CORPUS is an alist (FILE . TEXT).  Each alist maps a FILE to its non-empty list
of occurrences: `:links' the rewritable `attachment:' links, `:mentions'
everything else."
  (let ((basename (plist-get asset :basename))
        links mentions)
    (dolist (entry corpus)
      (let ((found (le::denote-attach--link-occurrences (cdr entry) basename))
            (other (le::denote-attach--mention-occurrences (cdr entry) basename)))
        (when found (push (cons (car entry) found) links))
        (when other (push (cons (car entry) other) mentions))))
    (list :links (nreverse links) :mentions (nreverse mentions))))

(defun le::denote-attach--status (asset links)
  "Whether ASSET's `attachment:' LINKS resolve today.
`working' when every referring file sits in the directory the asset's tree hangs
off, `broken' when any does not -- that link is already dead, so converting it
is a repair -- and `prose-only' when nothing links to the asset at all and only
a plain mention keeps it alive."
  (let ((parent (le::denote-attach--tree-parent (plist-get asset :tree))))
    (cond
     ((null links) 'prose-only)
     ((seq-every-p (lambda (link)
                     (equal (directory-file-name
                             (file-name-directory (expand-file-name (car link))))
                            parent))
                   links)
      'working)
     (t 'broken))))

(defun le::denote-attach--sweep-path (sweep-root playground path)
  "Where PATH lands under SWEEP-ROOT: its whole path relative to PLAYGROUND.
Mirroring the original layout keeps the `<2-char>/<rest-of-ID>/' provenance and
makes a collision impossible even when two trees hold the same file name."
  (expand-file-name (file-relative-name path playground) sweep-root))

(defun le::denote-attach--new-path (dest id ext)
  "The denote document path an asset with EXT becomes in DEST under ID."
  (denote-format-file-name (file-name-as-directory dest) id nil "screenshot" ext nil))

(defun le::denote-attach--plan (assets corpus dest sweep-root playground taken)
  "Decide what to do with ASSETS, given the playground's text as CORPUS.
ASSETS are plists (:path :tree :basename :ext :fallback), CORPUS an alist
\(FILE . TEXT), TAKEN the identifiers already in use.  DEST, SWEEP-ROOT and
PLAYGROUND are directories, used only to compute paths.  Return a plist:

  :convert    (:path :tree :basename :id :new-path :status :links :mentions)
  :sweep      (:path :tree :basename :new-path)
  :ambiguous  (:basename :paths) -- acted on by neither
  :file-edits (FILE . ((BASENAME . ID) ...)) -- the rewrites, grouped by file

A file name found at more than one path cannot be attributed to either from a
mention that names only the file, so such an asset is reported and left alone
rather than guessed at."
  (let* ((groups (seq-group-by (lambda (a) (plist-get a :basename)) assets))
         (ambiguous (seq-filter (lambda (g) (cdr (cdr g))) groups))
         (unique (seq-difference groups ambiguous))
         (taken (copy-sequence taken))
         convert sweep file-edits)
    (dolist (group unique)
      (let* ((asset (car (cdr group)))
             (refs (le::denote-attach--references asset corpus))
             (links (plist-get refs :links))
             (mentions (plist-get refs :mentions)))
        (if (and (null links) (null mentions))
            (push (append asset
                          (list :new-path (le::denote-attach--sweep-path
                                           sweep-root playground
                                           (plist-get asset :path))))
                  sweep)
          (let ((id (le::denote-attach--identifier
                     (plist-get asset :basename) (plist-get asset :fallback) taken)))
            (push id taken)
            (push (append asset
                          (list :id id
                                :new-path (le::denote-attach--new-path
                                           dest id (plist-get asset :ext))
                                :status (le::denote-attach--status asset links)
                                :links links
                                :mentions mentions))
                  convert)
            (dolist (link links)
              (let ((cell (assoc (car link) file-edits)))
                (if cell
                    (setcdr cell (append (cdr cell)
                                         (list (cons (plist-get asset :basename) id))))
                  (push (list (car link)
                              (cons (plist-get asset :basename) id))
                        file-edits))))))))
    (list :convert (nreverse convert)
          :sweep (nreverse sweep)
          :ambiguous (mapcar (lambda (g)
                               (list :basename (car g)
                                     :paths (mapcar (lambda (a) (plist-get a :path))
                                                    (cdr g))))
                             ambiguous)
          :file-edits (nreverse file-edits))))

(defun le::denote-attach--manual-fixes (plan)
  "The (FILE . COUNT) mentions in PLAN that no rewrite will repair, deduplicated.
Each is reported against the asset that produced it, so the same file may appear
once per asset."
  (apply #'append
         (mapcar (lambda (c)
                   (mapcar (lambda (m) (list :file (car m)
                                             :occurrences (cdr m)
                                             :basename (plist-get c :basename)))
                           (plist-get c :mentions)))
                 (plist-get plan :convert))))

;;; Report -----------------------------------------------------------------

(defconst le::denote-attach--status-labels
  '((working . "working") (broken . "already broken") (prose-only . "prose only"))
  "How each conversion status reads in the report.")

(defun le::denote-attach--rel (playground path)
  "PATH written relative to PLAYGROUND, for the report."
  (file-relative-name path playground))

(defun le::denote-attach--report (plan trees empty playground silo dest sweep-root)
  "Render PLAN as the report string shown before anything is touched.
TREES are the trees holding assets and EMPTY those skipped for holding none."
  (let* ((convert (plist-get plan :convert))
         (sweep (plist-get plan :sweep))
         (ambiguous (plist-get plan :ambiguous))
         (fixes (le::denote-attach--manual-fixes plan))
         (rel (lambda (p) (le::denote-attach--rel playground p))))
    (with-temp-buffer
      (insert "denote attachment migration\n\n")
      (insert (format "  silo:        %s\n" (abbreviate-file-name silo)))
      (insert (format "  playground:  %s\n" (abbreviate-file-name playground)))
      (insert (format "  destination: %s\n" (abbreviate-file-name dest)))
      (insert (format "  swept to:    %s\n\n" (abbreviate-file-name sweep-root)))
      (insert (format "  %d data/ tree(s) with assets%s\n"
                      (length trees)
                      (if empty
                          (format ", %d empty (skipped)" (length empty))
                        "")))
      (insert (format "  %d asset(s); %d to convert, %d to sweep aside"
                      (+ (length convert) (length sweep)
                         (apply #'+ (mapcar (lambda (a) (length (plist-get a :paths)))
                                            ambiguous)))
                      (length convert) (length sweep)))
      (insert (if ambiguous (format ", %d ambiguous\n" (length ambiguous)) "\n"))
      (when convert
        (insert "\nconvert to denote documents:\n")
        (dolist (c convert)
          (insert (format "\n  %s  [%s]  ->  %s\n"
                          (plist-get c :basename)
                          (alist-get (plist-get c :status)
                                     le::denote-attach--status-labels)
                          (file-name-nondirectory (plist-get c :new-path))))
          (dolist (link (plist-get c :links))
            (dolist (occurrence (cdr link))
              (insert (format "      %s:%d:\n          %s\n       -> %s\n"
                              (funcall rel (car link)) (car occurrence)
                              (cdr occurrence)
                              (car (le::denote-attach--rewrite-text
                                    (cdr occurrence)
                                    (plist-get c :basename)
                                    (plist-get c :id)))))))))
      (when fixes
        (insert "\nmentions NOT rewritten -- fix these by hand:\n")
        (dolist (f fixes)
          (insert (format "\n  %s\n" (plist-get f :basename)))
          (dolist (occurrence (plist-get f :occurrences))
            (insert (format "      %s:%d:\n          %s\n"
                            (funcall rel (plist-get f :file)) (car occurrence)
                            (truncate-string-to-width (cdr occurrence) 96 nil nil t))))))
      (when sweep
        (insert (format "\nsweep aside (unreferenced anywhere in the playground) -> %s:\n"
                        (funcall rel sweep-root)))
        (dolist (s sweep)
          (insert (format "  %s\n" (funcall rel (plist-get s :path))))))
      (when ambiguous
        (insert "\nambiguous -- one file name under several paths, acted on by NEITHER:\n")
        (dolist (a ambiguous)
          (insert (format "  %s\n" (plist-get a :basename)))
          (dolist (p (plist-get a :paths))
            (insert (format "      %s\n" (funcall rel p))))))
      (insert "\nafter every copy is verified, these tree(s) are deleted:\n")
      (dolist (tree trees)
        (insert (format "  %s\n" (funcall rel tree))))
      (dolist (tree empty)
        (insert (format "  %s (empty)\n" (funcall rel tree))))
      (buffer-string))))

(defun le::denote-attach--preview (report)
  "Show REPORT in *denote-attach* and return that buffer."
  (with-current-buffer (get-buffer-create "*denote-attach*")
    (special-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert report))
    (goto-char (point-min))
    (display-buffer (current-buffer))
    (current-buffer)))

;;; Imperative shell -------------------------------------------------------

(defun le::denote-attach--playground-root (dir)
  "The `.le-playground' directory DIR lives in, or nil.
Found by walking up to the checkout that HOLDS a `.le-playground', the same way
`le-denote-tasks' locates a note's project, then naming that entry."
  (when-let* ((checkout (locate-dominating-file dir ".le-playground")))
    (file-name-as-directory (expand-file-name ".le-playground" checkout))))

(defun le::denote-attach--data-trees (root)
  "Every directory named `data' under ROOT, `.git' never descended into."
  (seq-filter
   #'file-directory-p
   (directory-files-recursively
    root (rx bos "data" eos) t
    (lambda (dir)
      (not (equal (file-name-nondirectory (directory-file-name dir)) ".git"))))))

(defun le::denote-attach--tree-assets (tree)
  "The asset files under TREE: regular, non-dot, in a stable order.
Dot files are ignored -- `.DS_Store' is not an attachment -- so they neither
migrate nor keep a tree alive."
  (sort (seq-filter
         (lambda (f)
           (and (file-regular-p f)
                (not (string-prefix-p "." (file-name-nondirectory f)))))
         (directory-files-recursively tree ""))
        #'string<))

(defun le::denote-attach--asset (path tree)
  "The plist describing the asset at PATH inside TREE.
FALLBACK is derived from the file's modification time, and is used only when the
name carries no timestamp of its own."
  (let ((ext (file-name-extension path)))
    (list :path path
          :tree tree
          :basename (file-name-nondirectory path)
          :ext (if ext (concat "." ext) "")
          :fallback (format-time-string
                     "%Y%m%dT%H%M%S"
                     (file-attribute-modification-time (file-attributes path))))))

(defun le::denote-attach--under-any-p (path dirs)
  "Non-nil when PATH is inside any of DIRS."
  (let ((path (expand-file-name path)))
    (seq-some (lambda (dir)
                (string-prefix-p (file-name-as-directory (expand-file-name dir)) path))
              dirs)))

(defun le::denote-attach--text-contents (file)
  "FILE's contents as a string, or nil when it is binary or unreadable.
A NUL byte is the test: no text a reference could hide in contains one."
  (ignore-errors
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (unless (search-forward "\0" nil t)
        (buffer-string)))))

(defun le::denote-attach--corpus (playground excluded)
  "Read every text file under PLAYGROUND outside EXCLUDED; return (FILE . TEXT).
This is the reference corpus, and it is the WHOLE playground on purpose: an
asset in the silo may be cited from freeform scratch outside it, and a
silo-only scan would call such an asset an orphan and sweep a live reference's
target aside.  `tmp/' is included for the same reason -- keeping a dead asset is
untidy, sweeping a live one destroys a reference."
  (delq nil
        (mapcar
         (lambda (file)
           (unless (le::denote-attach--under-any-p file excluded)
             (when-let* ((text (le::denote-attach--text-contents file)))
               (cons file text))))
         (directory-files-recursively
          playground ""
          nil
          (lambda (dir)
            (not (equal (file-name-nondirectory (directory-file-name dir)) ".git")))))))

(defun le::denote-attach--taken-identifiers (silo)
  "The denote identifiers already in use anywhere in SILO."
  (delq nil (mapcar #'denote-retrieve-filename-identifier
                    (directory-files-recursively silo ""))))

(defun le::denote-attach--copy-verified (src dst)
  "Copy SRC to DST, creating parents; return non-nil once DST verifies.
Verification is a size comparison, and it gates the deletion of the source tree
-- nothing is removed on the strength of a copy that was not checked."
  (make-directory (file-name-directory dst) t)
  (when (file-exists-p dst)
    (user-error "Refusing to overwrite %s" (abbreviate-file-name dst)))
  (copy-file src dst nil t)
  (and (file-regular-p dst)
       (= (file-attribute-size (file-attributes src))
          (file-attribute-size (file-attributes dst)))))

(defun le::denote-attach--rewrite-file (file subs)
  "Apply SUBS, an alist (BASENAME . ID), to FILE's links; return the count.
The edit runs over the file's DISK contents in a temp buffer and is written back
with `write-region', the precedent `le::denote-logbook-append' sets: a visiting
buffer is never disturbed (an unmodified one auto-reverts), which matters
because the files carrying these links are exactly the journals the user keeps
open."
  (let ((text (le::denote-attach--text-contents file))
        (total 0))
    (dolist (sub subs)
      (let ((result (le::denote-attach--rewrite-text text (car sub) (cdr sub))))
        (setq text (car result)
              total (+ total (cdr result)))))
    (let ((coding-system-for-write 'utf-8))
      (write-region text nil file nil 'silent))
    total))

(defun le::denote-attach--remaining-assets (tree acted)
  "The assets still under TREE that ACTED (a list of paths) does not cover."
  (seq-remove (lambda (path) (member path acted))
              (le::denote-attach--tree-assets tree)))

;;;###autoload
(defun le::denote-migrate-attachments (&optional destination)
  "Retire this playground's org-attach `data/' trees into denote documents.
Discover every `data/' tree under the enclosing `.le-playground', then, for each
asset below them: convert it into a denote document in DESTINATION when anything
in the playground still mentions its file name, or move it under
<silo>/_tobedeleted/ when nothing does.  Every `[[attachment:FILE]]' aimed at a
converted asset is rewritten to `[[denote:ID]]', descriptions kept.  Finally the
emptied trees are deleted.

DESTINATION is read interactively, defaulting to <silo>/taskdata/ and created as
needed.  It is deliberately not <silo>/data/, which would itself be one of the
trees discovered here -- and so deleted at the end of the run.

The identifier of a converted asset is the timestamp already in its name
\(`clipboard-20260703T134035.png' -> `20260703T134035--screenshot.png'), so
capture time survives; a name without one falls back to the file's modification
time.

Nothing is touched before the report in *denote-attach* is shown and confirmed.
Assets are COPIED and verified first; a `data/' tree is deleted only once every
asset in it has been acted on and verified, so a tree holding something this
command refused to judge -- a file name found under two trees, which no mention
can attribute to either -- survives and is reported.

Only `attachment:' links are rewritten.  An asset kept alive by prose, or by a
hand-written `file:' path, is converted while that mention still points at the
old location; every such mention is listed for a manual fix rather than hidden.

Dead `:ID:' properties are left alone: whether org-attach ids should be pruned
is a separate judgement.

Signal a `user-error' when there is no denote silo or no enclosing
`.le-playground', when DESTINATION sits inside a `data/' tree that holds assets,
or when no `data/' tree holds anything at all."
  (interactive)
  (let* ((silo (file-name-as-directory
                (or (car (denote-directories))
                    (user-error "No denote silo here (is `denote-directory' set?)"))))
         (playground
          (or (le::denote-attach--playground-root silo)
              (user-error "No `.le-playground' above %s" (abbreviate-file-name silo))))
         (dest (file-name-as-directory
                (expand-file-name
                 (or destination
                     (read-directory-name
                      "Convert attachments into: " (expand-file-name "taskdata" silo)
                      nil nil nil)))))
         (sweep-root (file-name-as-directory (expand-file-name "_tobedeleted" silo)))
         (all-trees (le::denote-attach--data-trees playground))
         (trees (seq-remove (lambda (tree)
                              (null (le::denote-attach--tree-assets tree)))
                            all-trees))
         (empty (seq-difference all-trees trees)))
    ;; The destination must not be inside a tree this run would delete.  The
    ;; default cannot be, but the prompt accepts anything.
    (when-let* ((host (seq-find (lambda (tree)
                                  (le::denote-attach--under-any-p dest (list tree)))
                                trees)))
      (user-error "Destination %s is inside %s, a data/ tree with assets -- choose another"
                  (abbreviate-file-name dest) (abbreviate-file-name host)))
    (setq trees (seq-remove (lambda (tree)
                              (le::denote-attach--under-any-p tree (list dest)))
                            trees))
    (unless trees
      (user-error "No data/ tree with assets under %s" (abbreviate-file-name playground)))
    (let* ((assets (apply #'append
                          (mapcar (lambda (tree)
                                    (mapcar (lambda (path)
                                              (le::denote-attach--asset path tree))
                                            (le::denote-attach--tree-assets tree)))
                                  trees)))
           (corpus (le::denote-attach--corpus
                    playground (append all-trees (list sweep-root dest))))
           (plan (le::denote-attach--plan assets corpus dest sweep-root playground
                                          (le::denote-attach--taken-identifiers silo)))
           (convert (plist-get plan :convert))
           (sweep (plist-get plan :sweep))
           (report (le::denote-attach--report
                    plan trees empty playground silo dest sweep-root)))
      (le::denote-attach--preview report)
      (let ((proceed (yes-or-no-p
                      (format "Convert %d asset(s), sweep %d aside, then delete %d tree(s)? "
                              (length convert) (length sweep) (length trees)))))
        (quit-windows-on "*denote-attach*")
        (unless proceed
          (user-error "Migration abandoned; nothing was touched"))
        (let ((acted '())
              (rewritten 0))
          ;; Copy and verify everything before a single source is removed.
          (dolist (c convert)
            (unless (le::denote-attach--copy-verified
                     (plist-get c :path) (plist-get c :new-path))
              (error "Copy of %s did not verify" (abbreviate-file-name (plist-get c :path))))
            (push (plist-get c :path) acted))
          (dolist (s sweep)
            (unless (le::denote-attach--copy-verified
                     (plist-get s :path) (plist-get s :new-path))
              (error "Copy of %s did not verify" (abbreviate-file-name (plist-get s :path))))
            (push (plist-get s :path) acted))
          (dolist (edit (plist-get plan :file-edits))
            (setq rewritten
                  (+ rewritten (le::denote-attach--rewrite-file (car edit) (cdr edit)))))
          ;; Only now: a tree whose every asset was copied and verified is gone.
          (let (kept)
            (dolist (tree (append trees empty))
              (if (le::denote-attach--remaining-assets tree acted)
                  (push tree kept)
                (delete-directory tree t)))
            (message
             (concat
              (format "Converted %d, swept %d, rewrote %d link(s) in %d file(s)"
                      (length convert) (length sweep) rewritten
                      (length (plist-get plan :file-edits)))
              (if kept
                  (format "; kept %d tree(s) holding unjudged assets: %s"
                          (length kept)
                          (mapconcat (lambda (t2)
                                       (le::denote-attach--rel playground t2))
                                     kept ", "))
                (format "; deleted %d tree(s)" (length (append trees empty))))
              (when-let* ((fixes (le::denote-attach--manual-fixes plan)))
                (format " -- %d mention(s) still need a manual fix" (length fixes)))))))))))

(provide 'le-denote-attach)
;;; le-denote-attach.el ends here
