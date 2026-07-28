;;; le-denote-attach.el --- Retire org-attach data/ trees into denote documents  -*- lexical-binding: t; -*-

;;; Commentary:

;; An `[[attachment:FILE]]' link resolves through the enclosing heading's `:ID:'
;; AND through `org-attach-id-dir' (`data/') taken relative to THE ORG FILE'S
;; OWN DIRECTORY.  Both halves are static, so moving a heading between files
;; silently moves its attachment lookup with it -- and breaks it.  A playground
;; that has had headings exported or promoted therefore accumulates several
;; `data/' trees at different levels, only some of whose links still resolve.
;;
;; `le::denote-migrate-attachments' retires the whole arrangement in one
;; playground-wide run.  Denote identifiers are position-independent, so an
;; asset converted into a denote document survives every later move.
;;
;;   still referenced -> a denote document in the destination, and every
;;                       reference to it that CAN be rewritten becomes a
;;                       denote link
;;   no longer referenced -> moved under <playground>/_tobedeleted/, mirroring
;;                       its path relative to the playground so provenance
;;                       survives
;;   then                 -> the emptied `data/' trees are deleted
;;
;; The identifier is the CAPTURE TIMESTAMP already embedded in the file name and
;; the title is `screenshot', so `clipboard-20260703T134035.png' becomes
;; `20260703T134035--screenshot.png' -- the shape `le::denote-yank-media'
;; writes, with capture time preserved rather than replaced by migration time.
;;
;; "Referenced" is the BROADEST test: any mention of the asset's file name
;; anywhere in the playground, every file type, links and prose alike.  That is
;; deliberate.  A displaced heading's `attachment:' link cannot be resolved by
;; org-attach from its new home, so resolving links properly would report
;; exactly the references that most need finding as absent.  Matching the file
;; name finds them.
;;
;; Four kinds of reference are rewritten, and one is not:
;;
;;   attachment  `[[attachment:F]]' / `[[attachment:F][D]]' -- the link this
;;               migration exists for.
;;   file-link   an Org `[[file:.../F]]' -- the same repair, hand-written.
;;   path        the file name written as the last component of a PATH.  The
;;               WHOLE path is replaced by a path to the converted document, in
;;               whichever style the original used.  A path needs no link
;;               syntax, so nothing about the file's type or its surroundings
;;               disqualifies it -- which is what makes the awkward mentions
;;               (a path inside backticks in a Markdown file, say) repairable.
;;   prose       a bare mention of the file name, replaced by a DESCRIBED
;;               denote link whose description is the file name itself, so the
;;               rendered text is unchanged and only the reference is added.
;;   manual      a BARE name where a denote link would not render -- inside a
;;               verbatim or code span, or in a file type with no denote link
;;               syntax.  Reported with the reason, never touched.
;;
;; Trees are discovered under the SILO, not the whole playground: a denote link
;; resolves by identifier within the silo, so an asset has to live there to be
;; linkable.  A `data/' tree elsewhere in the playground is reported in a banner
;; at the top of the preview and left alone -- moving it, and the org files that
;; reference it, into the silo is a manual step.
;;
;; The destination defaults to <silo>/taskdata/ -- deliberately not
;; <silo>/data/, which would itself be one of the trees this command discovers
;; and deletes, and a name org-attach never looks for cannot be mistaken for an
;; attachment store.  `_tobedeleted/' sits at the PLAYGROUND root, outside the
;; silo: a swept asset is not a denote note and has no business among them.
;; Being outside the silo also keeps it out of tree discovery, which matters
;; because it MIRRORS the layout it swept, directories named `data' included.
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

;;; Pure core: identifiers -------------------------------------------------

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

;;; Pure core: what a reference looks like ---------------------------------

(defun le::denote-attach--attachment-regexp (basename)
  "Regexp matching an Org `attachment:' link whose target is exactly BASENAME.
Group 1 is the link's description, when it has one.  The target is bracketed by
`[[attachment:' and either `]]' or `][', so an asset whose name is a SUBSTRING
of another's can never be caught by the wrong rewrite."
  (rx "[[attachment:" (literal basename)
      (opt "][" (group (*? nonl)))
      "]]"))

(defun le::denote-attach--file-link-regexp (basename)
  "Regexp matching an Org `file:' link whose target's last component is BASENAME.
Group 1 is the description, when present.  The optional leading path must end
in a slash, so BASENAME is matched as a whole component and never as the tail
of a longer file name."
  (rx "[[file:"
      (opt (* (not (any "]\n"))) "/")
      (literal basename)
      (opt "][" (group (*? nonl)))
      "]]"))

(defun le::denote-attach--link-format (file)
  "The denote link format string for FILE, or nil when its type has none.
Both `denote-org-link-format' and `denote-md-link-format' take the identifier
first and the description second, so one `format' call serves either."
  (pcase (downcase (or (file-name-extension file) ""))
    ((or "org" "txt") denote-org-link-format)
    ("md" denote-md-link-format)))

(defun le::denote-attach--protected-regions (text org)
  "The (BEG . END) ranges of TEXT in which a denote link would not render.
ORG selects Org's verbatim markup (`~..~', `=..=', `#+begin_'/`#+end_' blocks)
over Markdown's (backticks and fenced blocks).  Positions are buffer positions
in a temp buffer holding TEXT, i.e. 1-based.

A link written inside one of these shows up as its own markup instead of a
link, so a mention found there is reported rather than rewritten."
  (with-temp-buffer
    (insert text)
    (let (regions)
      (goto-char (point-min))
      (let ((open (if org
                      (rx bol (* space) "#+begin_" (* nonl) eol)
                    (rx bol (* space) "```" (* nonl) eol)))
            (close (if org
                       (rx bol (* space) "#+end_" (* nonl) eol)
                     (rx bol (* space) "```" (* space) eol))))
        (while (re-search-forward open nil t)
          (let ((beg (match-beginning 0)))
            (push (cons beg (if (re-search-forward close nil t)
                                (match-end 0)
                              (point-max)))
                  regions))))
      (dolist (re (if org
                      (list (rx "~" (+ (not (any "~\n"))) "~")
                            (rx "=" (+ (not (any "=\n"))) "="))
                    (list (rx "`" (+ (not (any "`\n"))) "`"))))
        (goto-char (point-min))
        (while (re-search-forward re nil t)
          (push (cons (match-beginning 0) (match-end 0)) regions)))
      regions)))

(defun le::denote-attach--covered-p (pos ranges)
  "Non-nil when POS falls inside any (BEG . END) of RANGES."
  (seq-some (lambda (range) (and (>= pos (car range)) (< pos (cdr range)))) ranges))

(defconst le::denote-attach--name-char-rx "[A-Za-z0-9_.-]"
  "Characters that, next to a file name, mean it is part of a LONGER name.")

(defconst le::denote-attach--path-chars "A-Za-z0-9_./-"
  "Characters a written file path is made of, as a `skip-chars-backward' set.
`~' is deliberately absent: it belongs to a path only as a leading `~/', and
treating it as an ordinary path character would read the closing `~' of Org's
`~verbatim~' markup as part of the file name.")

(defun le::denote-attach--path-token-start (pos)
  "The start of the path token ending at POS in the current buffer.
Walks back over path characters, so the file name at POS is returned together
with whatever directories were written in front of it.  A leading `~' is taken
only when a `/' follows it, i.e. when it really is a home-relative path."
  (save-excursion
    (goto-char pos)
    (skip-chars-backward le::denote-attach--path-chars)
    (when (and (eq (char-before) ?~) (eq (char-after) ?/))
      (forward-char -1))
    (point)))

(defun le::denote-attach--path-replacement (token new-path playground file)
  "The path replacing TOKEN so it names NEW-PATH, written TOKEN's own way.
An absolute token stays absolute and a `~' one stays abbreviated; a token
anchored at `.le-playground/' keeps that anchor; anything else is a path
relative to FILE's own directory, which is the only reading that resolves.
PLAYGROUND is the playground root."
  (cond
   ((string-prefix-p "/" token) new-path)
   ((string-prefix-p "~" token) (abbreviate-file-name new-path))
   ((string-prefix-p ".le-playground/" token)
    (concat ".le-playground/" (file-relative-name new-path playground)))
   (t (file-relative-name new-path (file-name-directory file)))))

(defun le::denote-attach--occurrences (text basename file)
  "Every reference to BASENAME in TEXT, classified, in buffer order.
FILE names the containing file, and decides both its link syntax and its
verbatim markup.  Each occurrence is a plist:

  :kind   `attachment', `file-link', `path', `prose' (rewritable) or `manual'
  :why    for `manual', one of `:verbatim', `:unsupported'
  :line   1-based line number
  :beg    :end   character range within TEXT (1-based, as in a buffer)
  :before the text being replaced (a link, a whole path, or the bare file name)
  :text   the whole line, for the report
  :desc   an existing link's description, when it had one

An occurrence whose neighbouring characters show the file name is only part of
a LONGER name is not a reference at all, and is dropped.

A file name written as the last component of a PATH takes the whole path as its
span and is replaced by a path to the converted document: a path needs no link
syntax, so neither the file's type nor a surrounding verbatim span disqualifies
it -- which is what makes the awkward cases repairable rather than manual."
  (let* ((format (le::denote-attach--link-format file))
         (org (member (downcase (or (file-name-extension file) "")) '("org" "txt")))
         (protected (le::denote-attach--protected-regions text org))
         occurrences covered)
    (with-temp-buffer
      (insert text)
      ;; Links first, and their spans are then off-limits to the bare-name pass
      ;; -- the file name inside `[[attachment:F]]' is not a second reference.
      (dolist (pass (append
                     (list (cons 'attachment
                                 (le::denote-attach--attachment-regexp basename)))
                     (when org
                       (list (cons 'file-link
                                   (le::denote-attach--file-link-regexp basename))))))
        (goto-char (point-min))
        (while (re-search-forward (cdr pass) nil t)
          (push (cons (match-beginning 0) (match-end 0)) covered)
          (push (list :kind (car pass)
                      :line (line-number-at-pos (match-beginning 0))
                      :beg (match-beginning 0)
                      :end (match-end 0)
                      :before (match-string 0)
                      :desc (match-string 1)
                      :text (buffer-substring-no-properties
                             (line-beginning-position) (line-end-position)))
                occurrences)))
      (goto-char (point-min))
      (while (search-forward basename nil t)
        (let* ((beg (match-beginning 0))
               (end (match-end 0))
               (prev (when (> beg (point-min)) (char-to-string (char-before beg)))))
          (unless (or (le::denote-attach--covered-p beg covered)
                      ;; A LONGER file name that merely contains this one:
                      ;; `a.png.bak' for `a.png'.  A separator alone does not
                      ;; say so -- a mention can end a sentence -- so it counts
                      ;; only when something name-like follows it.
                      (looking-at (rx (or (any "A-Za-z0-9")
                                          (seq (any "._-") (any "A-Za-z0-9")))))
                      (and prev (not (equal prev "/"))
                           (string-match-p le::denote-attach--name-char-rx prev)))
            (let* ((token-start (le::denote-attach--path-token-start beg))
                   ;; A written path, either because directories precede the
                   ;; name or because it sits in a Markdown link target, where
                   ;; only a path belongs.
                   (path-p (or (< token-start beg)
                               (and (> beg (+ (point-min) 1))
                                    (equal (buffer-substring-no-properties
                                            (- beg 2) beg)
                                           "]("))))
                   (kind (cond (path-p 'path)
                               ((null format) 'manual)
                               ((le::denote-attach--covered-p beg protected) 'manual)
                               (t 'prose))))
              (push (list :kind kind
                          :why (when (eq kind 'manual)
                                 (if format :verbatim :unsupported))
                          :line (line-number-at-pos beg)
                          :beg (if path-p token-start beg)
                          :end end
                          :before (buffer-substring-no-properties
                                   (if path-p token-start beg) end)
                          :text (buffer-substring-no-properties
                                 (line-beginning-position) (line-end-position)))
                    occurrences))))))
    (sort (nreverse occurrences)
          (lambda (a b) (< (plist-get a :beg) (plist-get b :beg))))))

(defun le::denote-attach--replacement (occurrence context)
  "What OCCURRENCE becomes, or nil when it stays put.
CONTEXT is a plist (:id :new-path :file :playground) describing the conversion.
An existing link keeps its description; a bare mention GAINS one -- the file
name itself -- so the rendered text is exactly what it was and only the
reference is new.  A written path becomes a path to the converted document,
since link syntax would be wrong where a path is expected."
  (let ((id (plist-get context :id))
        (file (plist-get context :file)))
    (pcase (plist-get occurrence :kind)
      ((or 'attachment 'file-link)
       (if-let* ((desc (plist-get occurrence :desc)))
           (format denote-org-link-format id desc)
         (format "[[denote:%s]]" id)))
      ('path (le::denote-attach--path-replacement
              (plist-get occurrence :before)
              (plist-get context :new-path)
              (plist-get context :playground)
              file))
      ('prose (format (le::denote-attach--link-format file)
                      id (plist-get occurrence :before))))))

(defun le::denote-attach--rewrite (text basename context)
  "Rewrite TEXT's rewritable references to BASENAME, per CONTEXT.
CONTEXT is as `le::denote-attach--replacement' takes it.  Return
\(NEW-TEXT . COUNT).  Replacements are applied back to front, so each one's
recorded range is still valid when its turn comes."
  (let ((occurrences (le::denote-attach--occurrences
                      text basename (plist-get context :file)))
        (count 0))
    (with-temp-buffer
      (insert text)
      (dolist (occurrence (reverse occurrences))
        (when-let* ((new (le::denote-attach--replacement occurrence context)))
          (delete-region (plist-get occurrence :beg) (plist-get occurrence :end))
          (goto-char (plist-get occurrence :beg))
          (insert new)
          (setq count (1+ count))))
      (cons (buffer-string) count))))

;;; Pure core: the partition ----------------------------------------------

(defun le::denote-attach--tree-parent (tree)
  "The directory TREE's `attachment:' links resolve relative to.
`org-attach-id-dir' is relative to the referring file's own directory, so a link
resolves only from a file sitting directly in this directory."
  (directory-file-name (file-name-directory (directory-file-name tree))))

(defun le::denote-attach--references (asset corpus)
  "The references to ASSET found in CORPUS, as (:edits ALIST :manual ALIST).
CORPUS is an alist (FILE . TEXT).  Each alist maps a FILE to its non-empty list
of occurrence plists: `:edits' those a rewrite will handle, `:manual' those a
human must."
  (let ((basename (plist-get asset :basename))
        edits manual)
    (dolist (entry corpus)
      (let* ((occurrences (le::denote-attach--occurrences
                           (cdr entry) basename (car entry)))
             (rewritable (seq-remove (lambda (o) (eq (plist-get o :kind) 'manual))
                                     occurrences))
             (rest (seq-filter (lambda (o) (eq (plist-get o :kind) 'manual))
                               occurrences)))
        (when rewritable (push (cons (car entry) rewritable) edits))
        (when rest (push (cons (car entry) rest) manual))))
    (list :edits (nreverse edits) :manual (nreverse manual))))

(defun le::denote-attach--status (asset edits)
  "Whether ASSET's `attachment:' links in EDITS resolve today.
`working' when every file carrying one sits in the directory the asset's tree
hangs off, `broken' when any does not -- that link is already dead, so
converting it is a repair -- and `prose-only' when nothing links to the asset by
attachment at all and only a plain mention or hand-written path keeps it alive."
  (let* ((parent (le::denote-attach--tree-parent (plist-get asset :tree)))
         (citers (seq-filter
                  (lambda (entry)
                    (seq-some (lambda (o) (eq (plist-get o :kind) 'attachment))
                              (cdr entry)))
                  edits)))
    (cond
     ((null citers) 'prose-only)
     ((seq-every-p (lambda (entry)
                     (equal (directory-file-name
                             (file-name-directory (expand-file-name (car entry))))
                            parent))
                   citers)
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

  :convert    (:path :tree :basename :id :new-path :status :edits :manual)
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
             (edits (plist-get refs :edits))
             (manual (plist-get refs :manual)))
        (if (and (null edits) (null manual))
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
                                :status (le::denote-attach--status asset edits)
                                :edits edits
                                :manual manual))
                  convert)
            (dolist (entry edits)
              (let ((cell (assoc (car entry) file-edits))
                    (sub (list :basename (plist-get asset :basename)
                               :id id
                               :new-path (le::denote-attach--new-path
                                          dest id (plist-get asset :ext)))))
                (if cell
                    (setcdr cell (append (cdr cell) (list sub)))
                  (push (list (car entry) sub) file-edits))))))))
    (list :convert (nreverse convert)
          :sweep (nreverse sweep)
          :ambiguous (mapcar (lambda (g)
                               (list :basename (car g)
                                     :paths (mapcar (lambda (a) (plist-get a :path))
                                                    (cdr g))))
                             ambiguous)
          :file-edits (nreverse file-edits))))

(defun le::denote-attach--manual-fixes (plan)
  "The occurrences in PLAN that no rewrite will repair, flattened.
Each is (:basename :file :occurrence), so the report can name what needs the
fix, where, and why."
  (apply #'append
         (mapcar (lambda (c)
                   (apply #'append
                          (mapcar (lambda (entry)
                                    (mapcar (lambda (o)
                                              (list :basename (plist-get c :basename)
                                                    :file (car entry)
                                                    :occurrence o))
                                            (cdr entry)))
                                  (plist-get c :manual))))
                 (plist-get plan :convert))))

;;; Report -----------------------------------------------------------------

(defconst le::denote-attach--status-labels
  '((working . "working") (broken . "already broken") (prose-only . "prose only"))
  "How each conversion status reads in the report.")

(defconst le::denote-attach--kind-labels
  '((attachment . "attachment link") (file-link . "file: link")
    (path . "path") (prose . "prose"))
  "How each rewritable reference kind reads in the report.")

(defconst le::denote-attach--why-labels
  '((:verbatim . "bare name inside verbatim/code, where a link would not render")
    (:unsupported . "bare name in a file type with no denote link syntax"))
  "Why a mention is left for a human.")

(defun le::denote-attach--rel (playground path)
  "PATH written relative to PLAYGROUND, for the report."
  (file-relative-name path playground))

(defun le::denote-attach--outside-warning (outside playground)
  "The banner naming OUTSIDE, the out-of-silo `data/' trees, or nil when none.
It heads the report because it is the one thing here that needs a decision
BEFORE the run rather than a review of it: those assets cannot be migrated, and
the run about to be confirmed will not touch them."
  (when outside
    (concat
     (propertize
      (format "!!! %d data/ directory(ies) OUTSIDE the silo -- NOT MIGRATED !!!\n"
              (length outside))
      'face '(bold warning))
     (mapconcat (lambda (tree)
                  (format "      %s\n" (le::denote-attach--rel playground tree)))
                outside "")
     (propertize
      "    Move these, and the org files that reference them, INTO the silo
    first: a denote link resolves by identifier within the silo, so an asset
    outside it cannot be linked at all.  Then run this again.\n\n"
      'face 'bold))))

(defun le::denote-attach--report (plan trees empty outside playground silo dest sweep-root)
  "Render PLAN as the report string shown before anything is touched.
TREES are the trees holding assets, EMPTY those skipped for holding none, and
OUTSIDE the `data/' trees outside the silo that this run cannot migrate."
  (let* ((convert (plist-get plan :convert))
         (sweep (plist-get plan :sweep))
         (ambiguous (plist-get plan :ambiguous))
         (fixes (le::denote-attach--manual-fixes plan))
         (rel (lambda (p) (le::denote-attach--rel playground p))))
    (with-temp-buffer
      (when-let* ((warning (le::denote-attach--outside-warning outside playground)))
        (insert warning))
      (insert "denote attachment migration\n\n")
      (insert (format "  silo:        %s\n" (abbreviate-file-name silo)))
      (insert (format "  playground:  %s\n" (abbreviate-file-name playground)))
      (insert (format "  destination: %s\n" (abbreviate-file-name dest)))
      (insert (format "  swept to:    %s\n\n" (abbreviate-file-name sweep-root)))
      (insert (format "  %d data/ tree(s) with assets%s\n"
                      (length trees)
                      (if empty (format ", %d empty (skipped)" (length empty)) "")))
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
          (dolist (entry (plist-get c :edits))
            (dolist (o (cdr entry))
              (insert (format "      %s:%d: (%s)\n          %s\n       -> %s\n"
                              (funcall rel (car entry)) (plist-get o :line)
                              (alist-get (plist-get o :kind)
                                         le::denote-attach--kind-labels)
                              (plist-get o :before)
                              (le::denote-attach--replacement
                               o (list :id (plist-get c :id)
                                       :new-path (plist-get c :new-path)
                                       :file (car entry)
                                       :playground playground))))))))
      (when fixes
        (insert "\nmentions NOT rewritten -- fix these by hand:\n")
        (dolist (f fixes)
          (let ((o (plist-get f :occurrence)))
            (insert (format "\n  %s\n      %s:%d: -- %s\n          %s\n"
                            (plist-get f :basename)
                            (funcall rel (plist-get f :file)) (plist-get o :line)
                            (alist-get (plist-get o :why)
                                       le::denote-attach--why-labels)
                            (truncate-string-to-width
                             (string-trim (plist-get o :text)) 96 nil nil t))))))
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

(defconst le::denote-attach--sweep-dir-name "_tobedeleted"
  "Directory, at the playground root, that swept assets are staged in.
Never descended into when looking for `data/' trees: it MIRRORS the layout it
swept, directories named `data' included, so a second run would otherwise
rediscover the swept assets and migrate them again.")

(defun le::denote-attach--playground-root (dir)
  "The `.le-playground' directory DIR lives in, or nil.
Found by walking up to the checkout that HOLDS a `.le-playground', the same way
`le-denote-tasks' locates a note's project, then naming that entry."
  (when-let* ((checkout (locate-dominating-file dir ".le-playground")))
    (file-name-as-directory (expand-file-name ".le-playground" checkout))))

(defun le::denote-attach--skip-dir-p (dir)
  "Non-nil when DIR must never be descended into."
  (member (file-name-nondirectory (directory-file-name dir))
          (list ".git" le::denote-attach--sweep-dir-name)))

(defun le::denote-attach--data-trees (root)
  "Every directory named `data' under ROOT, skipping `.git' and the sweep dir."
  (seq-filter
   #'file-directory-p
   (directory-files-recursively
    root (rx bos "data" eos) t
    (lambda (dir) (not (le::denote-attach--skip-dir-p dir))))))

(defun le::denote-attach--outside-trees (playground silo)
  "The `data/' trees under PLAYGROUND that lie OUTSIDE SILO.
These are NOT migrated.  A denote link resolves by identifier within the silo,
so an asset has to live there to be linkable at all; a `data/' tree outside it
belongs to org files that have not moved in yet, and moving those files together
with their tree is a judgement for the user rather than something to infer.
They are reported so a run says what to move before it can help."
  (seq-remove (lambda (tree) (le::denote-attach--under-any-p tree (list silo)))
              (le::denote-attach--data-trees playground)))

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
The fallback identifier comes from the file's modification time, and is used
only when the name carries no timestamp of its own."
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
          playground "" nil
          (lambda (dir) (not (le::denote-attach--skip-dir-p dir)))))))

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

(defun le::denote-attach--rewrite-file (file subs playground)
  "Apply SUBS to FILE's references; return how many were rewritten.
SUBS is a list of plists (:basename :id :new-path), one per asset FILE refers
to.  PLAYGROUND anchors the paths written for `path' references.
The edit runs over the file's DISK contents in a temp buffer and is written back
with `write-region', the precedent `le::denote-logbook-append' sets: a visiting
buffer is never disturbed (an unmodified one auto-reverts), which matters
because the files carrying these links are exactly the journals the user keeps
open."
  (let ((text (le::denote-attach--text-contents file))
        (total 0))
    (dolist (sub subs)
      (let ((result (le::denote-attach--rewrite
                     text (plist-get sub :basename)
                     (list :id (plist-get sub :id)
                           :new-path (plist-get sub :new-path)
                           :file file
                           :playground playground))))
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
  "Retire this silo's org-attach `data/' trees into denote documents.
Discover every `data/' tree under the denote SILO, then, for each asset below
them: convert it into a denote document in DESTINATION when anything in the
playground still mentions its file name, or move it under
<playground>/_tobedeleted/ when nothing does.  Finally the emptied trees are
deleted.

A `data/' tree elsewhere in the playground is NOT migrated -- a denote link
resolves by identifier within the silo, so an asset outside it cannot be linked
at all.  Such trees are named in a banner at the top of the preview; moving
them, and the org files that reference them, into the silo is a manual step.

Every reference that CAN be repaired is.  An `[[attachment:F]]' link and an Org
`[[file:.../F]]' link become denote links, keeping any description.  A bare
mention in prose becomes a DESCRIBED denote link whose description is the file
name, so the rendered text does not change and only the reference is added.  A
file name written as part of a PATH has the WHOLE path replaced by a path to the
converted document, in the style the original used -- link syntax would be wrong
where a path is expected, which is also why neither the file's type nor a
surrounding code span matters there.

Only a BARE name whose file type has no denote link syntax, or which sits inside
verbatim or a code block, is left alone -- reported with the reason.

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
         (sweep-root (file-name-as-directory
                      (expand-file-name le::denote-attach--sweep-dir-name playground)))
         ;; Rooted at the SILO, not the playground: an asset has to live in the
         ;; silo to be reachable by a denote link at all.  A tree outside it is
         ;; reported, never migrated -- moving it in is a manual step.
         (all-trees (le::denote-attach--data-trees silo))
         (outside (le::denote-attach--outside-trees playground silo))
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
                    plan trees empty outside playground silo dest sweep-root)))
      (le::denote-attach--preview report)
      (let ((proceed (yes-or-no-p
                      (format "%sConvert %d asset(s), sweep %d aside, then delete %d tree(s)? "
                              (if outside
                                  (format "[%d data/ dir(s) OUTSIDE the silo NOT migrated] "
                                          (length outside))
                                "")
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
              (error "Copy of %s did not verify"
                     (abbreviate-file-name (plist-get c :path))))
            (push (plist-get c :path) acted))
          (dolist (s sweep)
            (unless (le::denote-attach--copy-verified
                     (plist-get s :path) (plist-get s :new-path))
              (error "Copy of %s did not verify"
                     (abbreviate-file-name (plist-get s :path))))
            (push (plist-get s :path) acted))
          (dolist (edit (plist-get plan :file-edits))
            (setq rewritten
                  (+ rewritten (le::denote-attach--rewrite-file
                                (car edit) (cdr edit) playground))))
          ;; Only now: a tree whose every asset was copied and verified is gone.
          (let (kept)
            (dolist (tree (append trees empty))
              (if (le::denote-attach--remaining-assets tree acted)
                  (push tree kept)
                (delete-directory tree t)))
            (message
             (concat
              (format "Converted %d, swept %d, rewrote %d reference(s) in %d file(s)"
                      (length convert) (length sweep) rewritten
                      (length (plist-get plan :file-edits)))
              (if kept
                  (format "; kept %d tree(s) holding unjudged assets: %s"
                          (length kept)
                          (mapconcat (lambda (tree)
                                       (le::denote-attach--rel playground tree))
                                     kept ", "))
                (format "; deleted %d tree(s)" (length (append trees empty))))
              (when-let* ((fixes (le::denote-attach--manual-fixes plan)))
                (format " -- %d mention(s) still need a manual fix"
                        (length fixes)))))))))))

(provide 'le-denote-attach)
;;; le-denote-attach.el ends here
