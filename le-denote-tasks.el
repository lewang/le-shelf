;;; le-denote-tasks.el --- Project/worktree filetags for denote task notes  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tag a denote note with the identity of the git checkout it lives under, so a
;; journal or task entry records which project (and which worktree of it) it
;; belongs to.  Org notes only -- the `#+filetags:' this manages are Org front
;; matter -- so every entry point verifies the file type before acting.  The tags are denote-native atomic keywords -- a `project'/`worktree'
;; prefix fused to the collapsed checkout name -- so the filename and the org
;; `#+filetags:' stay in lockstep and no forbidden character (hyphen, underscore,
;; space) ever lands inside a single keyword.
;;
;;   main checkout of `goodbear'        -> keyword `projectgoodbear'
;;   worktree `wt-2' of `goodbear'      -> keywords `projectgoodbear' `worktreewt2'
;;
;; Identity comes from the ENCLOSING checkout -- the dir holding `.le-playground'
;; that contains the note -- NOT the note's own silo (a `.le-playground/denote'
;; silo is itself a linked worktree of the central playground repo, so its git
;; would report the wrong project).  `le::denote-tasks--checkout' walks up from the
;; file to that dir; a single `git worktree list --porcelain' then names it (the
;; same plumbing `magit-list-worktrees' wraps, so no magit dependency is worth
;; taking, and vc has no worktree API at all):
;;   project  = basename of the git MAIN worktree (the first entry)
;;   worktree = basename of the checkout dir, only when its path differs from the
;;              main entry (i.e. it is a LINKED worktree), with a leading copy of
;;              the project name stripped so the tag never repeats it (wt2, not
;;              goodbearwt2)
;;
;; `le::denote-tasks-set-frontmatter' is FRONT MATTER FIRST: the `#+filetags:'
;; is the source of truth, the file name a regenerated cache of which only the
;; identifier is guaranteed stable.  So it (1) unions the tags into the note's
;; current front-matter keywords and writes them back canonically with
;; `le::denote-tasks--rewrite-keywords' -- no rename, standing on its own --
;; then (2)
;; renames the file to match that front matter (or reports the name is already
;; correct).  Every change is confirmed with a prompt unless NO-CONFIRM, in
;; which case all changes apply silently and the buffer is saved.
;;
;; It is a normal function, not just a command: FILE, TAGS and NO-CONFIRM are
;; arguments (the interactive spec fills them from the buffer, a nil TAGS to
;; derive, and the prefix arg), so Lisp can tag any file unattended --
;; e.g. sweep a tree, using the pure query `le::denote-tasks-checkout-tags' to
;; pick each file's project/worktree tags.  Each file's work runs inside that
;; file's own buffer, so the right silo's `.dir-locals.el' are applied -- a
;; cross-silo batch stays correct file by file, not stuck with the caller's
;; ambient denote settings.
;;
;; `le::denote-tasks-dired-set-frontmatter-marked' is that sweep from Dired: it
;; plans the marked files (`le::denote-tasks--plan' drops those needing
;; nothing), then either tags them all (prefix arg) or walks them with the
;; usual `map-y-or-n-p' y/n/!/q prompt -- each accepted file applied
;; atomically.
;;
;; `le::denote-tasks-find-journal' reads those same tags the other way round: it
;; opens the journal note of a checkout, creating it when there is none.  The
;; wanted note is the one whose title is `journal <checkout root basename>' and
;; whose keywords cover the checkout's tags -- per CHECKOUT, so a linked worktree
;; keeps its own journal.  It resolves the checkout by climbing to the nearest
;; `.le-playground' rather than consulting `denote-directory', because it is
;; meant to fire from a source buffer where the silo's `.dir-locals.el' never
;; applies, and it judges candidates on their front matter, not their names.
;; Two matches is a mis-tagged note, so it refuses rather than picking one.
;;
;; WHERE that climb starts is `le::project-root-here' -- the current tab's `w'
;; workspace before the buffer, since inside a workspace the workspace is the
;; authority for which project is meant.  So `find-journal' is a LOCATOR; the
;; set-frontmatter commands are not, acting on files the caller named or
;; marked in dired.
;;
;; The pure core (collapse / tags / added / strip-project / journal title +
;; match) is separated from the git + denote shell so the naming rules are
;; testable in isolation.

;;; Code:

(require 'denote)
(require 'seq)
(require 'le-project)

(declare-function dired-get-marked-files "dired")

;;; Pure core --------------------------------------------------------------

(defun le::denote-tasks--collapse (s)
  "Collapse S to a denote-atomic keyword body: downcase, keep only [a-z0-9].
Denote's own sluggifier collapses hyphens, underscores and spaces the same
way, so a keyword built here matches what denote would write to the filename."
  (replace-regexp-in-string "[^a-z0-9]" "" (downcase s)))

(defun le::denote-tasks--strip-project (base project)
  "Strip a leading PROJECT prefix (plus separators) from worktree dir BASE.
Return BASE unchanged if it does not start with PROJECT or would strip to
empty, so the worktree tag never repeats the project name (wt-2, not
goodbear-wt-2 -> goodbearwt2)."
  (if (string-prefix-p project base t)
      (let ((rest (replace-regexp-in-string
                   "\\`[^a-z0-9]+" "" (substring base (length project)))))
        (if (string-empty-p rest) base rest))
    base))

(defun le::denote-tasks--tags (project worktree)
  "Desired filetags for raw PROJECT name and optional WORKTREE label.
Each is the prefix `project'/`worktree' fused to the collapsed name."
  (append (list (concat "project" (le::denote-tasks--collapse project)))
          (when worktree
            (list (concat "worktree" (le::denote-tasks--collapse worktree))))))

(defun le::denote-tasks--added (current desired)
  "The DESIRED tags not already in CURRENT (order-stable)."
  (seq-remove (lambda (k) (member k current)) desired))

(defun le::denote-tasks--journal-title (checkout)
  "The title the journal note of CHECKOUT carries: \"journal <root basename>\".
CHECKOUT is the checkout ROOT directory, so a linked worktree -- being its own
directory -- gets a title distinct from the main checkout's, matching the
per-checkout journal identity its filetags already express."
  (concat "journal " (file-name-nondirectory (directory-file-name checkout))))

(defun le::denote-tasks--journal-match-p (title keywords want-title want-tags)
  "Non-nil when a note's TITLE and KEYWORDS identify it as the wanted journal.
TITLE and KEYWORDS are read from the note's FRONT MATTER -- the source of truth
here as everywhere in this file -- never from its file name, which is a
regenerated cache of them.  TITLE must equal WANT-TITLE exactly; KEYWORDS need
only COVER WANT-TAGS, so a journal the user has tagged further still matches."
  (and (equal title want-title)
       (null (le::denote-tasks--added keywords want-tags))))

;;; Imperative shell -------------------------------------------------------

(defun le::denote-tasks--git-lines (checkout &rest args)
  "Run git ARGS in CHECKOUT; return output lines, or nil on failure.
`default-directory' is bound to CHECKOUT, not left as the caller's: it is what
`process-lines' launches the subprocess in, while `-C' only tells git where to
look once it is running.  So the caller's own directory would otherwise decide
whether git starts at all -- a buffer whose directory has since been deleted
cannot host a subprocess, and a remote one would run git on the wrong host with
a local path.  CHECKOUT comes from `le::denote-tasks--checkout', so it exists
and sits on the same host as the file being asked about."
  (let ((default-directory checkout))
    (ignore-errors (apply #'process-lines "git" "-C" checkout args))))

(defun le::denote-tasks--checkout (file)
  "The git checkout holding the `.le-playground' that contains FILE, or nil.
Walk up from FILE to the first ancestor directory that contains a
`.le-playground' entry -- that directory is the enclosing project checkout
whose git identity names the note.  The result is fully expanded, so it can
be handed to `git -C' (which, unlike a shell, does not expand a leading ~)."
  (when-let* ((dir (and file (locate-dominating-file file ".le-playground"))))
    (expand-file-name dir)))

(defun le::denote-tasks--identity (checkout)
  "Return (PROJECT . WORKTREE-or-nil) raw names for git CHECKOUT, or nil.
A single `git worktree list --porcelain' answers both questions the way
`magit-list-worktrees' does under the hood (without pulling magit in): its
first entry is the repository's MAIN worktree, so its basename is PROJECT,
and CHECKOUT is a LINKED worktree exactly when its path differs from that
main path.  For a linked worktree, WORKTREE is CHECKOUT's basename with a
leading copy of PROJECT stripped (so the tag never repeats it); for the main
checkout it is nil.  Returns nil when CHECKOUT is not inside a git working
tree."
  (when-let* ((lines (le::denote-tasks--git-lines
                      checkout "worktree" "list" "--porcelain"))
              (first (car lines))
              (_ (string-prefix-p "worktree " first))
              (main-path (substring first (length "worktree "))))
    (let* ((project (file-name-nondirectory (directory-file-name main-path)))
           (worktree-p (not (file-equal-p checkout main-path)))
           (worktree (when worktree-p
                       (le::denote-tasks--strip-project
                        (file-name-nondirectory (directory-file-name checkout))
                        project))))
      (cons project worktree))))

(defun le::denote-tasks--name-for (file file-type keywords)
  "The denote file name FILE would have with KEYWORDS, other parts kept.
Mirror exactly what `denote-rename-file' builds -- same identifier, title and
signature sources -- so comparing this to FILE answers \"needs rename?\"."
  (denote-format-file-name
   (file-name-directory file)
   (denote-retrieve-filename-identifier file)
   keywords
   (or (denote-retrieve-title-or-filename file file-type) "")
   (concat "." (file-name-extension file))
   (or (denote-retrieve-filename-signature file) "")))

(defun le::denote-tasks--rewrite-keywords (file keywords file-type save)
  "Write KEYWORDS into FILE's `#+filetags:' line, canonically, given FILE-TYPE.
Like `denote-rewrite-keywords', but replace the WHOLE keywords line, not only
its value.  Denote's own rewrite preserves the existing key and whatever
spacing follows it (`denote--rewrite-front-matter-line'), so a source line with
no padding -- a bare `#+filetags:' -- yields `#+filetags::tag:'.  Formatting the
replacement with denote's own front-matter formatter keeps the canonical
padding (one source of truth) and normalizes such lines as it writes.  With
SAVE non-nil, save the buffer.  Assumes the current buffer visits FILE."
  (let* ((front-matter
          (denote--format-front-matter "" (current-time) keywords "" "" file-type))
         (line (denote--retrieve-front-matter-keywords-line-from-content
                front-matter file-type)))
    (with-current-buffer (find-file-noselect file)
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (when (re-search-forward (denote--keywords-key-regexp file-type) nil t 1)
            (delete-region (line-beginning-position) (line-end-position))
            (insert line)
            (when save (save-buffer))))))))

;;;###autoload
(defun le::denote-tasks-checkout-tags (file)
  "Return the project/worktree filetags FILE should carry, or nil.
Resolve FILE's enclosing `.le-playground' checkout and its git identity, then
build the denote-native keywords: a `project<name>' keyword always, plus a
`worktree<label>' keyword when the checkout is a linked git worktree.  This is
a pure query -- it never signals; it returns nil when FILE is not under such a
checkout, so a batch caller can map it over a tree and skip the misses."
  (when-let* ((checkout (le::denote-tasks--checkout file))
              (id (le::denote-tasks--identity checkout)))
    (le::denote-tasks--tags (car id) (cdr id))))

(defun le::denote-tasks--set-frontmatter-1 (file tags no-confirm)
  "Tag FILE with TAGS then rename it to match; NO-CONFIRM as in the caller.
The engine of `le::denote-tasks-set-frontmatter' -- see it for the semantics.
Assumes the current buffer is FILE's own buffer, so the silo's directory-local
settings are the ones denote uses; the caller establishes that context."
  (let ((file-type (denote-filetype-heuristics file)))
    (unless (eq file-type 'org)
      (user-error "%s is not an Org note (only Org notes are supported)"
                  (abbreviate-file-name file)))
    (unless (denote-retrieve-front-matter-keywords-line file file-type)
      (user-error "%s has no denote front matter to treat as authoritative"
                  (abbreviate-file-name file)))
    (let ((tags (or tags (le::denote-tasks-checkout-tags file))))
      (unless tags
        (user-error "No project/worktree tags for %s (under a git .le-playground checkout?)"
                    (abbreviate-file-name file)))
      (let* ((current (denote-retrieve-front-matter-keywords-value file file-type))
             (added (le::denote-tasks--added current tags))
             (denote-rename-confirmations
              (unless no-confirm denote-rename-confirmations))
             (denote-save-buffers (or no-confirm denote-save-buffers))
             ;; Every change is confirmed unless NO-CONFIRM: the front-matter
             ;; write here, the rename below via `denote-rename-confirmations'.
             (write-fm (and added
                            (or no-confirm
                                (yes-or-no-p
                                 (format "Add %s to the front matter of %s? "
                                         (mapconcat #'identity added " ")
                                         (file-name-nondirectory file))))))
             (target (if write-fm (append current added) current)))
        ;; Phase 1 -- front matter is authoritative; write it independently of
        ;; the rename so a declined rename never drops the tags.
        (when write-fm
          (le::denote-tasks--rewrite-keywords file target file-type denote-save-buffers))
        ;; Phase 2 -- sync the file-name cache to the (now-authoritative) front
        ;; matter.  NEW-NAME mirrors exactly what `denote-rename-file' would
        ;; build with these components, so the test is the real "needs rename?".
        (let ((new-name (le::denote-tasks--name-for file file-type target)))
          (cond
           ((not (string= (expand-file-name new-name) (expand-file-name file)))
            (denote-rename-file file 'keep-current target
                                'keep-current 'keep-current 'keep-current))
           (t
            (message "%s; file name already correct"
                     (cond (write-fm (format "Front matter tagged %s"
                                             (mapconcat #'identity added " ")))
                           (added "Left front matter unchanged")
                           (t (format "Already tagged %s"
                                      (mapconcat #'identity tags " ")))))
            file)))))))

;;;###autoload
(defun le::denote-tasks-set-frontmatter (file &optional tags no-confirm)
  "Ensure TAGS are in FILE's front matter, then rename FILE to match.
FILE is a denote note path.  TAGS is the list of keywords to guarantee
present; when nil it defaults to `le::denote-tasks-checkout-tags' for FILE (the
project/worktree tags of the git checkout FILE lives under).

The front matter is the source of truth; the file name is a regenerated cache
of it, of which only the identifier is guaranteed stable.  So this works in
two phases:

1. Set the front matter.  Read FILE's current keywords from its `#+filetags:'
   (NOT the file name, which may be stale), union in any missing TAGS, and
   write them back canonically with `le::denote-tasks--rewrite-keywords' (no
   rename).  This stands on its own: even if the rename below is declined, the
   tags are recorded.
2. Sync the file name.  Compute the name that now reflects the front matter;
   rename FILE to it with `denote-rename-file', or report it already matches.

Unless NO-CONFIRM is non-nil, every change is confirmed with a prompt (the
front-matter write, and the rename via `denote-rename-confirmations'); with
NO-CONFIRM the changes are applied silently and the buffer is saved.

The work runs inside FILE's own buffer, so the silo's `.dir-locals.el' (found
by searching up from FILE, and mode-aware) and `default-directory' are the ones
denote sees -- correct even when called from an unrelated buffer or across
several silos in one batch.  A buffer this opens is killed afterwards unless it
ended up modified, so a file already open (or with unsaved edits) is untouched.

This is a normal function, callable from Lisp: pass FILE (and NO-CONFIRM t) to
tag a file unattended -- e.g. to sweep a tree with
`le::denote-tasks-checkout-tags' picking each file's tags.  Interactively, FILE
is the current buffer's file, TAGS are derived, and a prefix arg sets
NO-CONFIRM.  Return FILE's (possibly new) path.

Only Org notes are supported (the `#+filetags:' the tooling manages are Org
front matter).  Signal a `user-error' when FILE is nil, is not an Org note, has
no denote front matter to treat as authoritative, or TAGS is nil and none can
be derived for it."
  (interactive (list buffer-file-name nil current-prefix-arg))
  (unless file
    (user-error "No file to tag (buffer is not visiting a file?)"))
  (let* ((existing (find-buffer-visiting file))
         (buffer (or existing (find-file-noselect file))))
    (unwind-protect
        (with-current-buffer buffer
          (le::denote-tasks--set-frontmatter-1 file tags no-confirm))
      ;; Only clean up a buffer we opened.  When NO-CONFIRM (unattended), save it
      ;; -- `set-visited-file-name' during the rename marks it modified even
      ;; though its content already matches disk -- then kill it, so a batch
      ;; leaves no buffers behind.  Otherwise (interactive review) kill it only if
      ;; it is clean, leaving anything with real unsaved edits for the user.
      (when (and (not existing) (buffer-live-p buffer))
        (when (and no-confirm (buffer-modified-p buffer))
          (with-current-buffer buffer (save-buffer)))
        (unless (buffer-modified-p buffer)
          (kill-buffer buffer))))))

(defun le::denote-tasks--plan (file &optional tags)
  "Return (:file FILE :tags TAGS :added ADDED) if FILE needs tagging, else nil.
TAGS default to `le::denote-tasks-checkout-tags' for FILE.  Return nil -- so a
batch caller drops it silently -- when FILE is not an Org denote note, has no
front matter, no TAGS can be derived, or FILE already carries TAGS and its name
already matches (i.e. there is nothing to do)."
  (when (and file (file-regular-p file))
    (let ((file-type (denote-filetype-heuristics file)))
      (when (and (eq file-type 'org)
                 (denote-retrieve-front-matter-keywords-line file file-type))
        (setq tags (or tags (le::denote-tasks-checkout-tags file)))
        (when tags
          (let* ((current (denote-retrieve-front-matter-keywords-value file file-type))
                 (added (le::denote-tasks--added current tags))
                 (new-name (le::denote-tasks--name-for
                            file file-type (append current added))))
            (when (or added (not (string= (expand-file-name new-name)
                                          (expand-file-name file))))
              (list :file file :tags tags :added added))))))))

;;;###autoload
(defun le::denote-tasks-dired-set-frontmatter-marked (&optional no-confirm)
  "Tag the marked Dired files with each one's checkout project/worktree filetags.
Operate on the marked files (or the file at point when none are marked).  For
each that is an Org denote note under a git `.le-playground' checkout and
actually needs a change, apply `le::denote-tasks-set-frontmatter' (set the
front matter, then rename to match); files that need nothing, are not Org
notes, or lie outside such a checkout are skipped without a prompt.

Without a prefix arg, confirm each file with the usual y/n/!/q prompt -- `y'
tags it, `n' skips it, `!' tags all remaining, `q' stops.  With a prefix arg
NO-CONFIRM, tag every eligible marked file without prompting.  Each file that
is acted on is applied atomically (its per-file answer is the confirmation), so
the front-matter and rename sub-prompts are suppressed."
  (interactive "P")
  (let ((plans (delq nil (mapcar #'le::denote-tasks--plan (dired-get-marked-files))))
        ;; The per-file answer is the confirmation; apply each atomically.
        (denote-rename-confirmations nil)
        (denote-save-buffers t)
        (done 0))
    (if (null plans)
        (message "No marked files need project/worktree tagging")
      (let ((act (lambda (p)
                   (le::denote-tasks-set-frontmatter (plist-get p :file) (plist-get p :tags) t)
                   (setq done (1+ done)))))
        (if no-confirm
            (mapc act plans)
          (map-y-or-n-p
           (lambda (p)
             (let ((added (plist-get p :added)))
               (format "Tag %s (%s)? "
                       (file-name-nondirectory (plist-get p :file))
                       (if added
                           (concat "add " (mapconcat #'identity added " "))
                         "rename to match front matter"))))
           act plans '("file" "files" "tag"))))
      (message "Tagged %d of %d eligible file(s)" done (length plans)))))

(defun le::denote-tasks--journal-dir (checkout)
  "The absolute `journal/' directory of CHECKOUT's playground denote silo."
  (expand-file-name ".le-playground/denote/journal/" checkout))

(defun le::denote-tasks--journal-matches (dir want-title want-tags)
  "The Org notes directly in DIR whose front matter matches WANT-TITLE/WANT-TAGS.
Non-recursive, and nil when DIR does not exist -- a checkout whose silo has no
`journal/' yet simply has no journal."
  (when (file-directory-p dir)
    (seq-filter
     (lambda (file)
       (and (file-regular-p file)
            (eq (denote-filetype-heuristics file) 'org)
            (le::denote-tasks--journal-match-p
             (denote-retrieve-front-matter-title-value file 'org)
             (denote-retrieve-front-matter-keywords-value file 'org)
             want-title want-tags)))
     (directory-files dir t "\\.org\\'"))))

(defun le::denote-tasks--journal-create (dir title tags)
  "Create the journal note TITLE carrying TAGS in DIR; return its path.
DIR is created first because `denote' refuses to create its own directory, and
`denote-directory' is bound to the silo root so DIR reads as a subdirectory of
it -- denote falls back to the ambient directory otherwise, landing the note in
the wrong silo -- which also scopes the identifier's uniqueness scan to this
silo.

`denote-save-buffers' is forced on: by default `denote' leaves the new note in
an unsaved buffer, so nothing reaches disk and the next lookup -- which reads
front matter from FILES -- would find no journal and mint a second one."
  (make-directory dir t)
  (let ((denote-directory (file-name-directory (directory-file-name dir)))
        (denote-save-buffers t))
    (denote title tags 'org dir)))

(defun le::denote-tasks-resolve-journal (origin)
  "Return the journal note path for ORIGIN's checkout, creating it if missing.
The resolving half of `le::denote-tasks-find-journal' -- the whole of it but the
final `find-file' -- so a non-interactive caller like `dnc' can borrow the same
per-checkout identity rule by passing the silo it is operating on as ORIGIN.
See `le::denote-tasks-find-journal' for the identity and the `user-error's on a
missing or untaggable checkout and on a duplicate journal."
  (let* ((checkout
          (or (le::denote-tasks--checkout origin)
              (user-error "No `.le-playground' above %s (run /playground-setup)"
                          (abbreviate-file-name origin))))
         (tags (or (le::denote-tasks-checkout-tags origin)
                   (user-error "No project/worktree tags for %s (a git checkout?)"
                               (abbreviate-file-name checkout))))
         (title (le::denote-tasks--journal-title checkout))
         (dir (le::denote-tasks--journal-dir checkout))
         (matches (le::denote-tasks--journal-matches dir title tags)))
    (when (cdr matches)
      (user-error "%d notes claim to be \"%s\" -- fix their front matter: %s"
                  (length matches) title
                  (mapconcat #'file-name-nondirectory matches ", ")))
    (or (car matches)
        (le::denote-tasks--journal-create dir title tags))))

;;;###autoload
(defun le::denote-tasks-find-journal ()
  "Open the journal note of the project this Emacs is working in.
Create it first when that project has none, then open it either way.

WHICH project comes from `le::project-root-here', so the answer follows the TAB
rather than the point: with the tab on one project and a file from another
visited in it, this journals to the TAB's project.

Identity is per CHECKOUT, not per project name: a linked worktree keeps its own
journal.  So the note wanted here is the one whose title is `journal ' plus the
basename of the checkout root and whose keywords cover
`le::denote-tasks-checkout-tags' (`project<name>' in a main checkout, plus
`worktree<label>' in a linked one).

From that origin the checkout is found by climbing with
`le::denote-tasks--checkout' rather than by consulting `denote-directory': this
is meant to fire from a source buffer, where the silo's `.dir-locals.el' -- and
so its `denote-directory' -- never applies.  The climb stays even with a project
root for an origin, being what establishes the project HAS a playground.
Candidates in the silo's `journal/' are then judged on their front matter, not
their file names.

Signal a `user-error' when there is no `.le-playground' above the origin
\(creating the silo is a git-worktree operation -- see /playground-setup), when
no project tags can be derived for it, or when two or more notes match: a
duplicate means a mis-tagged note, and silently picking one would hide it."
  (interactive)
  ;; Buffer tail: the helper is nil with neither workspace nor project, and a
  ;; `.le-playground' can sit outside a VC repo.
  (find-file
   (le::denote-tasks-resolve-journal
    (or (le::project-root-here) buffer-file-name default-directory))))

(provide 'le-denote-tasks)
;;; le-denote-tasks.el ends here
