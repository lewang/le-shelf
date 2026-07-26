;;; le-denote-tasks.el --- Project/worktree filetags for denote task notes  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tag a denote note with the identity of the git checkout it lives under, so a
;; journal or task entry records which project (and which worktree of it) it
;; belongs to.  The tags are denote-native atomic keywords -- a `project'/`worktree'
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
;; `le::denote-tasks-tag-file' is FRONT MATTER FIRST: the `#+filetags:' is the
;; source of truth, the file name a regenerated cache of which only the
;; identifier is guaranteed stable.  So it (1) unions the tags into the note's
;; current front-matter keywords and writes them back with
;; `denote-rewrite-keywords' -- no rename, standing on its own -- then (2)
;; renames the file to match that front matter (or reports the name is already
;; correct).  Every change is confirmed with a prompt unless NO-CONFIRM, in
;; which case all changes apply silently and the buffer is saved.
;;
;; It is a normal function, not just a command: FILE, TAGS and NO-CONFIRM are
;; arguments (the interactive spec fills them from the buffer, a nil TAGS to
;; derive, and the prefix arg), so Lisp can tag any file unattended --
;; e.g. sweep a tree, using the pure query `le::denote-tasks-checkout-tags' to
;; pick each file's project/worktree tags.
;;
;; The pure core (collapse / tags / added / strip-project) is separated from the
;; git + denote shell so the naming rules are testable in isolation.

;;; Code:

(require 'denote)
(require 'seq)

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

;;; Imperative shell -------------------------------------------------------

(defun le::denote-tasks--git-lines (checkout &rest args)
  "Run git ARGS in CHECKOUT; return output lines, or nil on failure."
  (ignore-errors (apply #'process-lines "git" "-C" checkout args)))

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

;;;###autoload
(defun le::denote-tasks-tag-file (file &optional tags no-confirm)
  "Ensure TAGS are in FILE's front matter, then rename FILE to match.
FILE is a denote note path.  TAGS is the list of keywords to guarantee
present; when nil it defaults to `le::denote-tasks-checkout-tags' for FILE (the
project/worktree tags of the git checkout FILE lives under).

The front matter is the source of truth; the file name is a regenerated cache
of it, of which only the identifier is guaranteed stable.  So this works in
two phases:

1. Set the front matter.  Read FILE's current keywords from its `#+filetags:'
   (NOT the file name, which may be stale), union in any missing TAGS, and
   write them back with `denote-rewrite-keywords' (no rename).  This stands on
   its own: even if the rename below is declined, the tags are recorded.
2. Sync the file name.  Compute the name that now reflects the front matter;
   rename FILE to it with `denote-rename-file', or report it already matches.

Unless NO-CONFIRM is non-nil, every change is confirmed with a prompt (the
front-matter write, and the rename via `denote-rename-confirmations'); with
NO-CONFIRM the changes are applied silently and the buffer is saved.

This is a normal function, callable from Lisp: pass FILE (and NO-CONFIRM t) to
tag a file unattended -- e.g. to sweep a tree with
`le::denote-tasks-checkout-tags' picking each file's tags.  Interactively, FILE
is the current buffer's file, TAGS are derived, and a prefix arg sets
NO-CONFIRM.  Return FILE's (possibly new) path.

Signal a `user-error' when FILE is nil, has no denote front matter to treat as
authoritative, or TAGS is nil and none can be derived for it."
  (interactive (list buffer-file-name nil current-prefix-arg))
  (unless file
    (user-error "No file to tag (buffer is not visiting a file?)"))
  (let ((file-type (denote-filetype-heuristics file)))
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
          (denote-rewrite-keywords file target file-type denote-save-buffers))
        ;; Phase 2 -- sync the file-name cache to the (now-authoritative) front
        ;; matter.  NEW-NAME mirrors exactly what `denote-rename-file' would
        ;; build with these components, so the test is the real "needs rename?".
        (let ((new-name (denote-format-file-name
                         (file-name-directory file)
                         (denote-retrieve-filename-identifier file)
                         target
                         (or (denote-retrieve-title-or-filename file file-type) "")
                         (concat "." (file-name-extension file))
                         (or (denote-retrieve-filename-signature file) ""))))
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

(provide 'le-denote-tasks)
;;; le-denote-tasks.el ends here
