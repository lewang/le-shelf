;;; le-project.el --- Merge a repo and its .le-playground worktree for listing  -*- lexical-binding: t; -*-

;;; Commentary:

;; `project-find-file' sees one repo at a time.  A project may keep a nested
;; `.le-playground/' git worktree (a separate repo, globally gitignored)
;; alongside its source, and there's no built-in way to list across both.
;; This defines a lightweight `le-playground' project type that unions a repo
;; and its playground for file LISTING ONLY: `project-current' stays native,
;; so grep/compile/vc-dir keep each repo's own identity.
;;
;; `le::project-current' resolves the merged super-project on demand;
;; `le::project-find-file' builds on it, passing the merged project to
;; `project-find-file-in' (driving the `project-files' union below).  Only it
;; sees the union; wire it up in post-init.el via `[remap project-find-file]'.
;;
;; The consult side of the same idea -- making consult's Project Buffer/File
;; sources span the superset -- lives in le-consult.el, which reuses
;; `le::project-current' as its climbing resolver.
;;
;; The buffer-flip entrance in the same "project" spirit -- cycling only the
;; current `w' workspace's buffers -- lives in le-project-flip.el
;; (`le::project-flip'), which pulls in buffer-flip.
;;
;; `le::project-root-here' answers a different question -- not "what is this
;; file's project?" but "which project does the user MEAN?" -- asking the `w'
;; workspace before the buffer.  Commands using ambient location as a LOCATOR
;; want it; ones acting ON the current buffer or file do not.
;;
;; The predicates below state the membership rule -- "the `.le-playground'
;; worktree belongs to its parent repo" -- once, for the callers that each need
;; it in a different shape: `w's buffer adoption knob, claude-code-ide's parent
;; session knob, and le-ide's file-belongs test.  They split by ARITY rather
;; than one function bending into every role; the identity-vs-containment
;; difference between the last two is load-bearing, not incidental.

;;; Code:

(require 'cl-lib)
(require 'project)
(require 'w)

;;;; The .le-playground membership rule

(defconst le::project-playground-name ".le-playground"
  "Name of the playground worktree a project may keep beside its source.")

(defun le::project-playground-dir (root)
  "Return ROOT's playground worktree path, slash-terminated.
Names the path; does not check that it exists.  The one place
`le::project-playground-name' is joined to a root."
  (file-name-as-directory
   (expand-file-name le::project-playground-name root)))

;;;###autoload
(defun le::project-playground-p (dir)
  "Return non-nil when DIR is a playground worktree.
Tests DIR's name and that it carries its own `.git', which is what makes it
a separate repo rather than a plain subdirectory.

Deliberately does NOT verify that DIR's `project-root' is DIR itself -- that
stronger check belongs to `le::project-try-playground-super', which is
building a project object and already paying for `project-try-vc'.  This one
runs per buffer in `w's pruning path, so it stays cheap."
  (let ((dir (directory-file-name (expand-file-name dir))))
    (and (string= le::project-playground-name (file-name-nondirectory dir))
         (file-exists-p (expand-file-name ".git" dir)))))

(defun le::project-playground-of-p (dir root)
  "Return non-nil when DIR is ROOT, or is ROOT's playground worktree.
IDENTITY, not containment: DIR is a resolved project root, so a nested CHILD
project sitting under ROOT must NOT count -- it is its own project, and
containment would wrongly admit it.  For the containment reading a buffer's
arbitrary `default-directory' needs, see
`le::project-adopt-playground-buffer-p'."
  (and dir root
       (let ((dir (directory-file-name (expand-file-name dir))))
         (or (file-equal-p dir root)
             (and (le::project-playground-p dir)
                  (file-equal-p (file-name-directory dir) root))))))

;;;###autoload
(defun le::project-adopt-playground-buffer-p (buf root)
  "Return non-nil when BUF sits inside ROOT's playground worktree.
CONTAINMENT, not identity: BUF's `default-directory' can be any level deep
inside the playground, so the test is a prefix one -- which also means a repo
nested INSIDE the playground is adopted.  That is today's behaviour and is
kept deliberately; deciding it by BUF's own project root would be defensible
and would change it."
  (when-let* ((dir (buffer-local-value 'default-directory buf)))
    (string-prefix-p (le::project-playground-dir root)
                     (expand-file-name dir))))

;;;; The merged repo + playground project type

(cl-defmethod project-root ((project (head le-playground)))
  "Parent repo root of the le-playground PROJECT.
Everything nests under it, so completion shows clean paths with no \"../\"."
  (project-root (nth 1 project)))

(cl-defmethod project-files ((project (head le-playground)) &optional _dirs)
  "Union of the parent repo's and the playground worktree's files.
Return absolute names (bind `project-files-relative-names' to nil): relative
names from two roots would collide, but absolutes combine correctly and
`project--read-file-name' still displays them relative to their common
parent."
  (let ((project-files-relative-names nil))
    (append (project-files (nth 1 project))
            (project-files (nth 2 project)))))

(defun le::project-try-playground-super (dir)
  "Span a repo and its nested `.le-playground' worktree as one project.
Fires from either side: when DIR is inside a `.le-playground/' worktree, or
when DIR is inside a repo that HAS a `.le-playground/' worktree at its root.
Return a `le-playground' project unioning both, or nil elsewhere (defer to
`project-try-vc')."
  (when-let* ((dir (expand-file-name dir))
              (parent
               (if (string-match "\\`\\(.+?\\)/\\.le-playground\\(?:/\\|\\'\\)" dir)
                   (match-string 1 dir)                   ; inside the worktree
                 (when-let* ((proj (project-try-vc dir))) ; inside the parent repo
                   (directory-file-name (project-root proj)))))
              (play-dir (le::project-playground-dir parent))
              ((le::project-playground-p play-dir))
              (parent-proj (project-try-vc parent))
              (play-proj (project-try-vc play-dir))
              ;; Confirm .le-playground is really its own repo (its root is
              ;; itself), not a plain subdir that climbed back up to PARENT.
              ((file-equal-p (project-root play-proj) play-dir)))
    (list 'le-playground parent-proj play-proj)))

(defun le::project-current (&optional maybe-prompt dir)
  "Like `project-current', but span a repo and its `.le-playground' worktree.
Return the merged `le-playground' super-project when DIR (default
`default-directory') sits in such a context, else defer to `project-current'
with MAYBE-PROMPT.  Shared by `le::project-find-file' and le-consult.el's
climbing sources; each supplies its own glue around the result."
  (or (le::project-try-playground-super (or dir default-directory))
      (project-current maybe-prompt dir)))

(defun le::project-root-here (&optional maybe-prompt dir)
  "The authoritative project root for the current context, or nil.
The current tab's `w' workspace wins when there is one: it names the project
being worked in, which the buffer may not -- a `*scratch*', an eshell that has
cd'd away, or a directory deleted underneath a live buffer all answer wrongly.
Otherwise fall back to `le::project-current', which unlike native
`project-current' spans a `.le-playground' worktree with its parent repo,
resolving from DIR and prompting per MAYBE-PROMPT.

Normalized, because `project-root' may answer with a `~'-abbreviated path."
  (when-let* ((root (or (plist-get (w-current) :project-root)
                        (when-let* ((pr (le::project-current maybe-prompt dir)))
                          (project-root pr)))))
    (file-name-as-directory (expand-file-name root))))

;;;###autoload
(defun le::project-find-file (&optional include-all)
  "`project-find-file' across a repo and its `.le-playground' worktree.
Builds the merged project on demand, so only this command sees the union.
With INCLUDE-ALL (a prefix argument) this degrades to the parent root only,
which is unsupported by design -- the playground is not included."
  (interactive "P")
  (let* ((pr (le::project-current t))
         (project-files-relative-names nil))
    (project-find-file-in
     (delq nil (list (and buffer-file-name
                          (project--find-default-from buffer-file-name pr))
                     (thing-at-point 'filename)))
     (list (project-root pr)) pr include-all)))

(provide 'le-project)
;;; le-project.el ends here
