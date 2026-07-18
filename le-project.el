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
;; `le::project-buffer-flip' is a separate entrance in the same "project"
;; spirit: it cycles buffers (via buffer-flip) restricted to the current `w'
;; workspace's project, by `let'-binding `buffer-flip-skip-patterns'.

;;; Code:

(require 'cl-lib)
(require 'project)

;; `le::project-buffer-flip' leans on the `w' workspace and buffer-flip; keep
;; both soft so this file loads without them.
(declare-function w-current "w" ())
(declare-function buffer-flip-forward "buffer-flip-buffers")
(defvar buffer-flip-skip-patterns)

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
              (play-dir (expand-file-name ".le-playground" parent))
              ((file-exists-p (expand-file-name ".git" play-dir)))
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

;;;###autoload
(defun le::project-buffer-flip ()
  "Flip through buffers of the current `w' workspace's project.
Resolve the workspace's `:project-root' and cycle (via buffer-flip) only
buffers whose `default-directory' is at or under it -- so the project's file
buffers, dired, magit, shells, etc. are included and everything else is
skipped.  The nested `.le-playground' worktree is covered by physical
nesting under the root.

Works by `let'-binding `buffer-flip-skip-patterns' with a skip predicate
consed onto the front, then handing off to `buffer-flip-forward', which
captures the binding for the whole cycling session (see buffer-flip).  With
no workspace project root, fall back to an unrestricted flip."
  (interactive)
  (require 'buffer-flip-buffers)
  (if-let* ((root (plist-get (w-current) :project-root)))
      (let* ((root (expand-file-name root))
             (skip (lambda (buf)
                     (let ((dir (buffer-local-value 'default-directory buf)))
                       (not (and dir (file-in-directory-p dir root))))))
             (buffer-flip-skip-patterns (cons skip buffer-flip-skip-patterns)))
        (buffer-flip-forward))
    (buffer-flip-forward)))

(provide 'le-project)
;;; le-project.el ends here
