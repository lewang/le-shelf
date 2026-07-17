;;; le-project.el --- Merge a repo and its .le-playground worktree for find-file  -*- lexical-binding: t; -*-

;;; Commentary:

;; `project-find-file' normally sees one repo at a time.  A project may keep
;; a nested `.le-playground/' git worktree (a separate repo, globally
;; gitignored) alongside its source, and there's no built-in way to complete
;; across both.  This defines a lightweight `le-playground' project type that
;; unions a repo and its playground for file-finding ONLY: `project-current'
;; stays native, so grep/compile/vc-dir keep each repo's own identity.
;;
;; `le::project-find-file-both' builds the merged project on demand, so only
;; that command sees the union.  Wire it up with a `[remap project-find-file]'
;; binding (see post-init.el).

;;; Code:

(require 'cl-lib)
(require 'project)

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

;;;###autoload
(defun le::project-find-file-both (&optional include-all)
  "`project-find-file' across a repo and its `.le-playground' worktree.
Builds the merged project on demand, so only this command sees the union.
With INCLUDE-ALL (a prefix argument) this degrades to the parent root only,
which is unsupported by design -- the playground is not included."
  (interactive "P")
  (let* ((pr (or (le::project-try-playground-super default-directory)
                 (project-current t)))
         (project-files-relative-names nil))
    (project-find-file-in
     (delq nil (list (and buffer-file-name
                          (project--find-default-from buffer-file-name pr))
                     (thing-at-point 'filename)))
     (list (project-root pr)) pr include-all)))

(provide 'le-project)
;;; le-project.el ends here
