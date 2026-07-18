;;; le-project-flip.el --- Cycle buffers of the current w workspace's project  -*- lexical-binding: t; -*-

;;; Commentary:

;; `le::project-flip' cycles buffers (via buffer-flip) restricted to the
;; current `w' workspace's project: it `let'-binds `buffer-flip-skip-patterns'
;; with a predicate that skips any buffer whose `default-directory' is outside
;; the workspace's `:project-root', then hands off to `buffer-flip-forward',
;; which captures the binding for the whole cycling session (see buffer-flip).
;;
;; This file is dedicated to the buffer-flip entrance, so it hard-`require's
;; buffer-flip up front -- le-shelf's `Package-Requires' names buffer-flip, so
;; the dependency is real and build-ordered.  `w' is treated as ambient (always
;; loaded, like everywhere else in le-shelf) and only `declare-function'd.

;;; Code:

(require 'buffer-flip-buffers)

(declare-function w-current "w" ())

;;;###autoload
(defun le::project-flip ()
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
  (if-let* ((root (plist-get (w-current) :project-root)))
      (let* ((root (expand-file-name root))
             (skip (lambda (buf)
                     (let ((dir (buffer-local-value 'default-directory buf)))
                       (not (and dir (file-in-directory-p dir root))))))
             (buffer-flip-skip-patterns (cons skip buffer-flip-skip-patterns)))
        (buffer-flip-forward))
    (buffer-flip-forward)))

(provide 'le-project-flip)
;;; le-project-flip.el ends here
