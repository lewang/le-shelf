;;; le-project-flip.el --- Cycle buffers of the current w workspace's project  -*- lexical-binding: t; -*-

;;; Commentary:

;; `le::project-flip' / `le::project-flip-backward' cycle buffers (via
;; buffer-flip) restricted to the current `w' workspace's project: they
;; `let'-bind `buffer-flip-skip-patterns' with a predicate that skips any
;; buffer whose `default-directory' is outside the workspace's `:project-root',
;; then hand off to `buffer-flip-forward' / `buffer-flip-backward', which
;; capture the binding for the whole cycling session (see buffer-flip).  Both
;; share `le::project-flip--run'.
;;
;; This file is dedicated to the buffer-flip entrance, so it hard-`require's
;; buffer-flip up front -- le-shelf's `Package-Requires' names buffer-flip, so
;; the dependency is real and build-ordered.  `w' is treated as ambient (always
;; loaded, like everywhere else in le-shelf) and only `declare-function'd.

;;; Code:

(require 'buffer-flip-buffers)

(declare-function w-current "w" ())

(defun le::project-flip--run (flip-fn)
  "Cycle buffers scoped to the current `w' workspace project, via FLIP-FN.
FLIP-FN is `buffer-flip-forward' or `buffer-flip-backward'.  Resolve the
workspace's `:project-root' and cycle only buffers whose `default-directory'
is at or under it -- so the project's file buffers, dired, magit, shells, etc.
are included and everything else is skipped (the nested `.le-playground'
worktree is covered by physical nesting under the root).

Works by `let'-binding `buffer-flip-skip-patterns' with the skip predicate
consed onto the front, then calling FLIP-FN, which captures the binding for the
whole cycling session (see buffer-flip).  With no workspace project root, fall
back to an unrestricted flip (both directions)."
  (if-let* ((root (plist-get (w-current) :project-root)))
      (let* ((root (expand-file-name root))
             (skip (lambda (buf)
                     (let ((dir (buffer-local-value 'default-directory buf)))
                       (not (and dir (file-in-directory-p dir root))))))
             (buffer-flip-skip-patterns (cons skip buffer-flip-skip-patterns)))
        (funcall flip-fn))
    (funcall flip-fn)))

;;;###autoload
(defun le::project-flip ()
  "Flip forward through buffers of the current `w' workspace's project.
Cold-start entrance; see `le::project-flip--run' for scoping details."
  (interactive)
  (le::project-flip--run #'buffer-flip-forward))

;;;###autoload
(defun le::project-flip-backward ()
  "Flip backward through buffers of the current `w' workspace's project.
Cold-start entrance; see `le::project-flip--run' for scoping details."
  (interactive)
  (le::project-flip--run #'buffer-flip-backward))

(provide 'le-project-flip)
;;; le-project-flip.el ends here
