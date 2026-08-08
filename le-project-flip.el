;;; le-project-flip.el --- Cycle buffers of the current w workspace's project  -*- lexical-binding: t; -*-

;;; Commentary:

;; `le::project-flip' / `le::project-flip-backward' cycle buffers (via
;; buffer-flip) restricted to the current `w' workspace's project: they
;; `let'-bind `buffer-flip-skip-patterns' with a predicate that skips whatever
;; would not DISPLAY in the pane being flipped, then hand off to
;; `buffer-flip-forward' / `buffer-flip-backward', which capture the binding
;; for the whole cycling session (see buffer-flip).  Both share
;; `le::project-flip--run'.
;;
;; The scoping decision itself is pure -- `le::project-flip--keep-p' maps a
;; pane role and a project root to a predicate -- so it is tested directly,
;; and `--run' stays a thin caller that negates it.
;;
;; This file is dedicated to the buffer-flip entrance, so it hard-`require's
;; buffer-flip up front -- le-shelf's `Package-Requires' names buffer-flip, so
;; the dependency is real and build-ordered.  `w' is treated as ambient (always
;; loaded, like everywhere else in le-shelf) and only `declare-function'd.

;;; Code:

(require 'buffer-flip-buffers)
(require 'w)

(declare-function le::ide-compatible-layout "le-ide" ())
(declare-function le::frame-top-left-buffer-p "le-frame" (buf &optional act))
(declare-function le::frame-bottom-left-buffer-p "le-frame" (buf &optional act))
(declare-function le::frame-cc-buffer-p "le-frame" (buf &optional act))

(defun le::project-flip--pane-role ()
  "Return the IDE pane the selected window occupies, or nil.
One of `cc', `top-left' or `bottom-left' when the frame has the recognized
3-pane layout (`le::ide-compatible-layout'); nil outside it, and nil for a
window the layout does not account for."
  (when-let* ((layout (le::ide-compatible-layout)))
    (let ((win (selected-window)))
      (cond
       ((eq win (plist-get layout :cc-win))      'cc)
       ((eq win (plist-get layout :top-left))    'top-left)
       ((eq win (plist-get layout :bottom-left)) 'bottom-left)))))

(defun le::project-flip--keep-p (role root)
  "Return the predicate deciding which buffers a flip may surface.
ROLE is a pane from `le::project-flip--pane-role' and ROOT the workspace's
project root.  Each pane cycles what would DISPLAY there, reusing the same
routing predicates `display-buffer-alist' fills it with: top-left drops
popups and magit-status, bottom-left keeps only those, and the cc pane keeps
the project's Claude terminals.  With no ROLE, workspace membership alone.

The cc pane's predicate stands ALONE; the others compose with
`w-buffer-in-project-p'.  That predicate reads `default-directory', which in
a terminal buffer tracks the shell's cwd and so drifts once Claude changes
directory -- composing it in would drop instances that
`le::frame-cc-buffer-p' has already scoped to the project by session record."
  (if (eq role 'cc)
      #'le::frame-cc-buffer-p
    (let ((pane (pcase role
                  ('top-left    #'le::frame-top-left-buffer-p)
                  ('bottom-left #'le::frame-bottom-left-buffer-p))))
      (lambda (buf)
        (and (w-buffer-in-project-p buf root)
             (or (null pane) (funcall pane buf)))))))

(defun le::project-flip--run (flip-fn)
  "Cycle buffers scoped to the current `w' workspace project, via FLIP-FN.
FLIP-FN is `buffer-flip-forward' or `buffer-flip-backward'.  Resolve the
workspace's `:project-root' and cycle only what
`le::project-flip--keep-p' keeps for the selected pane -- by default the
project's own file buffers, dired, magit, shells and any adopted
`.le-playground' scratchpad, while a nested CHILD project's buffers are
skipped (they belong to their own workspace).

Works by `let'-binding `buffer-flip-skip-patterns' with the skip predicate
consed onto the front, then calling FLIP-FN, which captures the binding for the
whole cycling session (see buffer-flip).  With no workspace project root, fall
back to an unrestricted flip (both directions)."
  (if-let* ((root (plist-get (w-current) :project-root)))
      (let* ((keep-p (le::project-flip--keep-p (le::project-flip--pane-role) root))
             (skip (lambda (buf) (not (funcall keep-p buf))))
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
