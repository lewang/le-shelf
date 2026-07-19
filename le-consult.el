;;; le-consult.el --- Span a repo and its .le-playground worktree in consult  -*- lexical-binding: t; -*-

;;; Commentary:

;; `consult-buffer' (C-x b, narrow `p') and `consult-project-buffer'
;; (C-x p b) build their "Project Buffer" / "Project File" lists from a
;; single project root via `consult-project-function'.  From inside a
;; `.le-playground/' worktree those sources miss the parent repo's entries
;; (see le-project.el for the merged `le-playground' super-project).
;;
;; The fix lives at the SOURCE level, not the command level: rebuild
;; climbing copies of consult's project buffer/file sources, each running its
;; `:items'/`:new' with `consult-project-function' dynamically bound to a
;; resolver that climbs to the merged super-project, and splice them into the
;; source lists.  Then both commands inherit the union.
;;
;; The global `consult-project-function' is left NATIVE on purpose.  Its only
;; reach beyond these listing sources is consult-grep/ripgrep/find's default
;; root; flipping it globally would jump that root to the parent repo from
;; inside `.le-playground' -- and since `.le-playground' is globally
;; gitignored, ripgrep would then skip scratchpad files.  So we climb
;; per-source only.
;;
;; Install by calling `le::consult-project-superset-setup' once from consult's
;; `use-package' `:config' (after any `consult-customize').  It is autoloaded,
;; so no `require' is needed in init.

;;; Code:

(require 'cl-lib)
(require 'le-project)

(declare-function w-buffer-in-project-p "w" (buf root))

;; Resolved lazily inside `le::consult-project-superset-setup' (autoloaded), so
;; declare consult's specials rather than requiring at file load.
(defvar consult-project-function)
(defvar consult-buffer-sources)
(defvar consult-project-buffer-sources)
(defvar consult-source-project-buffer)
(defvar consult-source-project-recent-file)
(defvar consult-source-project-buffer-hidden)
(defvar consult-source-project-recent-file-hidden)

;; Climbing copies of consult's project sources, rebuilt by the setup fn from
;; consult's own source vars (so they inherit any `consult-customize').
(defvar le::consult-source-project-buffer nil
  "Climbing copy of `consult-source-project-buffer'.")
(defvar le::consult-source-project-recent-file nil
  "Climbing copy of `consult-source-project-recent-file'.")
(defvar le::consult-source-project-buffer-hidden nil
  "Climbing copy of `consult-source-project-buffer-hidden'.")
(defvar le::consult-source-project-recent-file-hidden nil
  "Climbing copy of `consult-source-project-recent-file-hidden'.")

(defun le::consult--project-root (&optional maybe-prompt)
  "Resolve the merged super-project root, `consult-project-function'-shaped.
Climb to a repo and its `.le-playground' worktree via `le::project-current'
and return the parent root, or nil when there is no project."
  (when-let* ((pr (le::project-current maybe-prompt)))
    (project-root pr)))

(defun le::consult--prune-buffers (items-fn)
  "Wrap a buffer source's ITEMS-FN to drop nested child-project buffers.
Each consult buffer candidate is a (NAME . BUFFER) pair (see
`consult--buffer-pair'); keep it only when BUFFER belongs to the merged
super-project root per `w-buffer-in-project-p' -- so a nested CHILD
project's buffers (which live under the root on disk but resolve to the
child's own project) are pruned from the parent's Project Buffer list,
while an adopted `.le-playground' buffer is kept.  With no super-project
root, or for a candidate that is not a live buffer, the entry passes
through."
  (lambda (&rest args)
    (let ((result (apply items-fn args))
          (root (le::consult--project-root)))
      (if (not root)
          result
        (seq-filter
         (lambda (item)
           (let ((buf (if (consp item) (cdr item) (get-buffer item))))
             (or (not (bufferp buf))
                 (w-buffer-in-project-p buf root))))
         result)))))

(defun le::consult--prune-files (items-fn)
  "Wrap a file source's ITEMS-FN to drop nested child-project files.
Each consult file candidate is a (DISPLAY . ABSOLUTE-FILE) pair (see
`consult-source-project-recent-file', whose `:items' returns root-relative
display strings consed onto the expanded absolute name).  Build the merged
super-project's `project-files' set once and keep a candidate only when its
file is a member -- so a nested CHILD project's files (physically under the
root but absent from the git-tracked union) are pruned from the parent's
Project File list, while parent-repo and adopted `.le-playground' files pass.
With no super-project, or an empty file set, the items pass through unchanged.

Order is preserved (`seq-filter'), so consult's recentf MRU ordering and its
native exclusion of already-open files (via `consult--buffer-file-hash') both
survive -- this only removes the child's entries."
  (lambda (&rest args)
    (let ((result (apply items-fn args))
          (pr (le::project-current)))
      (if (not pr)
          result
        (let ((members (make-hash-table :test #'equal)))
          (dolist (f (project-files pr))
            (puthash (expand-file-name f) t members))
          (if (zerop (hash-table-count members))
              result
            (seq-filter
             (lambda (item)
               (let ((file (if (consp item) (cdr item) item)))
                 (or (not (stringp file))
                     (gethash (expand-file-name file) members))))
             result)))))))

(defun le::consult--climb-source (source)
  "Return a copy of consult SOURCE whose `:items'/`:new' climb the super-project.
Wrap the source's `:items' (and `:new', when present) so each runs with
`consult-project-function' dynamically bound to `le::consult--project-root';
every other property is inherited unchanged.  The global
`consult-project-function' is untouched, so grep/ripgrep/find keep the native
per-repo root.

Both buffer and file sources are also pruned of nested child-project entries:
buffer sources via `le::consult--prune-buffers' (per-buffer `project-current'),
file sources via `le::consult--prune-files' (membership in the super-project's
git-tracked `project-files').  So a nested child workspace's buffers/files do
not appear in the parent's Project Buffer/File lists, while parent-repo and
adopted `.le-playground' entries pass."
  (let ((items (plist-get source :items))
        (new (plist-get source :new))
        (bufferp (eq (plist-get source :category) 'buffer))
        (filep (eq (plist-get source :category) 'file)))
    (cl-flet ((climb (fn)
                (lambda (&rest args)
                  (let ((consult-project-function #'le::consult--project-root))
                    (apply fn args)))))
      ;; Prepended `:items'/`:new' win over the spliced originals (plist-get
      ;; returns the first match); every other property comes from SOURCE.
      `(:items ,(cond (bufferp (le::consult--prune-buffers (climb items)))
                      (filep (le::consult--prune-files (climb items)))
                      (t (climb items)))
        ,@(when new (list :new (climb new)))
        ,@source))))

;;;###autoload
(defun le::consult-project-superset-setup ()
  "Make consult's project buffer/file sources span a repo + its `.le-playground'.
Rebuild climbing copies of consult's Project Buffer/File sources and splice
them into `consult-buffer-sources' (the -hidden pair, for `consult-buffer'
narrow `p') and `consult-project-buffer-sources' (the plain pair, for
`consult-project-buffer').  \"Project Root\" and the global
`consult-project-function' are left untouched.

Idempotent: the copies are always rebuilt from consult's own sources (so they
track `consult-customize'), and after the first splice the original symbols
are gone from the lists, so re-runs replace nothing further."
  (require 'consult)
  (setq le::consult-source-project-buffer
        (le::consult--climb-source consult-source-project-buffer)
        le::consult-source-project-recent-file
        (le::consult--climb-source consult-source-project-recent-file)
        le::consult-source-project-buffer-hidden
        (le::consult--climb-source consult-source-project-buffer-hidden)
        le::consult-source-project-recent-file-hidden
        (le::consult--climb-source consult-source-project-recent-file-hidden))
  ;; `consult-buffer' (C-x b, narrow `p') pulls the -hidden variants.
  (setq consult-buffer-sources
        (cl-substitute 'le::consult-source-project-buffer-hidden
                       'consult-source-project-buffer-hidden
                       (cl-substitute 'le::consult-source-project-recent-file-hidden
                                      'consult-source-project-recent-file-hidden
                                      consult-buffer-sources)))
  ;; `consult-project-buffer' (C-x p b) pulls the plain variants.
  (setq consult-project-buffer-sources
        (cl-substitute 'le::consult-source-project-buffer
                       'consult-source-project-buffer
                       (cl-substitute 'le::consult-source-project-recent-file
                                      'consult-source-project-recent-file
                                      consult-project-buffer-sources))))

(provide 'le-consult)
;;; le-consult.el ends here
