;;; le-project-flip-test.el --- Tests for flip scoping  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for `le::project-flip--keep-p', the pure core of the flip: given
;; a pane role and a workspace root it answers which buffers a flip may
;; surface, and `le::project-flip--run' is a thin caller that negates it into
;; `buffer-flip-skip-patterns'.
;;
;; The cases that carry weight are about COMPOSITION, since that is what
;; differs per pane: the two left panes intersect their display role with
;; workspace membership, while the cc pane's predicate stands alone.  Collapsing
;; that distinction is silent -- flips keep working, they just quietly drop the
;; Claude instances whose terminal has cd'd out of the project.
;;
;; Run in batch:
;;
;;   emacs -Q -batch -L "$PWD" -L <elpaca-builds>/w -L <elpaca-builds>/buffer-flip \
;;     -l "$PWD/le-project-flip.el" -l "$PWD/le-project-flip-test.el" \
;;     -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'le-project-flip)

(defvar le::project-flip-test--membership-calls nil
  "Buffers `w-buffer-in-project-p' was asked about.")

(defmacro le::project-flip-test--with-stubs (in-project cc-buffers &rest body)
  "Run BODY with the routing predicates stubbed.
IN-PROJECT is the list of buffers `w-buffer-in-project-p' accepts and
CC-BUFFERS the list `le::frame-cc-buffer-p' accepts.  The two left-pane
predicates split on a buffer's name: one starting with \"magit\" is
bottom-left, anything else top-left."
  (declare (indent 2) (debug (form form body)))
  `(let ((le::project-flip-test--membership-calls nil))
     (cl-letf (((symbol-function 'w-buffer-in-project-p)
                (lambda (buf _root)
                  (push buf le::project-flip-test--membership-calls)
                  (and (memq buf ,in-project) t)))
               ((symbol-function 'le::frame-cc-buffer-p)
                (lambda (buf &optional _act) (and (memq buf ,cc-buffers) t)))
               ((symbol-function 'le::frame-bottom-left-buffer-p)
                (lambda (buf &optional _act)
                  (string-prefix-p "magit" (buffer-name buf))))
               ((symbol-function 'le::frame-top-left-buffer-p)
                (lambda (buf &optional _act)
                  (not (string-prefix-p "magit" (buffer-name buf))))))
       ,@body)))

(defmacro le::project-flip-test--with-buffers (names &rest body)
  "Run BODY with a live buffer per NAMES, bound to `bufs' in order."
  (declare (indent 1) (debug (form body)))
  `(let ((bufs (mapcar #'generate-new-buffer ,names)))
     (unwind-protect (progn ,@body)
       (mapc #'kill-buffer bufs))))

;;;; The cc pane: its predicate stands alone

(ert-deftest le::project-flip--keep-p/cc-pane-keeps-only-claude-terminals ()
  "A flip in the cc pane surfaces the project's Claude instances and nothing else."
  (le::project-flip-test--with-buffers '("*cci: proj*" "*cci: proj:two*" "file.el")
    (let ((keep (le::project-flip--keep-p 'cc "/tmp/proj/")))
      (le::project-flip-test--with-stubs bufs (list (nth 0 bufs) (nth 1 bufs))
        (should (funcall keep (nth 0 bufs)))
        (should (funcall keep (nth 1 bufs)))
        (should-not (funcall keep (nth 2 bufs)))))))

(ert-deftest le::project-flip--keep-p/cc-pane-ignores-workspace-membership ()
  "The regression this composition exists to avoid: an instance whose terminal
has cd'd out of the project still belongs to the cc pane, because the session
table already scoped it and `w-buffer-in-project-p' reads `default-directory'."
  (le::project-flip-test--with-buffers '("*cci: drifted*")
    (let ((keep (le::project-flip--keep-p 'cc "/tmp/proj/")))
      (le::project-flip-test--with-stubs nil bufs
        (should (funcall keep (car bufs)))
        (should-not le::project-flip-test--membership-calls)))))

;;;; The left panes: display role INTERSECTED with workspace membership

(ert-deftest le::project-flip--keep-p/top-left-pane-composes ()
  "Top-left keeps project buffers that are not magit-status."
  (le::project-flip-test--with-buffers '("file.el" "magit: proj" "foreign.el")
    (let ((keep (le::project-flip--keep-p 'top-left "/tmp/proj/")))
      (le::project-flip-test--with-stubs (list (nth 0 bufs) (nth 1 bufs)) nil
        (should (funcall keep (nth 0 bufs)))
        (should-not (funcall keep (nth 1 bufs)))
        (should-not (funcall keep (nth 2 bufs)))))))

(ert-deftest le::project-flip--keep-p/bottom-left-pane-composes ()
  "Bottom-left keeps only the magit-status/popup role, still project-scoped."
  (le::project-flip-test--with-buffers '("magit: proj" "file.el" "magit: foreign")
    (let ((keep (le::project-flip--keep-p 'bottom-left "/tmp/proj/")))
      (le::project-flip-test--with-stubs (list (nth 0 bufs) (nth 1 bufs)) nil
        (should (funcall keep (nth 0 bufs)))
        (should-not (funcall keep (nth 1 bufs)))
        (should-not (funcall keep (nth 2 bufs)))))))

(ert-deftest le::project-flip--keep-p/claude-terminals-are-not-left-pane-buffers ()
  "A Claude terminal is claimed by the cc pane, so a left-pane flip drops it
even though it passes workspace membership."
  (le::project-flip-test--with-buffers '("*cci: proj*")
    (let ((keep (le::project-flip--keep-p 'top-left "/tmp/proj/")))
      ;; The real `le::frame-top-left-buffer-p' excludes cc buffers; model that
      ;; by asking the cc predicate, as the real one does.
      (cl-letf (((symbol-function 'w-buffer-in-project-p) (lambda (&rest _) t))
                ((symbol-function 'le::frame-top-left-buffer-p)
                 (lambda (&rest _) nil)))
        (should-not (funcall keep (car bufs)))))))

;;;; No pane: workspace membership alone

(ert-deftest le::project-flip--keep-p/outside-the-layout-keeps-the-workspace ()
  "With no recognized pane the flip is scoped by workspace membership only."
  (le::project-flip-test--with-buffers '("file.el" "magit: proj" "foreign.el")
    (let ((keep (le::project-flip--keep-p nil "/tmp/proj/")))
      (le::project-flip-test--with-stubs (list (nth 0 bufs) (nth 1 bufs)) nil
        (should (funcall keep (nth 0 bufs)))
        (should (funcall keep (nth 1 bufs)))
        (should-not (funcall keep (nth 2 bufs)))))))

(provide 'le-project-flip-test)
;;; le-project-flip-test.el ends here
