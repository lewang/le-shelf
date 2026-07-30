;;; le-project-test.el --- Tests for le-project's root and membership rule  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for `le::project-root-here' and the `.le-playground' membership
;; predicates.
;;
;; `le::project-root-here' is an `or' over two EFFECTFUL lookups -- `w-current'
;; reads tab state, `le::project-current' the filesystem -- so there is no pure
;; core: `w-current' is stubbed and the fallback rung runs against a throwaway
;; checkout.  The normalization cases carry the weight, a `~'-abbreviated root
;; being the failure that would pass unnoticed.
;;
;; The membership predicates split by arity, and the cases below pin the split
;; itself: a nested child project under ROOT must fail the IDENTITY predicate
;; while a directory deep inside the playground must pass the CONTAINMENT one.
;; Collapsing them would silently change whichever caller lost.
;;
;; Run in batch:
;;
;;   emacs -Q -batch -L "$PWD" -L <elpaca-builds>/w \
;;     -l "$PWD/le-project.el" -l "$PWD/le-project-test.el" \
;;     -f ert-run-tests-batch-and-exit
;;
;; The paths must be absolute: `-l le-project.el' is resolved against
;; `load-path' rather than the working directory, so the relative form fails to
;; load.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'le-project)

(defmacro le::project-test--with-workspace (root &rest body)
  "Run BODY with `w-current' reporting a workspace whose `:project-root' is ROOT.
ROOT nil stands for \"this tab carries no workspace\"."
  (declare (indent 1) (debug (form body)))
  `(cl-letf (((symbol-function 'w-current)
              (lambda () (when ,root (list :name "test" :project-root ,root)))))
     ,@body))

(defmacro le::project-test--with-checkout (var &rest body)
  "Bind VAR to a throwaway git checkout directory, run BODY, then delete it."
  (declare (indent 1) (debug (symbolp body)))
  `(let ((,var (file-name-as-directory
                (file-truename (make-temp-file "le-project-test" t)))))
     (unwind-protect
         (progn
           (let ((default-directory ,var))
             (ignore (process-lines "git" "init")))
           ,@body)
       (delete-directory ,var t))))

(defmacro le::project-test--with-playground (root play &rest body)
  "Bind ROOT to a throwaway checkout and PLAY to its playground worktree.
The playground carries its own `.git', so it reads as a separate repo."
  (declare (indent 2) (debug (symbolp symbolp body)))
  `(le::project-test--with-checkout ,root
     (let ((,play (file-name-as-directory
                   (expand-file-name ".le-playground" ,root))))
       (make-directory (expand-file-name ".git" ,play) t)
       ,@body)))

;;; The membership rule -----------------------------------------------------

(ert-deftest le::project-playground-dir/expands-and-terminates ()
  "The joined path is expanded and slash-terminated, so callers may prefix it."
  (should (equal (le::project-playground-dir "~/src/le-shelf")
                 (expand-file-name "~/src/le-shelf/.le-playground/"))))

(ert-deftest le::project-playground-p/needs-its-own-git ()
  "The name alone is not enough -- a plain subdirectory is not a worktree."
  (le::project-test--with-checkout root
    (let ((bare (expand-file-name ".le-playground" root)))
      (make-directory bare t)
      (should-not (le::project-playground-p bare)))))

(ert-deftest le::project-playground-p/accepts-either-path-form ()
  "A slash-terminated DIR must not read as an empty last component."
  (le::project-test--with-playground root play
    (should (le::project-playground-p play))
    (should (le::project-playground-p (directory-file-name play)))))

(ert-deftest le::project-playground-p/rejects-other-names ()
  "A repo that simply is not the playground answers nil."
  (le::project-test--with-playground root play
    (should (le::project-playground-p play))
    (should-not (le::project-playground-p root))))

(ert-deftest le::project-playground-of-p/root-is-its-own ()
  "The trivial arm: a project root belongs to itself."
  (le::project-test--with-checkout root
    (should (le::project-playground-of-p root root))))

(ert-deftest le::project-playground-of-p/playground-belongs-to-parent ()
  "The rule itself: ROOT's playground worktree counts as ROOT."
  (le::project-test--with-playground root play
    (should (le::project-playground-of-p play root))))

(ert-deftest le::project-playground-of-p/nested-child-excluded ()
  "IDENTITY, not containment -- a child repo under ROOT is its own project.
Containment would admit it, which is the whole reason this predicate is
separate from `le::project-adopt-playground-buffer-p'."
  (le::project-test--with-checkout root
    (let ((child (file-name-as-directory (expand-file-name "child" root))))
      (make-directory (expand-file-name ".git" child) t)
      (should-not (le::project-playground-of-p child root)))))

(ert-deftest le::project-playground-of-p/other-roots-playground-excluded ()
  "A real playground whose parent is not ROOT does not belong to ROOT."
  (le::project-test--with-playground root play
    (le::project-test--with-checkout other
      (should-not (le::project-playground-of-p play other)))))

(ert-deftest le::project-adopt-playground-buffer-p/deep-inside-adopted ()
  "CONTAINMENT: the buffer's `default-directory' may be any level deep."
  (with-temp-buffer
    (setq default-directory "/src/le-shelf/.le-playground/denote/journal/")
    (should (le::project-adopt-playground-buffer-p (current-buffer)
                                                   "/src/le-shelf/"))))

(ert-deftest le::project-adopt-playground-buffer-p/abbreviated-root ()
  "A `~'-abbreviated ROOT must still match an expanded `default-directory'."
  (with-temp-buffer
    (setq default-directory (expand-file-name "~/src/le-shelf/.le-playground/x/"))
    (should (le::project-adopt-playground-buffer-p (current-buffer)
                                                   "~/src/le-shelf/"))))

(ert-deftest le::project-adopt-playground-buffer-p/outside-not-adopted ()
  "A buffer in the repo proper needs no adopting, and does not get it."
  (with-temp-buffer
    (setq default-directory "/src/le-shelf/lisp/")
    (should-not (le::project-adopt-playground-buffer-p (current-buffer)
                                                       "/src/le-shelf/"))))

(ert-deftest le::project-adopt-playground-buffer-p/sibling-name-not-a-prefix ()
  "A directory merely starting with the playground's name is not inside it."
  (with-temp-buffer
    (setq default-directory "/src/le-shelf/.le-playground-old/")
    (should-not (le::project-adopt-playground-buffer-p (current-buffer)
                                                       "/src/le-shelf/"))))

;;; The workspace rung ------------------------------------------------------

(ert-deftest le::project-root-here/workspace-wins ()
  "A workspace root that is already canonical comes back unchanged."
  (le::project-test--with-workspace "/src/le-shelf/"
    (should (equal (le::project-root-here) "/src/le-shelf/"))))

(ert-deftest le::project-root-here/workspace-root-is-slash-terminated ()
  "Callers may concatenate onto the answer, so it always ends in a slash."
  (le::project-test--with-workspace "/src/le-shelf"
    (should (equal (le::project-root-here) "/src/le-shelf/"))))

(ert-deftest le::project-root-here/workspace-root-is-expanded ()
  "A `~'-abbreviated root must not escape the helper."
  (le::project-test--with-workspace "~/src/le-shelf/"
    (let ((answer (le::project-root-here)))
      (should-not (string-prefix-p "~" answer))
      (should (equal answer (expand-file-name "~/src/le-shelf/"))))))

;;; The fallback rung -------------------------------------------------------

(ert-deftest le::project-root-here/falls-back-to-le-project-current ()
  "With no workspace, resolve the project of `default-directory'."
  (le::project-test--with-checkout root
    (le::project-test--with-workspace nil
      (let ((default-directory root))
        (should (equal (le::project-root-here) root))))))

(ert-deftest le::project-root-here/nil-when-nothing-resolves ()
  "No workspace and no project is nil -- not an error, and not a guess."
  (le::project-test--with-checkout root
    (let ((outside (file-name-as-directory (expand-file-name "outside" root))))
      (make-directory outside t)
      (delete-directory (expand-file-name ".git" root) t)
      (le::project-test--with-workspace nil
        (let ((default-directory outside))
          (should-not (le::project-root-here)))))))

(provide 'le-project-test)
;;; le-project-test.el ends here
