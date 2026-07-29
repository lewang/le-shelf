;;; le-project-test.el --- Tests for le-project's authoritative root  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for `le::project-root-here'.  It is an `or' over two EFFECTFUL
;; lookups -- `w-current' reads tab state, `le::project-current' the filesystem
;; -- so there is no pure core: `w-current' is stubbed and the fallback rung runs
;; against a throwaway checkout.  The normalization cases carry the weight, a
;; `~'-abbreviated root being the failure that would pass unnoticed.
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
