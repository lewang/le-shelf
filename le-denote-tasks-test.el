;;; le-denote-tasks-test.el --- Tests for le-denote-tasks' journal lookup  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for the functional core of `le::denote-tasks-find-journal': the
;; title a checkout's journal carries, and the front-matter predicate that
;; decides whether a candidate note is that journal.  Both are pure, so those
;; tests touch no denote silo, git checkout or filesystem.
;;
;; The one exception is at the end: `le::denote-tasks-checkout-tags' shells out
;; to git, and its regression -- a subprocess launched in the CALLER's
;; `default-directory' rather than in the checkout -- is unreachable through a
;; pure function.  Those two build a throwaway git checkout in a temp
;; directory.  Run in batch:
;;
;;   emacs -Q -batch -L "$PWD" -L <elpaca-builds>/denote \
;;     -l "$PWD/le-denote-tasks.el" -l "$PWD/le-denote-tasks-test.el" \
;;     -f ert-run-tests-batch-and-exit
;;
;; The paths must be absolute: `-l le-denote-tasks.el' is resolved against
;; `load-path' rather than the working directory, so the relative form fails to
;; load.

;;; Code:

(require 'ert)
(require 'le-denote-tasks)

;;; Journal title ----------------------------------------------------------

(ert-deftest le::denote-tasks--journal-title/names-the-checkout-root ()
  (should (equal (le::denote-tasks--journal-title "/src/le-shelf")
                 "journal le-shelf")))

(ert-deftest le::denote-tasks--journal-title/trailing-slash-is-ignored ()
  (should (equal (le::denote-tasks--journal-title "/src/le-shelf/")
                 (le::denote-tasks--journal-title "/src/le-shelf"))))

(ert-deftest le::denote-tasks--journal-title/worktree-keeps-its-own-name ()
  "A linked worktree is its own directory, so it gets its own journal title."
  (should-not (equal (le::denote-tasks--journal-title "/src/goodbear-wt-2/")
                     (le::denote-tasks--journal-title "/src/goodbear/")))
  (should (equal (le::denote-tasks--journal-title "/src/goodbear-wt-2/")
                 "journal goodbear-wt-2")))

(ert-deftest le::denote-tasks--journal-title/name-is-not-sluggified ()
  "The title is front matter, not a file-name component: hyphens survive."
  (should (equal (le::denote-tasks--journal-title "/src/le-shelf")
                 "journal le-shelf")))

;;; Journal match predicate ------------------------------------------------

(ert-deftest le::denote-tasks--journal-match-p/title-and-tags-match ()
  (should (le::denote-tasks--journal-match-p
           "journal le-shelf" '("projectleshelf")
           "journal le-shelf" '("projectleshelf"))))

(ert-deftest le::denote-tasks--journal-match-p/wrong-title-loses ()
  "Another project's journal in the same silo must not match."
  (should-not (le::denote-tasks--journal-match-p
               "journal goodbear" '("projectleshelf")
               "journal le-shelf" '("projectleshelf"))))

(ert-deftest le::denote-tasks--journal-match-p/title-match-is-exact ()
  (should-not (le::denote-tasks--journal-match-p
               "journal le-shelf notes" '("projectleshelf")
               "journal le-shelf" '("projectleshelf"))))

(ert-deftest le::denote-tasks--journal-match-p/missing-tag-loses ()
  "A worktree journal needs both tags; the project tag alone is not enough."
  (should-not (le::denote-tasks--journal-match-p
               "journal goodbear-wt-2" '("projectgoodbear")
               "journal goodbear-wt-2" '("projectgoodbear" "worktreewt2"))))

(ert-deftest le::denote-tasks--journal-match-p/extra-keywords-are-allowed ()
  "Keywords need only COVER the wanted tags -- the user may add their own."
  (should (le::denote-tasks--journal-match-p
           "journal le-shelf" '("projectleshelf" "scratch")
           "journal le-shelf" '("projectleshelf"))))

(ert-deftest le::denote-tasks--journal-match-p/keyword-order-is-irrelevant ()
  (should (le::denote-tasks--journal-match-p
           "journal goodbear-wt-2" '("worktreewt2" "projectgoodbear")
           "journal goodbear-wt-2" '("projectgoodbear" "worktreewt2"))))

(ert-deftest le::denote-tasks--journal-match-p/main-journal-loses-to-worktree-tags ()
  "The main checkout's journal must not answer a linked worktree's lookup."
  (should-not (le::denote-tasks--journal-match-p
               "journal goodbear" '("projectgoodbear")
               "journal goodbear-wt-2" '("projectgoodbear" "worktreewt2"))))

(ert-deftest le::denote-tasks--journal-match-p/absent-front-matter-title-loses ()
  "A note whose title could not be retrieved is nil-titled, never a match."
  (should-not (le::denote-tasks--journal-match-p
               nil '("projectleshelf")
               "journal le-shelf" '("projectleshelf"))))

;;; Git identity, from a hostile default-directory -------------------------

(defmacro le::denote-tasks-test--with-checkout (var &rest body)
  "Bind VAR to a throwaway git checkout holding a `.le-playground', run BODY.
The checkout is deleted afterwards even when BODY fails."
  (declare (indent 1) (debug (symbolp body)))
  `(let ((,var (file-name-as-directory
                (make-temp-file "le-denote-tasks-test" t))))
     (unwind-protect
         (progn
           (make-directory (expand-file-name ".le-playground" ,var))
           (let ((default-directory ,var))
             (ignore (process-lines "git" "init")))
           ,@body)
       (delete-directory ,var t))))

(defun le::denote-tasks-test--project-tag (checkout)
  "The lone `project<name>' tag a main CHECKOUT should yield."
  (list (concat "project"
                (le::denote-tasks--collapse
                 (file-name-nondirectory (directory-file-name checkout))))))

(ert-deftest le::denote-tasks-checkout-tags/reads-a-live-default-directory ()
  "The control for the test below: an ordinary cwd inside the checkout."
  (le::denote-tasks-test--with-checkout root
    (let ((default-directory root))
      (should (equal (le::denote-tasks-checkout-tags
                      (expand-file-name ".le-playground/denote/journal/" root))
                     (le::denote-tasks-test--project-tag root))))))

(ert-deftest le::denote-tasks-checkout-tags/survives-a-deleted-default-directory ()
  "Tags must still resolve when the CALLER's own directory is gone.
`process-lines' launches git in `default-directory', while `-C' only steers git
once it runs, so leaving the former unbound let a dead cwd fail the launch
outright -- which `ignore-errors' then reported as though the checkout were not
a git repository at all.  A dired buffer whose directory was deleted underneath
it is the everyday way to reach this."
  (le::denote-tasks-test--with-checkout root
    (let ((default-directory (expand-file-name "gone/" root)))
      (should-not (file-directory-p default-directory))
      (should (equal (le::denote-tasks-checkout-tags
                      (expand-file-name ".le-playground/denote/journal/" root))
                     (le::denote-tasks-test--project-tag root))))))

(provide 'le-denote-tasks-test)
;;; le-denote-tasks-test.el ends here
