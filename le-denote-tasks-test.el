;;; le-denote-tasks-test.el --- Tests for le-denote-tasks' journal lookup  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for the functional core of `le::denote-tasks-find-journal': the
;; title a checkout's journal carries, and the front-matter predicate that
;; decides whether a candidate note is that journal.  Nothing here touches a
;; denote silo, a git checkout or the filesystem -- both functions are pure.
;; Run in batch:
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

(provide 'le-denote-tasks-test)
;;; le-denote-tasks-test.el ends here
