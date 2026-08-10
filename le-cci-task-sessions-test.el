;;; le-cci-task-sessions-test.el --- Tests for task-recorded conversations  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for the pure core of `le-cci-task-sessions': parsing and
;; formatting the CC_SESSIONS property, and matching a task's recorded
;; conversation against the live instances.
;;
;; The case that carries the design is `--match' finding a hit on a NON-HEAD
;; entry.  A task records the conversation its target instance was on at the
;; time; every /clear in that terminal since has pushed the uuid further down
;; the terminal's own list without changing which terminal it is.  Matching
;; head against head would lose the instance the first time it was cleared,
;; which is exactly the case this feature exists to get right.
;;
;; Prepending is `le::cci-session-ids--add', already covered by
;; le-cci-session-ids-test.el, so it is not re-tested here.
;;
;; Run in batch:
;;
;;   emacs -Q -batch -L "$PWD" \
;;     -l "$PWD/le-cci-session-ids.el" -l "$PWD/le-cci-task-sessions.el" \
;;     -l "$PWD/le-cci-task-sessions-test.el" \
;;     -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'le-cci-task-sessions)

;;;; Reading the property

(ert-deftest le::cci-task-sessions--parse/splits-in-order ()
  "Newest first is the property's own order; parsing must not disturb it."
  (should (equal (le::cci-task-sessions--parse "a") '("a")))
  (should (equal (le::cci-task-sessions--parse "a b c") '("a" "b" "c"))))

(ert-deftest le::cci-task-sessions--parse/tolerates-an-absent-property ()
  "A task never prompted from has no property at all -- nil, not an error."
  (should-not (le::cci-task-sessions--parse nil))
  (should-not (le::cci-task-sessions--parse ""))
  (should-not (le::cci-task-sessions--parse "   ")))

(ert-deftest le::cci-task-sessions--parse/collapses-stray-whitespace ()
  "Hand-edited or re-indented properties must still read as a clean list."
  (should (equal (le::cci-task-sessions--parse "  a   b  ") '("a" "b")))
  (should (equal (le::cci-task-sessions--parse "a\tb") '("a" "b"))))

;;;; Writing it back

(ert-deftest le::cci-task-sessions--format/round-trips ()
  (should (equal (le::cci-task-sessions--format '("a" "b")) "a b"))
  (let ((ids '("a" "b" "c")))
    (should (equal (le::cci-task-sessions--parse
                    (le::cci-task-sessions--format ids))
                   ids))))

(ert-deftest le::cci-task-sessions--format/empty-is-nil-not-blank ()
  "Nil lets a caller distinguish \"write nothing\" from \"write a blank\"."
  (should-not (le::cci-task-sessions--format nil)))

;;;; Matching a recorded conversation to a live instance

(ert-deftest le::cci-task-sessions--match/finds-a-non-head-entry ()
  "The instance is still the right one after it has been cleared.
The task recorded `old' when that was the terminal's current conversation;
two /clears later it sits third in the terminal's list, and matching only
head-to-head would miss it."
  (should (equal (le::cci-task-sessions--match
                  "old" '((buf-a . ("newest" "newer" "old"))))
                 'buf-a)))

(ert-deftest le::cci-task-sessions--match/picks-the-holder-among-siblings ()
  "Two instances of one project: the recorded uuid is what tells them apart."
  (should (equal (le::cci-task-sessions--match
                  "x" '((buf-a . ("p" "q")) (buf-b . ("r" "x"))))
                 'buf-b)))

(ert-deftest le::cci-task-sessions--match/no-holder-is-nil ()
  "The recorded conversation's instance is gone -- the caller must ask."
  (should-not (le::cci-task-sessions--match "x" '((buf-a . ("p" "q")))))
  (should-not (le::cci-task-sessions--match "x" nil))
  ;; An instance that never declared a conversation matches nothing.
  (should-not (le::cci-task-sessions--match "x" '((buf-a . nil)))))

(ert-deftest le::cci-task-sessions--match/nothing-recorded-matches-nothing ()
  "No head means the caller's clause never fires; it must not match a stray."
  (should-not (le::cci-task-sessions--match nil '((buf-a . ("x")))))
  (should-not (le::cci-task-sessions--match "" '((buf-a . ("x"))))))

(ert-deftest le::cci-task-sessions--match/first-match-wins ()
  "Two instances claiming one conversation is degenerate; the pick is stable."
  (should (equal (le::cci-task-sessions--match
                  "x" '((buf-a . ("x")) (buf-b . ("x"))))
                 'buf-a)))

(provide 'le-cci-task-sessions-test)
;;; le-cci-task-sessions-test.el ends here
