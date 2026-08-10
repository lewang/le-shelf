;;; le-cci-prompt2-core-test.el --- Tests for cci target resolution  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for the pure core of `le-cci-prompt2-core': choosing which of a
;; project's Claude instances a prompt targets.
;;
;; The rule only bites when a project runs several instances.  The case that
;; carries it is a lone visible session that is NOT the most recently used
;; one: hiding an instance stamps it as recently used, so the instance just
;; dismissed outranks the one still on screen, and MRU alone aims the prompt
;; off-screen.  Preferring the displayed session is what fixes that, and
;; testing it against MRU-disagrees is the only way to see the difference --
;; when the two agree, any rule passes.
;;
;; The window and session-table reads live in
;; `le::cci-prompt2--session-for-root', which is a thin caller and is not
;; tested here.
;;
;; Run in batch (needs the package load-path for le-cci-prompt2-core's own
;; requires -- claude-code-ide, w, denote):
;;
;;   emacs -Q -batch -l /path/to/load-path.el -L "$PWD" \
;;     -l "$PWD/le-cci-prompt2-core-test.el" \
;;     -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'le-cci-prompt2-core)

(ert-deftest le::cci-prompt2--pick-session/lone-visible-beats-mru ()
  "The case the rule exists for: what is on screen is not what MRU names."
  (should (equal (le::cci-prompt2--pick-session '(:shown) :mru) :shown)))

(ert-deftest le::cci-prompt2--pick-session/nothing-visible-falls-back ()
  "Composing from a frame showing no instance -- nothing better than MRU."
  (should (equal (le::cci-prompt2--pick-session nil :mru) :mru)))

(ert-deftest le::cci-prompt2--pick-session/several-visible-falls-back ()
  "Two instances on screen is ambiguous, not an answer; MRU breaks the tie."
  (should (equal (le::cci-prompt2--pick-session '(:a :b) :mru) :mru)))

(ert-deftest le::cci-prompt2--pick-session/lone-visible-wins-without-an-mru ()
  "A visible session stands on its own; it is not a tiebreak against MRU."
  (should (equal (le::cci-prompt2--pick-session '(:shown) nil) :shown)))

(ert-deftest le::cci-prompt2--pick-session/nothing-to-go-on-is-nil ()
  "No instance displayed and none recorded -- the caller signals, not this."
  (should-not (le::cci-prompt2--pick-session nil nil)))

(provide 'le-cci-prompt2-core-test)
;;; le-cci-prompt2-core-test.el ends here
