;;; le-org-codepath-test.el --- Tests for le-org-codepath  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for `le::org-codepath-escape'.  Pure -- no Org needed; run in
;; batch:
;;
;;   emacs -Q -batch -L . -l le-org-codepath.el -l le-org-codepath-test.el \
;;     -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'le-org-codepath)

(ert-deftest le::org-codepath-escape/bare-at-bol ()
  "A bare compressed ref at the start of the string is escaped."
  (should (equal (le::org-codepath-escape "=7=1") "\\=7=1")))

(ert-deftest le::org-codepath-escape/single-segment ()
  "A single-segment path is escaped like any other."
  (should (equal (le::org-codepath-escape "=7") "\\=7")))

(ert-deftest le::org-codepath-escape/multi-segment ()
  "A deep path escapes only its leading `='."
  (should (equal (le::org-codepath-escape "=1=2=3") "\\=1=2=3")))

(ert-deftest le::org-codepath-escape/after-space ()
  "A bare ref after whitespace is escaped."
  (should (equal (le::org-codepath-escape "see =7=1 here") "see \\=7=1 here")))

(ert-deftest le::org-codepath-escape/after-punctuation ()
  "A bare ref after punctuation is escaped."
  (should (equal (le::org-codepath-escape "(=7=1)") "(\\=7=1)")))

(ert-deftest le::org-codepath-escape/full-form-untouched ()
  "The full form `foo=1' is self-safe and left alone."
  (should (equal (le::org-codepath-escape "foo=1") "foo=1"))
  (should (equal (le::org-codepath-escape "denotecli=7=1") "denotecli=7=1")))

(ert-deftest le::org-codepath-escape/idempotent ()
  "An already-escaped ref is not double-escaped."
  (should (equal (le::org-codepath-escape "\\=7=1") "\\=7=1"))
  (should (equal (le::org-codepath-escape
                  (le::org-codepath-escape "=7=1"))
                 "\\=7=1")))

(ert-deftest le::org-codepath-escape/multiple-refs ()
  "Every bare ref in a string is escaped."
  (should (equal (le::org-codepath-escape "a =1 and =2=3")
                 "a \\=1 and \\=2=3")))

(ert-deftest le::org-codepath-escape/word-adjacent-not-escaped ()
  "A path run glued to a trailing word char is the full form -- untouched."
  (should (equal (le::org-codepath-escape "x=1") "x=1")))

(provide 'le-org-codepath-test)
;;; le-org-codepath-test.el ends here
