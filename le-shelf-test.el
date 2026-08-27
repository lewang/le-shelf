;;; le-shelf-test.el --- Tests for le-shelf  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for `le-cycle-spacing--region', the pure core of
;; `le-cycle-spacing': given the buffer around point it answers which
;; whitespace the next press deletes.  Since no press count is tracked, the
;; cases that carry weight are the ones where two kinds of whitespace are
;; available at once -- right beats left, left beats the line break -- and
;; the boundaries that keep a press from touching whitespace point is not
;; sitting against.
;;
;; Run in batch:
;;
;;   emacs -Q -batch -L "$PWD" -l "$PWD/le-shelf.el" -l "$PWD/le-shelf-test.el" \
;;     -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'le-shelf)

(defun le-shelf-test--cycle (before)
  "Return BEFORE after one `le-cycle-spacing' press.
Point is written as \"|\" in BEFORE and in the returned string."
  (with-temp-buffer
    (insert before)
    (goto-char (point-min))
    (search-forward "|")
    (delete-char -1)
    (le-cycle-spacing)
    (insert "|")
    (buffer-string)))

(ert-deftest le-shelf-test-cycle-spacing-clears-right-first ()
  (should (equal (le-shelf-test--cycle "foo|  bar") "foo|bar"))
  (should (equal (le-shelf-test--cycle "foo | bar") "foo |bar")))

(ert-deftest le-shelf-test-cycle-spacing-stops-at-end-of-line ()
  (should (equal (le-shelf-test--cycle "foo|  \n  bar") "foo|\n  bar")))

(ert-deftest le-shelf-test-cycle-spacing-clears-left-second ()
  (should (equal (le-shelf-test--cycle "foo  |bar") "foo|bar")))

(ert-deftest le-shelf-test-cycle-spacing-stops-at-beginning-of-line ()
  (should (equal (le-shelf-test--cycle "foo  \n  |bar") "foo  \n|bar")))

(ert-deftest le-shelf-test-cycle-spacing-merges-line-third ()
  (should (equal (le-shelf-test--cycle "foo\n|bar") "foo|bar"))
  (should (equal (le-shelf-test--cycle "foo  \n|bar") "foo|bar")))

(ert-deftest le-shelf-test-cycle-spacing-leaves-untouching-whitespace-alone ()
  (should (equal (le-shelf-test--cycle "foo\nbar|baz") "foo\nbar|baz"))
  (should (equal (le-shelf-test--cycle "foo\n  bar|baz") "foo\n  bar|baz")))

(ert-deftest le-shelf-test-cycle-spacing-merge-eats-blank-lines ()
  (should (equal (le-shelf-test--cycle "foo  \n\n  \n|bar") "foo|bar")))

(ert-deftest le-shelf-test-cycle-spacing-noop-at-beginning-of-buffer ()
  (should (equal (le-shelf-test--cycle "|foo") "|foo")))

(ert-deftest le-shelf-test-cycle-spacing-three-presses-join-lisp-parens ()
  (should (equal (le-shelf-test--cycle ") |  )") ") |)"))
  (should (equal (le-shelf-test--cycle ") |)") ")|)")))

(provide 'le-shelf-test)
;;; le-shelf-test.el ends here
