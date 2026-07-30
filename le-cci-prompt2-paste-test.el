;;; le-cci-prompt2-paste-test.el --- Tests for CCI paste-placeholder expansion  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for the pure core of `le-cci-prompt2-paste': the placeholder
;; substitution and the cross-entry paste index.  Reading
;; ~/.claude/history.jsonl and the paste cache is the imperative shell and is
;; not exercised here -- every case below builds its own hash tables, so the
;; suite needs no filesystem and no Claude install.
;;
;; Run in batch:
;;
;;   emacs -Q -batch -L "$PWD" \
;;     -l "$PWD/le-cci-prompt2-paste.el" -l "$PWD/le-cci-prompt2-paste-test.el" \
;;     -f ert-run-tests-batch-and-exit
;;
;; The paths must be absolute: `-l le-cci-prompt2-paste.el' is resolved against
;; `load-path' rather than the working directory, so the relative form fails to
;; load.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'le-cci-prompt2-paste)

(defun le::cci-prompt2-paste-test--pasted (&rest specs)
  "Build a pastedContents hash table from SPECS, each (ID KEY VALUE).
KEY is :content for an entry carrying its payload inline, or :hash for
the elided shape that carries only a contentHash."
  (let ((table (make-hash-table :test #'equal)))
    (pcase-dolist (`(,id ,key ,value) specs)
      (let ((obj (make-hash-table :test #'equal)))
        (puthash "type" "text" obj)
        (puthash (pcase key (:content "content") (:hash "contentHash")) value obj)
        (puthash id obj table)))
    table))

(defun le::cci-prompt2-paste-test--record (session ts pasted)
  "Return a history record plist with SESSION, TS and PASTED."
  (list :display "" :pasted pasted :ts ts :session session))

(defun le::cci-prompt2-paste-test--key (session id)
  "Return the index key `le::cci-prompt2--paste-index' builds for SESSION and ID.
Spelled by `concat' because \"s1\\0 1\" written as one literal would read
the digit as part of an octal escape."
  (concat session "\0" id))

(defun le::cci-prompt2-paste-test--payloads (index session id)
  "Return INDEX's payload strings for SESSION and ID, sorted."
  (sort (mapcar #'cdr (gethash (le::cci-prompt2-paste-test--key session id) index))
        #'string<))


;;;; Expansion

(ert-deftest le::cci-prompt2-paste-test-inline-content ()
  "An inline payload replaces its placeholder in place."
  (should (equal (le::cci-prompt2--expand-pastes
                  "see [Pasted text #1 +2 lines] please"
                  (le::cci-prompt2-paste-test--pasted '("1" :content "a\nb\nc"))
                  nil)
                 "see a\nb\nc please")))

(ert-deftest le::cci-prompt2-paste-test-unanswered-survives ()
  "A placeholder no source answers is left exactly as it stands."
  (let ((display "see [Pasted text #1 +2 lines] please"))
    (should (equal (le::cci-prompt2--expand-pastes display nil nil) display))
    (should (equal (le::cci-prompt2--expand-pastes
                    display
                    (le::cci-prompt2-paste-test--pasted '("1" :hash "deadbeef"))
                    nil)
                   display))
    (should (equal (le::cci-prompt2--expand-pastes
                    display nil (lambda (_id _lines) nil))
                   display))))

(ert-deftest le::cci-prompt2-paste-test-inline-beats-lookup ()
  "The entry's own payload wins and LOOKUP is never consulted."
  (let (called)
    (should (equal (le::cci-prompt2--expand-pastes
                    "[Pasted text #1 +1 lines]"
                    (le::cci-prompt2-paste-test--pasted
                     '("1" :content "own\npayload"))
                    (lambda (_id _lines) (setq called t) "borrowed"))
                   "own\npayload"))
    (should-not called)))

(ert-deftest le::cci-prompt2-paste-test-expected-lines ()
  "LOOKUP is handed the payload line count the placeholder's suffix implies."
  (should (equal (le::cci-prompt2--paste-expected-lines "4") 5))
  (should (equal (le::cci-prompt2--paste-expected-lines nil) 1))
  (let (seen)
    (le::cci-prompt2--expand-pastes
     "[Pasted text #7 +4 lines] and [Pasted text #8]"
     nil
     (lambda (id lines) (push (cons id lines) seen) nil))
    (should (equal (nreverse seen) '(("7" . 5) ("8" . 1))))))

(ert-deftest le::cci-prompt2-paste-test-multiple-placeholders ()
  "Each placeholder in one display resolves independently of the others."
  (should (equal (le::cci-prompt2--expand-pastes
                  "[Pasted text #1 +0 lines]/[Pasted text #2 +0 lines]/[Pasted text #3 +0 lines]"
                  (le::cci-prompt2-paste-test--pasted '("1" :content "one")
                                                      '("3" :content "three"))
                  (lambda (id _lines) (and (equal id "2") "two")))
                 "one/two/three")))

(ert-deftest le::cci-prompt2-paste-test-replacement-is-literal ()
  "A payload's backslashes reach the text untouched."
  (should (equal (le::cci-prompt2--expand-pastes
                  "[Pasted text #1]"
                  (le::cci-prompt2-paste-test--pasted '("1" :content "\\1 \\& \\\\"))
                  nil)
                 "\\1 \\& \\\\")))


;;;; The index

(ert-deftest le::cci-prompt2-paste-test-index-collects-per-session ()
  "Payloads sharing a paste id collect per session, never across."
  (let ((index (le::cci-prompt2--paste-index
                (list (le::cci-prompt2-paste-test--record
                       "s1" 200.0
                       (le::cci-prompt2-paste-test--pasted '("1" :content "later")))
                      (le::cci-prompt2-paste-test--record
                       "s1" 100.0
                       (le::cci-prompt2-paste-test--pasted '("1" :content "earlier")))
                      (le::cci-prompt2-paste-test--record
                       "s2" 150.0
                       (le::cci-prompt2-paste-test--pasted '("1" :content "elsewhere")))))))
    (should (equal (le::cci-prompt2-paste-test--payloads index "s1" "1")
                   '("earlier" "later")))
    (should (equal (le::cci-prompt2-paste-test--payloads index "s2" "1")
                   '("elsewhere")))
    (should-not (gethash (le::cci-prompt2-paste-test--key "s1" "2") index))))

(ert-deftest le::cci-prompt2-paste-test-index-carries-timestamps ()
  "Each indexed payload keeps its record's timestamp, so recency is decidable."
  (let ((index (le::cci-prompt2--paste-index
                (list (le::cci-prompt2-paste-test--record
                       "s1" 200.0
                       (le::cci-prompt2-paste-test--pasted '("1" :content "later")))
                      (le::cci-prompt2-paste-test--record
                       "s1" 100.0
                       (le::cci-prompt2-paste-test--pasted '("1" :content "earlier")))))))
    (should (equal (sort (copy-sequence
                          (gethash (le::cci-prompt2-paste-test--key "s1" "1") index))
                         (lambda (a b) (< (car a) (car b))))
                   '((100.0 . "earlier") (200.0 . "later"))))))

(ert-deftest le::cci-prompt2-paste-test-index-skips-unusable ()
  "Nothing to borrow is nothing to index: no payload, no session, no table."
  (let ((index (le::cci-prompt2--paste-index
                (list (le::cci-prompt2-paste-test--record
                       "s1" 100.0
                       (le::cci-prompt2-paste-test--pasted '("1" :hash "deadbeef")))
                      (le::cci-prompt2-paste-test--record
                       nil 100.0
                       (le::cci-prompt2-paste-test--pasted '("2" :content "orphan")))
                      (le::cci-prompt2-paste-test--record "s1" 100.0 nil)))))
    (should (zerop (hash-table-count index)))))

(ert-deftest le::cci-prompt2-paste-test-index-indexes-every-id ()
  "One record contributing several pastes indexes each of them."
  (let ((index (le::cci-prompt2--paste-index
                (list (le::cci-prompt2-paste-test--record
                       "s1" 100.0
                       (le::cci-prompt2-paste-test--pasted
                        '("1" :content "one") '("2" :content "two")))))))
    (should (equal (le::cci-prompt2-paste-test--payloads index "s1" "1") '("one")))
    (should (equal (le::cci-prompt2-paste-test--payloads index "s1" "2") '("two")))))

(provide 'le-cci-prompt2-paste-test)
;;; le-cci-prompt2-paste-test.el ends here
