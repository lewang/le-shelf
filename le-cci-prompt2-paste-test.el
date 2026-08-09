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


;;;; Borrowing from an earlier submission

(ert-deftest le::cci-prompt2-paste-test-sibling-needs-the-line-count ()
  "A candidate of the wrong length is refused even with nothing else on offer."
  (let ((index (le::cci-prompt2--paste-index
                (list (le::cci-prompt2-paste-test--record
                       "s1" 100.0
                       (le::cci-prompt2-paste-test--pasted
                        '("1" :content "a\nb\nc")))))))
    (should (equal (le::cci-prompt2--paste-sibling index "s1" "1" 200.0 3)
                   "a\nb\nc"))
    (should-not (le::cci-prompt2--paste-sibling index "s1" "1" 200.0 4))
    (should-not (le::cci-prompt2--paste-sibling index "s1" "1" 200.0 2))))

(ert-deftest le::cci-prompt2-paste-test-sibling-prefers-recency ()
  "Two candidates of the right length: the later one wins."
  (let ((index (le::cci-prompt2--paste-index
                (list (le::cci-prompt2-paste-test--record
                       "s1" 100.0
                       (le::cci-prompt2-paste-test--pasted '("1" :content "old\nold")))
                      (le::cci-prompt2-paste-test--record
                       "s1" 200.0
                       (le::cci-prompt2-paste-test--pasted '("1" :content "new\nnew")))))))
    (should (equal (le::cci-prompt2--paste-sibling index "s1" "1" 300.0 2)
                   "new\nnew"))
    ;; ...unless recency puts it out of reach: at 150 only the old one is earlier
    (should (equal (le::cci-prompt2--paste-sibling index "s1" "1" 150.0 2)
                   "old\nold"))))

(ert-deftest le::cci-prompt2-paste-test-sibling-must-be-earlier ()
  "A payload recorded at or after the entry is never borrowed.
Recalling a prompt is what loses its paste map, so the payload always
predates the entry that lost it -- a later one is a different paste."
  (let ((index (le::cci-prompt2--paste-index
                (list (le::cci-prompt2-paste-test--record
                       "s1" 200.0
                       (le::cci-prompt2-paste-test--pasted '("1" :content "later")))))))
    (should-not (le::cci-prompt2--paste-sibling index "s1" "1" 200.0 1))
    (should-not (le::cci-prompt2--paste-sibling index "s1" "1" 100.0 1))))

(ert-deftest le::cci-prompt2-paste-test-sibling-stays-in-session ()
  "Another session's payload under the same paste id is not a candidate."
  (let ((index (le::cci-prompt2--paste-index
                (list (le::cci-prompt2-paste-test--record
                       "s2" 100.0
                       (le::cci-prompt2-paste-test--pasted '("1" :content "theirs")))))))
    (should-not (le::cci-prompt2--paste-sibling index "s1" "1" 200.0 1))
    (should (equal (le::cci-prompt2--paste-sibling index "s2" "1" 200.0 1)
                   "theirs"))))

(ert-deftest le::cci-prompt2-paste-test-line-count ()
  "Line counting matches the \"+K lines\" convention: newlines plus one."
  (should (equal (le::cci-prompt2--paste-line-count "one") 1))
  (should (equal (le::cci-prompt2--paste-line-count "a\nb") 2))
  (should (equal (le::cci-prompt2--paste-line-count "a\nb\n") 3)))


;;;; The cache, the marker, and the lookup that orders them

(ert-deftest le::cci-prompt2-paste-test-cache-content ()
  "A cache hit returns the file verbatim; a miss returns nil."
  (let ((dir (file-name-as-directory (make-temp-file "cci-paste-test" t))))
    (unwind-protect
        (let ((le::cci-prompt2--paste-cache-dir dir))
          (with-temp-file (expand-file-name "abc123.txt" dir)
            (insert "cached\npayload"))
          (should (equal (le::cci-prompt2--paste-cache-content "abc123")
                         "cached\npayload"))
          (should-not (le::cci-prompt2--paste-cache-content "deadbeef")))
      (delete-directory dir t))))

(ert-deftest le::cci-prompt2-paste-test-hash-must-be-hex ()
  "A contentHash that could name a file outside the cache is refused."
  (should (equal (le::cci-prompt2--paste-hash
                  (gethash "1" (le::cci-prompt2-paste-test--pasted
                                '("1" :hash "0123456789abcdef"))))
                 "0123456789abcdef"))
  (should-not (le::cci-prompt2--paste-hash
               (gethash "1" (le::cci-prompt2-paste-test--pasted
                             '("1" :hash "../../etc/passwd")))))
  (should-not (le::cci-prompt2--paste-hash
               (gethash "1" (le::cci-prompt2-paste-test--pasted
                             '("1" :content "inline")))))
  (should-not (le::cci-prompt2--paste-hash nil)))

(ert-deftest le::cci-prompt2-paste-test-marker ()
  "The marker names the paste and its claimed length, and is not prose."
  (should (equal (le::cci-prompt2--paste-marker "6" 5)
                 "[paste #6 unrecoverable: 5 lines]"))
  (should (equal (le::cci-prompt2--paste-marker "2" 1)
                 "[paste #2 unrecoverable: 1 line]")))

(ert-deftest le::cci-prompt2-paste-test-lookup-source-order ()
  "The cache outranks a sibling, a sibling outranks the marker."
  (let ((dir (file-name-as-directory (make-temp-file "cci-paste-test" t))))
    (unwind-protect
        (let* ((le::cci-prompt2--paste-cache-dir dir)
               (index (le::cci-prompt2--paste-index
                       (list (le::cci-prompt2-paste-test--record
                              "s1" 100.0
                              (le::cci-prompt2-paste-test--pasted
                               '("1" :content "sibling")
                               '("2" :content "sibling"))))))
               (elided (le::cci-prompt2-paste-test--record
                        "s1" 300.0
                        (le::cci-prompt2-paste-test--pasted
                         '("1" :hash "abc123"))))
               (bare (le::cci-prompt2-paste-test--record "s1" 300.0 nil)))
          (with-temp-file (expand-file-name "abc123.txt" dir) (insert "cached"))
          ;; id 1 is in the cache AND has a sibling -- the cache answers
          (should (equal (funcall (le::cci-prompt2--paste-lookup elided index) "1" 1)
                         "cached"))
          ;; id 2 is only a sibling
          (should (equal (funcall (le::cci-prompt2--paste-lookup bare index) "2" 1)
                         "sibling"))
          ;; id 3 is neither
          (should (equal (funcall (le::cci-prompt2--paste-lookup bare index) "3" 4)
                         "[paste #3 unrecoverable: 4 lines]")))
      (delete-directory dir t))))

(ert-deftest le::cci-prompt2-paste-test-lookup-never-declines ()
  "Every placeholder comes out expanded or marked -- never left bare."
  (let* ((index (le::cci-prompt2--paste-index
                 (list (le::cci-prompt2-paste-test--record
                        "s1" 100.0
                        (le::cci-prompt2-paste-test--pasted
                         '("2" :content "borrowed\ntwo"))))))
         (record (le::cci-prompt2-paste-test--record
                  "s1" 300.0
                  (le::cci-prompt2-paste-test--pasted '("1" :content "own"))))
         (text (le::cci-prompt2--expand-pastes
                "[Pasted text #1 +0 lines] [Pasted text #2 +1 lines] [Pasted text #3 +9 lines]"
                (plist-get record :pasted)
                (le::cci-prompt2--paste-lookup record index))))
    (should (equal text
                   "own borrowed\ntwo [paste #3 unrecoverable: 10 lines]"))
    (should-not (string-match-p "Pasted text #" text))))

;;;; Scoping a project-wide history to one instance

(defun le::cci-prompt2-paste-test--entry (session ts text)
  "Return a history entry plist with SESSION, TS and TEXT."
  (list :id nil :state "CLI" :text text :ts ts :session session))

(ert-deftest le::cci-prompt2-paste-test-entries-for-sessions ()
  "Only the target instance's conversations survive the narrowing."
  (let ((entries (list (le::cci-prompt2-paste-test--entry "a" 300.0 "mine, now")
                       (le::cci-prompt2-paste-test--entry "b" 200.0 "a sibling's")
                       (le::cci-prompt2-paste-test--entry "a0" 100.0 "mine, before /clear"))))
    ;; One instance owns many conversations -- /clear mints a fresh one in
    ;; the same terminal, and both halves of its history stay in scope.
    (should (equal (mapcar (lambda (e) (plist-get e :text))
                           (le::cci-prompt2--entries-for-sessions entries '("a" "a0")))
                   '("mine, now" "mine, before /clear")))
    (should-not (le::cci-prompt2--entries-for-sessions entries '("unknown")))
    (should-not (le::cci-prompt2--entries-for-sessions entries nil))))

(ert-deftest le::cci-prompt2-paste-test-entries-for-sessions-drops-unattributed ()
  "A heading written before instances were recorded belongs to none of them.
Project scope is where it stays reachable; admitting it here would put a
sibling's prompt in this instance's history."
  (let ((entries (list (le::cci-prompt2-paste-test--entry "a" 300.0 "attributed")
                       (le::cci-prompt2-paste-test--entry nil 200.0 "older heading"))))
    (should (equal (mapcar (lambda (e) (plist-get e :text))
                           (le::cci-prompt2--entries-for-sessions entries '("a")))
                   '("attributed")))))

(ert-deftest le::cci-prompt2-paste-test-entry-position ()
  "The shown prompt is re-found across a scope change, by prompt not by index."
  (let* ((mine (le::cci-prompt2-paste-test--entry "a" 300.0 "mine"))
         (theirs (le::cci-prompt2-paste-test--entry "b" 200.0 "theirs"))
         (widened (list mine theirs))
         (narrowed (list mine)))
    ;; Widening: same prompt, new index.
    (should (equal (le::cci-prompt2--entry-position widened
                                                    (le::cci-prompt2-paste-test--entry
                                                     "b" 200.0 "theirs"))
                   1))
    (should (equal (le::cci-prompt2--entry-position narrowed mine) 0))
    ;; Narrowing away the shown entry: absent, so the caller falls back.
    (should-not (le::cci-prompt2--entry-position narrowed theirs))
    (should-not (le::cci-prompt2--entry-position widened nil))))

(ert-deftest le::cci-prompt2-paste-test-entry-position-ignores-id ()
  "An org heading and its CLI twin are the same prompt despite differing ids."
  (let ((org-entry (list :id "[2026-08-09 Sun 00:44:12]" :state "COMMITTED"
                         :text "same" :ts 300.0 :session "a"))
        (cli-entry (le::cci-prompt2-paste-test--entry "a" 300.0 "same")))
    (should (equal (le::cci-prompt2--entry-position (list org-entry) cli-entry) 0))))

(provide 'le-cci-prompt2-paste-test)
;;; le-cci-prompt2-paste-test.el ends here
