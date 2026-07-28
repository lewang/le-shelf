;;; le-denote-attach-test.el --- Tests for le-denote-attach's functional core  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for the pure core of `le::denote-migrate-attachments': identifier
;; derivation, link rewriting, mention detection, path computation, and the
;; convert/sweep/ambiguous partition.  Nothing here touches a filesystem -- the
;; partition takes the playground's text as already-read (FILE . TEXT) pairs, so
;; every decision is a function of strings.  Run in batch:
;;
;;   emacs -Q -batch -L "$PWD" -L <elpaca-builds>/denote \
;;     -l "$PWD/le-denote-attach.el" -l "$PWD/le-denote-attach-test.el" \
;;     -f ert-run-tests-batch-and-exit
;;
;; The paths must be absolute: `-l le-denote-attach.el' is resolved against
;; `load-path' rather than the working directory, so the relative form fails.
;;
;; Several fixtures cover cases the motivating playground does NOT exercise --
;; one file name under two trees, a name carrying no timestamp, a taken
;; identifier -- because there every asset is a uniquely-timestamped
;; `clipboard-*.png'.  Those paths exist only here.

;;; Code:

(require 'ert)
(require 'le-denote-attach)

;;; Identifiers ------------------------------------------------------------

(ert-deftest le::denote-attach--name-identifier/clipboard-name ()
  (should (equal (le::denote-attach--name-identifier "clipboard-20260703T134035.png")
                 "20260703T134035")))

(ert-deftest le::denote-attach--name-identifier/timestamp-mid-name ()
  (should (equal (le::denote-attach--name-identifier "shot-20260703T134035-cropped.png")
                 "20260703T134035")))

(ert-deftest le::denote-attach--name-identifier/no-timestamp-is-nil ()
  (should-not (le::denote-attach--name-identifier "diagram.png"))
  (should-not (le::denote-attach--name-identifier "20260703.png")))

(ert-deftest le::denote-attach--bump-identifier/adds-one-second ()
  (should (equal (le::denote-attach--bump-identifier "20260703T134035") "20260703T134036")))

(ert-deftest le::denote-attach--bump-identifier/rolls-over ()
  (should (equal (le::denote-attach--bump-identifier "20260703T135959") "20260703T140000"))
  (should (equal (le::denote-attach--bump-identifier "20261231T235959") "20270101T000000")))

(ert-deftest le::denote-attach--identifier/prefers-the-embedded-timestamp ()
  "Capture time beats migration time: the name already carries the better id."
  (should (equal (le::denote-attach--identifier
                  "clipboard-20260703T134035.png" "20260728T120000" nil)
                 "20260703T134035")))

(ert-deftest le::denote-attach--identifier/falls-back-when-name-has-none ()
  (should (equal (le::denote-attach--identifier "diagram.png" "20260728T120000" nil)
                 "20260728T120000")))

(ert-deftest le::denote-attach--identifier/bumps-past-a-taken-id ()
  (should (equal (le::denote-attach--identifier
                  "clipboard-20260703T134035.png" "20260728T120000"
                  '("20260703T134035" "20260703T134036"))
                 "20260703T134037")))

;;; Link rewriting ---------------------------------------------------------

(ert-deftest le::denote-attach--rewrite-text/bare-link ()
  (should (equal (le::denote-attach--rewrite-text
                  "see [[attachment:a.png]] here" "a.png" "20260101T000000")
                 (cons "see [[denote:20260101T000000]] here" 1))))

(ert-deftest le::denote-attach--rewrite-text/described-link-keeps-description ()
  "The description is the reader's label for the asset, so it survives."
  (should (equal (le::denote-attach--rewrite-text
                  "see [[attachment:a.png][the chart]]" "a.png" "20260101T000000")
                 (cons "see [[denote:20260101T000000][the chart]]" 1))))

(ert-deftest le::denote-attach--rewrite-text/repeated-occurrences ()
  (should (equal (le::denote-attach--rewrite-text
                  "[[attachment:a.png]] and [[attachment:a.png][x]]"
                  "a.png" "20260101T000000")
                 (cons "[[denote:20260101T000000]] and [[denote:20260101T000000][x]]" 2))))

(ert-deftest le::denote-attach--rewrite-text/no-match-is-unchanged ()
  (let ((text "nothing to see"))
    (should (equal (le::denote-attach--rewrite-text text "a.png" "20260101T000000")
                   (cons text 0)))))

(ert-deftest le::denote-attach--rewrite-text/substring-name-is-not-caught ()
  "A name that is a SUBSTRING of another's must not be rewritten by it.
The link delimiters are what make this safe, so assert it rather than assume it."
  (should (equal (le::denote-attach--rewrite-text
                  "[[attachment:ba.png]]" "a.png" "20260101T000000")
                 (cons "[[attachment:ba.png]]" 0)))
  (should (equal (le::denote-attach--rewrite-text
                  "[[attachment:a.png.bak]]" "a.png" "20260101T000000")
                 (cons "[[attachment:a.png.bak]]" 0))))

(ert-deftest le::denote-attach--rewrite-text/other-link-types-untouched ()
  "Only `attachment:' links are rewritten; a `file:' path is left for a human."
  (should (equal (le::denote-attach--rewrite-text
                  "[[file:data/xx/yy/a.png]]" "a.png" "20260101T000000")
                 (cons "[[file:data/xx/yy/a.png]]" 0))))

(ert-deftest le::denote-attach--link-occurrences/reports-line-numbers ()
  (should (equal (le::denote-attach--link-occurrences
                  "one\ntwo [[attachment:a.png]]\nthree\n[[attachment:a.png][d]]\n"
                  "a.png")
                 '((2 . "[[attachment:a.png]]") (4 . "[[attachment:a.png][d]]")))))

;;; Mentions the rewrite cannot repair ------------------------------------

(ert-deftest le::denote-attach--mention-occurrences/prose-only ()
  (should (equal (le::denote-attach--mention-occurrences
                  "we discussed a.png at length" "a.png")
                 '((1 . "we discussed a.png at length")))))

(ert-deftest le::denote-attach--mention-occurrences/a-link-is-not-a-mention ()
  "An `attachment:' link is rewritable, so it is not residue."
  (should-not (le::denote-attach--mention-occurrences
               "see [[attachment:a.png]]" "a.png")))

(ert-deftest le::denote-attach--mention-occurrences/link-and-prose-on-one-line ()
  "Blanking the link must leave the prose mention -- and its line number."
  (should (equal (le::denote-attach--mention-occurrences
                  "x\n[[attachment:a.png]] but also `data/xx/a.png`\n" "a.png")
                 '((2 . "but also `data/xx/a.png`")))))

(ert-deftest le::denote-attach--mention-occurrences/file-link-counts-as-residue ()
  (should (equal (le::denote-attach--mention-occurrences "[[file:data/x/a.png]]" "a.png")
                 '((1 . "[[file:data/x/a.png]]")))))

;;; Paths ------------------------------------------------------------------

(ert-deftest le::denote-attach--tree-parent/is-the-dir-links-resolve-from ()
  (should (equal (le::denote-attach--tree-parent "/pg/denote/journal/data")
                 "/pg/denote/journal"))
  (should (equal (le::denote-attach--tree-parent "/pg/denote/journal/data/")
                 "/pg/denote/journal")))

(ert-deftest le::denote-attach--sweep-path/mirrors-the-playground-path ()
  "Provenance survives, and two trees holding one file name cannot collide."
  (should (equal (le::denote-attach--sweep-path
                  "/pg/denote/_tobedeleted/" "/pg/"
                  "/pg/denote/journal/data/38/AB/clipboard-1.png")
                 "/pg/denote/_tobedeleted/denote/journal/data/38/AB/clipboard-1.png"))
  (should-not (equal (le::denote-attach--sweep-path
                      "/pg/denote/_tobedeleted/" "/pg/" "/pg/data/38/AB/same.png")
                     (le::denote-attach--sweep-path
                      "/pg/denote/_tobedeleted/" "/pg/"
                      "/pg/denote/journal/data/38/AB/same.png"))))

(ert-deftest le::denote-attach--new-path/is-a-denote-document-name ()
  (should (equal (le::denote-attach--new-path "/pg/denote/taskdata" "20260703T134035" ".png")
                 "/pg/denote/taskdata/20260703T134035--screenshot.png")))

;;; Status -----------------------------------------------------------------

(defconst le::denote-attach-test--asset
  '(:path "/pg/denote/journal/data/38/AB/clipboard-20260703T134035.png"
    :tree "/pg/denote/journal/data"
    :basename "clipboard-20260703T134035.png"
    :ext ".png"
    :fallback "20260728T120000")
  "An asset under the journal's own attachment tree.")

(ert-deftest le::denote-attach--status/working-when-the-citer-sits-in-the-parent ()
  (should (eq (le::denote-attach--status
               le::denote-attach-test--asset
               '(("/pg/denote/journal/j.org" . ((1 . "x")))))
              'working)))

(ert-deftest le::denote-attach--status/broken-when-the-citer-sits-elsewhere ()
  "org-attach resolves `data/' relative to the CITING file's directory, so a link
from one directory away is already dead -- converting it is a repair."
  (should (eq (le::denote-attach--status
               le::denote-attach-test--asset
               '(("/pg/interviews/i.md" . ((1 . "x")))))
              'broken)))

(ert-deftest le::denote-attach--status/broken-when-any-citer-is-displaced ()
  (should (eq (le::denote-attach--status
               le::denote-attach-test--asset
               '(("/pg/denote/journal/j.org" . ((1 . "x")))
                 ("/pg/interviews/i.md" . ((2 . "y")))))
              'broken)))

(ert-deftest le::denote-attach--status/prose-only-without-links ()
  (should (eq (le::denote-attach--status le::denote-attach-test--asset nil)
              'prose-only)))

;;; The partition ----------------------------------------------------------

(defun le::denote-attach-test--asset (path tree &optional fallback)
  "A test asset plist for PATH under TREE."
  (list :path path :tree tree
        :basename (file-name-nondirectory path)
        :ext (concat "." (file-name-extension path))
        :fallback (or fallback "20260728T120000")))

(defun le::denote-attach-test--plan (assets corpus &optional taken)
  "Run the partition over ASSETS and CORPUS with fixed directories."
  (le::denote-attach--plan assets corpus
                           "/pg/denote/taskdata/" "/pg/denote/_tobedeleted/" "/pg/"
                           taken))

(ert-deftest le::denote-attach--plan/partitions-convert-and-sweep ()
  (let* ((cited (le::denote-attach-test--asset
                 "/pg/denote/journal/data/38/AB/cited.png" "/pg/denote/journal/data"))
         (orphan (le::denote-attach-test--asset
                  "/pg/denote/journal/data/9C/DE/orphan.png" "/pg/denote/journal/data"))
         (plan (le::denote-attach-test--plan
                (list cited orphan)
                '(("/pg/denote/journal/j.org" . "see [[attachment:cited.png]]")))))
    (should (equal (mapcar (lambda (c) (plist-get c :basename))
                           (plist-get plan :convert))
                   '("cited.png")))
    (should (equal (mapcar (lambda (s) (plist-get s :basename))
                           (plist-get plan :sweep))
                   '("orphan.png")))
    (should-not (plist-get plan :ambiguous))))

(ert-deftest le::denote-attach--plan/sweep-path-is-under-the-sweep-root ()
  (let* ((orphan (le::denote-attach-test--asset
                  "/pg/data/9C/DE/orphan.png" "/pg/data"))
         (plan (le::denote-attach-test--plan (list orphan) nil)))
    (should (equal (plist-get (car (plist-get plan :sweep)) :new-path)
                   "/pg/denote/_tobedeleted/data/9C/DE/orphan.png"))))

(ert-deftest le::denote-attach--plan/ambiguous-name-is-acted-on-by-neither ()
  "A mention names only the file, so a name under two paths cannot be attributed."
  (let* ((a (le::denote-attach-test--asset "/pg/data/11/AA/same.png" "/pg/data"))
         (b (le::denote-attach-test--asset
             "/pg/denote/journal/data/22/BB/same.png" "/pg/denote/journal/data"))
         (plan (le::denote-attach-test--plan
                (list a b)
                '(("/pg/denote/journal/j.org" . "[[attachment:same.png]]")))))
    (should-not (plist-get plan :convert))
    (should-not (plist-get plan :sweep))
    (should-not (plist-get plan :file-edits))
    (should (equal (plist-get (car (plist-get plan :ambiguous)) :basename) "same.png"))
    (should (= 2 (length (plist-get (car (plist-get plan :ambiguous)) :paths))))))

(ert-deftest le::denote-attach--plan/prose-only-asset-is-converted-and-flagged ()
  "A prose mention keeps an asset alive, but no rewrite can follow it."
  (let* ((asset (le::denote-attach-test--asset
                 "/pg/denote/journal/data/38/AB/talked-about.png"
                 "/pg/denote/journal/data"))
         (plan (le::denote-attach-test--plan
                (list asset)
                '(("/pg/interviews/i.md" . "we cropped talked-about.png by hand"))))
         (converted (car (plist-get plan :convert))))
    (should converted)
    (should (eq (plist-get converted :status) 'prose-only))
    (should-not (plist-get converted :links))
    (should-not (plist-get plan :file-edits))
    (should (equal (mapcar (lambda (f) (plist-get f :file))
                           (le::denote-attach--manual-fixes plan))
                   '("/pg/interviews/i.md")))))

(ert-deftest le::denote-attach--plan/link-and-file-path-both-reported ()
  "An asset carrying a rewritable link AND a hand-written path is converted, with
the path left for a human."
  (let* ((asset (le::denote-attach-test--asset
                 "/pg/denote/journal/data/38/AB/both.png" "/pg/denote/journal/data"))
         (plan (le::denote-attach-test--plan
                (list asset)
                '(("/pg/denote/journal/j.org"
                   . "[[attachment:both.png]]\nand [[file:data/38/AB/both.png]]\n"))))
         (converted (car (plist-get plan :convert))))
    (should (eq (plist-get converted :status) 'working))
    (should (equal (plist-get converted :links)
                   '(("/pg/denote/journal/j.org" . ((1 . "[[attachment:both.png]]"))))))
    (should (equal (plist-get converted :mentions)
                   '(("/pg/denote/journal/j.org"
                      . ((2 . "and [[file:data/38/AB/both.png]]")))))) ))

(ert-deftest le::denote-attach--plan/file-edits-group-by-file ()
  "One journal citing two assets yields one entry carrying both substitutions."
  (let* ((a (le::denote-attach-test--asset
             "/pg/denote/journal/data/11/AA/clipboard-20260101T000000.png"
             "/pg/denote/journal/data"))
         (b (le::denote-attach-test--asset
             "/pg/denote/journal/data/22/BB/clipboard-20260202T000000.png"
             "/pg/denote/journal/data"))
         (plan (le::denote-attach-test--plan
                (list a b)
                '(("/pg/denote/journal/j.org"
                   . "[[attachment:clipboard-20260101T000000.png]]
[[attachment:clipboard-20260202T000000.png]]"))))
         (edits (plist-get plan :file-edits)))
    (should (= 1 (length edits)))
    (should (equal (car (car edits)) "/pg/denote/journal/j.org"))
    (should (equal (cdr (car edits))
                   '(("clipboard-20260101T000000.png" . "20260101T000000")
                     ("clipboard-20260202T000000.png" . "20260202T000000"))))))

(ert-deftest le::denote-attach--plan/identifiers-avoid-the-taken-set ()
  (let* ((asset (le::denote-attach-test--asset
                 "/pg/denote/journal/data/38/AB/clipboard-20260703T134035.png"
                 "/pg/denote/journal/data"))
         (plan (le::denote-attach-test--plan
                (list asset)
                '(("/pg/denote/journal/j.org"
                   . "[[attachment:clipboard-20260703T134035.png]]"))
                '("20260703T134035"))))
    (should (equal (plist-get (car (plist-get plan :convert)) :id) "20260703T134036"))))

(ert-deftest le::denote-attach--plan/two-assets-never-share-an-identifier ()
  "The taken set is threaded through the run, not just read from the silo."
  (let* ((a (le::denote-attach-test--asset "/pg/data/11/AA/one.png" "/pg/data"
                                           "20260728T120000"))
         (b (le::denote-attach-test--asset "/pg/data/22/BB/two.png" "/pg/data"
                                           "20260728T120000"))
         (plan (le::denote-attach-test--plan
                (list a b)
                '(("/pg/denote/journal/j.org"
                   . "[[attachment:one.png]] [[attachment:two.png]]"))))
         (ids (mapcar (lambda (c) (plist-get c :id)) (plist-get plan :convert))))
    (should (equal ids '("20260728T120000" "20260728T120001")))))

(provide 'le-denote-attach-test)
;;; le-denote-attach-test.el ends here
