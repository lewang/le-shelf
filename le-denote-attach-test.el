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

;;; Rewriting --------------------------------------------------------------

(defun le::denote-attach-test--rewrite (text basename &optional file new-path)
  "Rewrite TEXT with fixed conversion context; FILE defaults to an Org file."
  (le::denote-attach--rewrite
   text basename
   (list :id "20260101T000000"
         :new-path (or new-path "/pg/denote/taskdata/20260101T000000--screenshot.png")
         :file (or file "/pg/denote/journal/j.org")
         :playground "/pg/")))

(ert-deftest le::denote-attach--rewrite/bare-attachment-link ()
  (should (equal (le::denote-attach-test--rewrite "see [[attachment:a.png]] here" "a.png")
                 (cons "see [[denote:20260101T000000]] here" 1))))

(ert-deftest le::denote-attach--rewrite/described-link-keeps-description ()
  "The description is the reader's label for the asset, so it survives."
  (should (equal (le::denote-attach-test--rewrite
                  "see [[attachment:a.png][the chart]]" "a.png")
                 (cons "see [[denote:20260101T000000][the chart]]" 1))))

(ert-deftest le::denote-attach--rewrite/repeated-occurrences ()
  (should (equal (le::denote-attach-test--rewrite
                  "[[attachment:a.png]] and [[attachment:a.png][x]]" "a.png")
                 (cons "[[denote:20260101T000000]] and [[denote:20260101T000000][x]]" 2))))

(ert-deftest le::denote-attach--rewrite/no-match-is-unchanged ()
  (let ((text "nothing to see"))
    (should (equal (le::denote-attach-test--rewrite text "a.png") (cons text 0)))))

(ert-deftest le::denote-attach--rewrite/substring-name-is-not-caught ()
  "A name that is a SUBSTRING of another's must never be rewritten by it."
  (should (equal (le::denote-attach-test--rewrite "[[attachment:ba.png]]" "a.png")
                 (cons "[[attachment:ba.png]]" 0)))
  (should (equal (le::denote-attach-test--rewrite "[[attachment:a.png.bak]]" "a.png")
                 (cons "[[attachment:a.png.bak]]" 0)))
  (should (equal (le::denote-attach-test--rewrite "prose about ba.png" "a.png")
                 (cons "prose about ba.png" 0))))

(ert-deftest le::denote-attach--rewrite/org-file-link-is-repaired ()
  "A hand-written `file:' link is the same displacement, so it is the same repair."
  (should (equal (le::denote-attach-test--rewrite "[[file:data/xx/yy/a.png]]" "a.png")
                 (cons "[[denote:20260101T000000]]" 1)))
  (should (equal (le::denote-attach-test--rewrite "[[file:data/yy/a.png][shot]]" "a.png")
                 (cons "[[denote:20260101T000000][shot]]" 1))))

(ert-deftest le::denote-attach--rewrite/prose-gains-a-described-link ()
  "The visible text is unchanged; only the reference is added."
  (should (equal (le::denote-attach-test--rewrite "we cropped a.png by hand" "a.png")
                 (cons "we cropped [[denote:20260101T000000][a.png]] by hand" 1))))

(ert-deftest le::denote-attach--rewrite/prose-in-markdown-uses-md-syntax ()
  (should (equal (le::denote-attach-test--rewrite
                  "we cropped a.png by hand" "a.png" "/pg/interviews/i.md")
                 (cons "we cropped [a.png](denote:20260101T000000) by hand" 1))))

(ert-deftest le::denote-attach--rewrite/bare-name-in-verbatim-is-left-alone ()
  "A link inside verbatim renders as its own markup, so it is not written."
  (should (equal (le::denote-attach-test--rewrite "see ~a.png~ there" "a.png")
                 (cons "see ~a.png~ there" 0)))
  (should (equal (le::denote-attach-test--rewrite
                  "see `a.png` there" "a.png" "/pg/interviews/i.md")
                 (cons "see `a.png` there" 0))))

(ert-deftest le::denote-attach--rewrite/bare-name-in-a-src-block-is-left-alone ()
  (should (equal (le::denote-attach-test--rewrite
                  "#+begin_src sh\nmv a.png /tmp\n#+end_src\n" "a.png")
                 (cons "#+begin_src sh\nmv a.png /tmp\n#+end_src\n" 0))))

;;; Paths -- the whole token is replaced, and verbatim does not matter ------

(ert-deftest le::denote-attach--rewrite/path-in-backticks-in-markdown ()
  "The motivating case: a path inside inline code in a .md file.
A path needs no link syntax, so the backticks are irrelevant -- and replacing
the WHOLE path is what makes it correct rather than stranded."
  (should (equal (le::denote-attach-test--rewrite
                  "(see `.le-playground/data/38/AB/a.png`): unreadable"
                  "a.png" "/pg/interviews/i.md")
                 (cons "(see `.le-playground/denote/taskdata/20260101T000000--screenshot.png`): unreadable"
                       1))))

(ert-deftest le::denote-attach--rewrite/path-keeps-an-absolute-style ()
  (should (equal (le::denote-attach-test--rewrite "at /pg/data/38/AB/a.png now" "a.png")
                 (cons "at /pg/denote/taskdata/20260101T000000--screenshot.png now" 1))))

(ert-deftest le::denote-attach--rewrite/relative-path-resolves-against-its-file ()
  "A bare relative path can only mean relative to the file that wrote it."
  (should (equal (le::denote-attach-test--rewrite "at data/38/AB/a.png now" "a.png")
                 (cons "at ../taskdata/20260101T000000--screenshot.png now" 1))))

(ert-deftest le::denote-attach--rewrite/markdown-image-target-becomes-a-path ()
  "An image target must stay a path, or the `!' embed would be destroyed."
  (should (equal (le::denote-attach-test--rewrite
                  "![chart](a.png)" "a.png" "/pg/denote/journal/i.md")
                 (cons "![chart](../taskdata/20260101T000000--screenshot.png)" 1))))

;;; Occurrence classification ---------------------------------------------

(defun le::denote-attach-test--kinds (text basename &optional file)
  "The kinds of every occurrence of BASENAME in TEXT, in order."
  (mapcar (lambda (o) (plist-get o :kind))
          (le::denote-attach--occurrences
           text basename (or file "/pg/denote/journal/j.org"))))

(ert-deftest le::denote-attach--occurrences/classifies-each-form ()
  (should (equal (le::denote-attach-test--kinds
                  "[[attachment:a.png]]\n[[file:d/a.png]]\nprose a.png\nd/a.png\n~a.png~\n"
                  "a.png")
                 '(attachment file-link prose path manual))))

(ert-deftest le::denote-attach--occurrences/reports-line-numbers ()
  (should (equal (mapcar (lambda (o) (plist-get o :line))
                         (le::denote-attach--occurrences
                          "one\ntwo [[attachment:a.png]]\nthree\nprose a.png\n"
                          "a.png" "/pg/denote/journal/j.org"))
                 '(2 4))))

(ert-deftest le::denote-attach--occurrences/a-link-is-counted-once ()
  "The file name inside a link is not also a bare mention of it."
  (should (equal (le::denote-attach-test--kinds "[[attachment:a.png]]" "a.png")
                 '(attachment))))

(ert-deftest le::denote-attach--occurrences/unsupported-type-yields-manual ()
  (let ((occurrences (le::denote-attach--occurrences
                      "prose a.png here" "a.png" "/pg/notes.rst")))
    (should (equal (mapcar (lambda (o) (plist-get o :kind)) occurrences) '(manual)))
    (should (eq (plist-get (car occurrences) :why) :unsupported))))

(ert-deftest le::denote-attach--occurrences/verbatim-yields-manual-with-reason ()
  (let ((occurrences (le::denote-attach--occurrences
                      "see ~a.png~" "a.png" "/pg/denote/journal/j.org")))
    (should (eq (plist-get (car occurrences) :why) :verbatim))))

(ert-deftest le::denote-attach--occurrences/a-path-outranks-verbatim ()
  "A path in verbatim is still repairable: it needs no link syntax."
  (should (equal (le::denote-attach-test--kinds "see ~d/a.png~" "a.png") '(path))))

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

(defun le::denote-attach-test--edits (&rest files)
  "An edits alist claiming each of FILES carries an attachment link."
  (mapcar (lambda (file) (cons file (list (list :kind 'attachment :line 1)))) files))

(ert-deftest le::denote-attach--status/working-when-the-citer-sits-in-the-parent ()
  (should (eq (le::denote-attach--status
               le::denote-attach-test--asset
               (le::denote-attach-test--edits "/pg/denote/journal/j.org"))
              'working)))

(ert-deftest le::denote-attach--status/broken-when-the-citer-sits-elsewhere ()
  "org-attach resolves `data/' relative to the CITING file's directory, so a link
from one directory away is already dead -- converting it is a repair."
  (should (eq (le::denote-attach--status
               le::denote-attach-test--asset
               (le::denote-attach-test--edits "/pg/interviews/i.md"))
              'broken)))

(ert-deftest le::denote-attach--status/broken-when-any-citer-is-displaced ()
  (should (eq (le::denote-attach--status
               le::denote-attach-test--asset
               (le::denote-attach-test--edits "/pg/denote/journal/j.org"
                                              "/pg/interviews/i.md"))
              'broken)))

(ert-deftest le::denote-attach--status/prose-only-without-attachment-links ()
  "Only an `attachment:' link speaks to whether org-attach still resolves; a
prose or path reference says nothing about it."
  (should (eq (le::denote-attach--status le::denote-attach-test--asset nil)
              'prose-only))
  (should (eq (le::denote-attach--status
               le::denote-attach-test--asset
               '(("/pg/denote/journal/j.org" . ((:kind path :line 1)))))
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

(ert-deftest le::denote-attach--plan/prose-only-asset-is-converted-and-repaired ()
  "A prose mention keeps an asset alive -- and can now carry a denote link too."
  (let* ((asset (le::denote-attach-test--asset
                 "/pg/denote/journal/data/38/AB/talked-about.png"
                 "/pg/denote/journal/data"))
         (plan (le::denote-attach-test--plan
                (list asset)
                '(("/pg/interviews/i.md" . "we cropped talked-about.png by hand"))))
         (converted (car (plist-get plan :convert))))
    (should converted)
    (should (eq (plist-get converted :status) 'prose-only))
    (should (equal (mapcar (lambda (o) (plist-get o :kind))
                           (cdr (car (plist-get converted :edits))))
                   '(prose)))
    (should-not (le::denote-attach--manual-fixes plan))
    (should (equal (car (car (plist-get plan :file-edits))) "/pg/interviews/i.md"))))

(ert-deftest le::denote-attach--plan/only-a-bare-name-in-verbatim-needs-a-human ()
  (let* ((asset (le::denote-attach-test--asset
                 "/pg/denote/journal/data/38/AB/quoted.png" "/pg/denote/journal/data"))
         (plan (le::denote-attach-test--plan
                (list asset)
                '(("/pg/denote/journal/j.org" . "the file ~quoted.png~ is stale")))))
    (should (plist-get plan :convert))
    (should-not (plist-get plan :file-edits))
    (should (equal (mapcar (lambda (f) (plist-get f :file))
                           (le::denote-attach--manual-fixes plan))
                   '("/pg/denote/journal/j.org")))))

(ert-deftest le::denote-attach--plan/link-and-hand-written-path-both-repaired ()
  "Both forms are the same displacement, so both are fixed in one pass."
  (let* ((asset (le::denote-attach-test--asset
                 "/pg/denote/journal/data/38/AB/both.png" "/pg/denote/journal/data"))
         (plan (le::denote-attach-test--plan
                (list asset)
                '(("/pg/denote/journal/j.org"
                   . "[[attachment:both.png]]\nand [[file:data/38/AB/both.png]]\n"))))
         (converted (car (plist-get plan :convert))))
    (should (eq (plist-get converted :status) 'working))
    (should (equal (mapcar (lambda (o) (plist-get o :kind))
                           (cdr (car (plist-get converted :edits))))
                   '(attachment file-link)))
    (should-not (plist-get converted :manual))))

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
    (should (equal (mapcar (lambda (s) (plist-get s :basename)) (cdr (car edits)))
                   '("clipboard-20260101T000000.png" "clipboard-20260202T000000.png")))
    (should (equal (mapcar (lambda (s) (plist-get s :id)) (cdr (car edits)))
                   '("20260101T000000" "20260202T000000")))))

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
