;;; le-denote-test.el --- Tests for le-denote's settled-tree sweep  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for the functional core of `le::denote-export-settled': region ->
;; level-1 tree spans, the per-tree judgement fold, and the two identity guards.
;; Nothing here touches a denote silo or creates a file -- the buffer-based
;; helpers run in a temp Org buffer, the rest are pure.  Run in batch:
;;
;;   emacs -Q -batch -L "$PWD" -L <elpaca-builds>/denote \
;;     -l "$PWD/le-denote.el" -l "$PWD/le-denote-test.el" \
;;     -f ert-run-tests-batch-and-exit
;;
;; The paths must be absolute: `-l le-denote.el' is resolved against `load-path'
;; rather than the working directory, so the relative form fails to load.

;;; Code:

(require 'ert)
(require 'le-denote)

(defmacro le-denote-test--with-org (text &rest body)
  "Evaluate BODY in an Org temp buffer holding TEXT, point at `point-min'.
The buffer declares the silo's keyword set, so `org-done-keywords' holds both
DONE and CANCELLED.  Under `emacs -Q' it holds DONE alone and CANCELLED is not a
keyword at all, which would read a CANCELLED heading as a non-task."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (delay-mode-hooks (org-mode))
     (insert "#+TODO: REFINING REFINE-INTERVIEWING TODO DOING VERIFYING PR WAITING HOLD | DONE CANCELLED\n")
     (insert ,text)
     (org-set-regexps-and-options)
     (goto-char (point-min))
     ,@body))

(defun le-denote-test--bol (needle)
  "Return the beginning of the first line in the buffer containing NEEDLE."
  (save-excursion
    (goto-char (point-min))
    (search-forward needle)
    (line-beginning-position)))

(defun le-denote-test--root-headings (roots)
  "Return the headline text at the start of each span in ROOTS."
  (mapcar (lambda (root)
            (save-excursion
              (goto-char (car root))
              (org-get-heading t t t t)))
          roots))

(defun le-denote-test--trees (&optional claimed)
  "Return the plan's trees for the whole buffer, judged against CLAIMED."
  (plist-get (le::denote-export--plan
              (le::denote-export--region-roots (point-min) (point-max))
              claimed)
             :trees))

(defun le-denote-test--codes (trees)
  "Return the code=path of every node in TREES, in order."
  (mapcar (lambda (node) (plist-get node :code))
          (le::denote-export--nodes trees)))


;;;; le::denote-export--region-roots

(ert-deftest le::denote-export--region-roots/sibling-subtrees ()
  "A region spanning two siblings yields one span each, in buffer order."
  (le-denote-test--with-org
      "* TODO foo=1 alpha\nbody\n* TODO foo=2 beta\nbody\n* TODO foo=3 gamma\n"
    (should (equal (le-denote-test--root-headings
                    (le::denote-export--region-roots
                     (le-denote-test--bol "foo=1")
                     (le-denote-test--bol "foo=3")))
                   '("foo=1 alpha" "foo=2 beta")))))

(ert-deftest le::denote-export--region-roots/ends-at-heading-bol ()
  "A region ending exactly on a heading's bol excludes that heading."
  (le-denote-test--with-org
      "* TODO foo=1 alpha\nbody\n* TODO foo=2 beta\n"
    (should (equal (le-denote-test--root-headings
                    (le::denote-export--region-roots
                     (le-denote-test--bol "foo=1")
                     (le-denote-test--bol "foo=2")))
                   '("foo=1 alpha")))))

(ert-deftest le::denote-export--region-roots/whole-subtree-from-body ()
  "A region opening inside a body expands back to the containing heading."
  (le-denote-test--with-org
      "* TODO foo=1 alpha\nmid body\n** TODO foo=1=1 beta\n"
    (should (equal (le-denote-test--root-headings
                    (le::denote-export--region-roots
                     (le-denote-test--bol "mid body")
                     (point-max)))
                   '("foo=1 alpha")))))

(ert-deftest le::denote-export--region-roots/level-1-ancestor ()
  "A region wholly inside a child yields that child's level-1 ancestor.
Level 1 is the only root level, so a finished child is never taken out of a
live parent."
  (le-denote-test--with-org
      "* DOING foo=1 alpha\n** DONE foo=1=1 beta\n* TODO foo=2 gamma\n"
    (should (equal (le-denote-test--root-headings
                    (le::denote-export--region-roots
                     (le-denote-test--bol "foo=1=1")
                     (le-denote-test--bol "foo=2")))
                   '("foo=1 alpha")))))

(ert-deftest le::denote-export--region-roots/reaches-past-a-child ()
  "A region opening in a child and running on takes both level-1 trees."
  (le-denote-test--with-org
      "* TODO foo=1 alpha\n** TODO foo=1=1 beta\n* TODO foo=2 gamma\n"
    (should (equal (le-denote-test--root-headings
                    (le::denote-export--region-roots
                     (le-denote-test--bol "foo=1=1")
                     (point-max)))
                   '("foo=1 alpha" "foo=2 gamma")))))

(ert-deftest le::denote-export--region-roots/single-heading ()
  "A region inside one heading yields that subtree, descendants included."
  (le-denote-test--with-org
      "* TODO foo=1 alpha\n** TODO foo=1=1 beta\n* TODO foo=2 gamma\n"
    (let ((roots (le::denote-export--region-roots
                  (le-denote-test--bol "foo=1 alpha")
                  (le-denote-test--bol "foo=2"))))
      (should (equal (le-denote-test--root-headings roots) '("foo=1 alpha")))
      ;; The span reaches the child and stops short of the next root.
      (should (string-match-p
               "foo=1=1"
               (buffer-substring-no-properties (car (car roots)) (cdr (car roots)))))
      (should (< (cdr (car roots)) (le-denote-test--bol "foo=2"))))))

(ert-deftest le::denote-export--region-roots/spans-are-disjoint ()
  "Root spans are ordered and never overlap.
`org-end-of-subtree' stops at the end of a subtree's last line, so a span ends
one newline short of the next root -- which is why deleting a span leaves an
empty line behind."
  (le-denote-test--with-org
      "* TODO foo=1 alpha\n** TODO foo=1=1 beta\n* TODO foo=2 gamma\n"
    (let ((roots (le::denote-export--region-roots (point-min) (point-max))))
      (should (= (length roots) 2))
      (should (< (cdr (nth 0 roots)) (car (nth 1 roots))))
      (should (equal (buffer-substring-no-properties
                      (cdr (nth 0 roots)) (car (nth 1 roots)))
                     "\n")))))


;;;; le::denote-export--plan

(ert-deftest le::denote-export--plan/nodes-in-buffer-order ()
  "Settled trees carry no skip reason, and their nodes come in buffer order."
  (le-denote-test--with-org
      "* DONE foo=1 alpha\n** DONE foo=1=1 beta\n* DONE foo=2 gamma\n"
    (let ((trees (le-denote-test--trees)))
      (should (equal (mapcar (lambda (tree) (plist-get tree :skip)) trees) '(nil nil)))
      (should (equal (le-denote-test--codes trees) '("foo=1" "foo=1=1" "foo=2"))))))

(ert-deftest le::denote-export--plan/regions-are-the-roots ()
  "Each tree carries the span it was judged from."
  (le-denote-test--with-org
      "* DONE foo=1 alpha\n* DONE foo=2 beta\n"
    (let ((roots (le::denote-export--region-roots (point-min) (point-max))))
      (should (equal (mapcar (lambda (tree) (plist-get tree :region))
                             (plist-get (le::denote-export--plan roots nil) :trees))
                     roots)))))

(ert-deftest le::denote-export--plan/skips-non-task-root ()
  "A root with no TODO state is skipped as :not-task, which outranks :open."
  (le-denote-test--with-org
      "* 2026-07-26\n** DOING foo=1 alpha\n"
    (let ((tree (car (le-denote-test--trees))))
      (should (eq (plist-get tree :skip) :not-task))
      (should (equal (plist-get tree :headline) "2026-07-26")))))

(ert-deftest le::denote-export--plan/skips-open-tree ()
  "A tree holding an unclosed task heading is skipped as :open, naming it."
  (le-denote-test--with-org
      "* DONE foo=1 alpha\n** DOING foo=1=1 beta\n"
    (let ((tree (car (le-denote-test--trees))))
      (should (eq (plist-get tree :skip) :open))
      (should (equal (plist-get tree :detail) '("DOING foo=1=1 beta"))))))

(ert-deftest le::denote-export--plan/skips-uncoded-tree-alone ()
  "An uncoded task heading skips its own tree and lets its sibling through."
  (le-denote-test--with-org
      "* DONE foo=1 alpha\n* DONE refactor parser\n"
    (let ((trees (le-denote-test--trees)))
      (should-not (plist-get (nth 0 trees) :skip))
      (should (eq (plist-get (nth 1 trees) :skip) :uncoded))
      (should (equal (plist-get (nth 1 trees) :detail) '("refactor parser"))))))

(ert-deftest le::denote-export--plan/skips-both-duplicate-claimants ()
  "A code=path claimed by two trees skips both of them."
  (le-denote-test--with-org
      "* DONE foo=1 alpha\n* DONE foo=1 alpha again\n"
    (let ((trees (le-denote-test--trees)))
      (should (equal (mapcar (lambda (tree) (plist-get tree :skip)) trees)
                     '(:duplicate :duplicate)))
      (should (equal (plist-get (nth 0 trees) :detail) '("foo=1"))))))

(ert-deftest le::denote-export--plan/skips-colliding-tree-alone ()
  "A code=path already filed skips only the tree claiming it."
  (le-denote-test--with-org
      "* DONE foo=1 alpha\n* DONE foo=2 beta\n"
    (let ((trees (le-denote-test--trees
                  '(("foo=1" . "20260719T190750==foo=1--alpha.org")))))
      (should (eq (plist-get (nth 0 trees) :skip) :collision))
      (should (equal (plist-get (nth 0 trees) :detail)
                     '("foo=1 -> 20260719T190750==foo=1--alpha.org")))
      (should-not (plist-get (nth 1 trees) :skip)))))

(ert-deftest le::denote-export--plan/reason-precedence ()
  "An open tree that is also uncoded reports :open -- the first reason wins."
  (le-denote-test--with-org
      "* DOING foo=1 alpha\n** DONE refactor parser\n"
    (should (eq (plist-get (car (le-denote-test--trees)) :skip) :open))))

(ert-deftest le::denote-export--plan/open-tree-does-not-poison-duplicates ()
  "A skipped tree's codes are not claims, so a settled twin still files."
  (le-denote-test--with-org
      "* DOING foo=1 alpha\n* DONE foo=1 twin\n"
    (let ((trees (le-denote-test--trees)))
      (should (eq (plist-get (nth 0 trees) :skip) :open))
      (should-not (plist-get (nth 1 trees) :skip)))))

(ert-deftest le::denote-export--plan/cancelled-tree-is-settled ()
  "CANCELLED is in `org-done-keywords' here, so a cancelled tree files."
  (le-denote-test--with-org
      "* CANCELLED foo=1 alpha\n"
    (should-not (plist-get (car (le-denote-test--trees)) :skip))))

(ert-deftest le::denote-export--plan/content-only-for-survivors ()
  "Content is extracted for the trees that will be filed, and for no others."
  (le-denote-test--with-org
      "* DONE foo=1 alpha\nbody\n* DOING foo=2 beta\n"
    (let* ((trees (le-denote-test--trees))
           (filed (car (le::denote-export--nodes
                        (le::denote-export--filed trees))))
           (skipped (car (plist-get (nth 1 trees) :nodes))))
      (should (equal (plist-get filed :content) "* DONE foo=1 alpha\nbody\n"))
      (should-not (plist-get skipped :content)))))


;;;; le::denote-export--report

(ert-deftest le::denote-export--report/lists-the-files-to-be-written ()
  "A filed tree is counted in the summary and gets a preview row."
  (le-denote-test--with-org
      "* DONE foo=1 alpha\n"
    (let ((report (le::denote-export--report
                   (le-denote-test--trees) "/tmp/task/" "20260727T000000")))
      (should (string-match-p "1 tree(s) seen; 1 to file, 1 task node(s)" report))
      (should (string-match-p "foo=1 +-> +20260727T000000" report)))))

(ert-deftest le::denote-export--report/names-the-root-once ()
  "A tree tripped by its own root reads once, with its state; children list."
  (le-denote-test--with-org
      "* DOING foo=1 alpha\n** WAITING foo=1=1 beta\n"
    (let ((report (le::denote-export--report
                   (le-denote-test--trees) "/tmp/task/" "20260727T000000")))
      (should (string-match-p "^  DOING foo=1 alpha$" report))
      (should-not (string-match-p "^      DOING foo=1 alpha$" report))
      (should (string-match-p "^      WAITING foo=1=1 beta$" report)))))

(ert-deftest le::denote-export--report/counts-not-task-without-listing-it ()
  "A non-task tree is counted in the summary and named nowhere below it."
  (le-denote-test--with-org
      "* 2026-07-26\nprose\n* DOING foo=1 alpha\n"
    (let ((report (le::denote-export--report
                   (le-denote-test--trees) "/tmp/task/" "20260727T000000")))
      (should (string-match-p "0 to file" report))
      (should (string-match-p "1 skipped: not a task" report))
      (should (string-match-p "1 skipped: not settled" report))
      (should (string-match-p "skipped -- not settled:" report))
      (should-not (string-match-p "2026-07-26" report)))))


;;;; le::denote-export--duplicate-codes

(ert-deftest le::denote-export--duplicate-codes/none ()
  "Distinct codes yield no duplicates."
  (should-not (le::denote-export--duplicate-codes
               '((:code "foo=1") (:code "foo=2")))))

(ert-deftest le::denote-export--duplicate-codes/reported-once ()
  "A code claimed three times is still reported once."
  (should (equal (le::denote-export--duplicate-codes
                  '((:code "foo=1") (:code "foo=2") (:code "foo=1") (:code "foo=1")))
                 '("foo=1"))))

(ert-deftest le::denote-export--duplicate-codes/order-of-first-repeat ()
  "Duplicates come back in the order their repeat was seen."
  (should (equal (le::denote-export--duplicate-codes
                  '((:code "a=1") (:code "b=1") (:code "b=1") (:code "a=1")))
                 '("b=1" "a=1"))))


;;;; le::denote-export--collisions

(ert-deftest le::denote-export--collisions/hit ()
  "A code already filed comes back paired with the existing file name."
  (should (equal (le::denote-export--collisions
                  '((:code "foo=1") (:code "foo=2"))
                  '(("foo=1" . "20260719T190750==foo=1--alpha.org")))
                 '(("foo=1" . "20260719T190750==foo=1--alpha.org")))))

(ert-deftest le::denote-export--collisions/miss ()
  "Codes absent from the claimed set produce nothing."
  (should-not (le::denote-export--collisions
               '((:code "foo=3"))
               '(("foo=1" . "20260719T190750==foo=1--alpha.org")))))

(ert-deftest le::denote-export--collisions/several ()
  "Every colliding node is reported, in node order."
  (should (equal (le::denote-export--collisions
                  '((:code "foo=1") (:code "foo=2") (:code "foo=3"))
                  '(("foo=3" . "c.org") ("foo=1" . "a.org")))
                 '(("foo=1" . "a.org") ("foo=3" . "c.org")))))

(ert-deftest le::denote-export--collisions/empty-claim-set ()
  "An empty task directory collides with nothing."
  (should-not (le::denote-export--collisions '((:code "foo=1")) nil)))


;;;; le::denote-export--parse-code

(ert-deftest le::denote-export--parse-code/code-and-description ()
  "A coded headline splits into its code=path and the remaining text."
  (should (equal (le::denote-export--parse-code "foo=1=2 do the thing")
                 '("foo=1=2" . "do the thing"))))

(ert-deftest le::denote-export--parse-code/bare-code ()
  "A headline that is only a code=path has an empty description."
  (should (equal (le::denote-export--parse-code "foo=1") '("foo=1" . ""))))

(ert-deftest le::denote-export--parse-code/uncoded ()
  "A headline without a code=path returns nil."
  (should-not (le::denote-export--parse-code "refactor parser"))
  (should-not (le::denote-export--parse-code "foo-bar=1 dashes are out")))


;;;; le::denote-export--promote

(ert-deftest le::denote-export--promote/shifts-uniformly ()
  "The top heading lands at level 1 and descendants keep their depth below it."
  (should (equal (le::denote-export--promote "*** TODO foo=1\n**** TODO foo=1=1\n")
                 "* TODO foo=1\n** TODO foo=1=1\n")))

(ert-deftest le::denote-export--promote/already-level-1 ()
  "A subtree already at level 1 is left where it is."
  (should (equal (le::denote-export--promote "* TODO foo=1\n** child\n")
                 "* TODO foo=1\n** child\n")))

(ert-deftest le::denote-export--promote/collapses-blank-runs ()
  "The gaps left by cut children collapse to a single blank line."
  (should (equal (le::denote-export--promote "* TODO foo=1\nbody\n\n\n\nmore\n")
                 "* TODO foo=1\nbody\n\nmore\n")))

;;; Heading regexps: the keyword is org's own regexp-opt, never a character class

(ert-deftest le::denote--heading-regexp/matches-a-dashed-keyword ()
  "A keyword containing a dash anchors a heading; `[A-Z]+' stopped at the dash."
  (le-denote-test--with-org
      "* REFINE-INTERVIEWING foo=1 alpha\n"
    (should (re-search-forward (le::denote--heading-regexp "foo=1") nil t))))

(ert-deftest le::denote--heading-regexp/matches-every-declared-keyword ()
  "Every keyword the buffer declares anchors a heading."
  (dolist (kw '("REFINING" "REFINE-INTERVIEWING" "TODO" "DOING" "VERIFYING"
                "PR" "WAITING" "HOLD" "DONE" "CANCELLED"))
    (le-denote-test--with-org
        (format "* %s foo=1 alpha\n" kw)
      (should (re-search-forward (le::denote--heading-regexp "foo=1") nil t)))))

(ert-deftest le::denote--heading-regexp/rejects-a-non-keyword ()
  "A first word that is not a declared keyword is not a task heading."
  (le-denote-test--with-org
      "* NOTASTATE foo=1 alpha\n"
    (should-not (re-search-forward (le::denote--heading-regexp "foo=1") nil t))))

(ert-deftest le::denote--heading-regexp/code-ends-at-a-boundary ()
  "A longer id that merely starts with CODE does not match."
  (le-denote-test--with-org
      "* TODO foo=1=2 alpha\n"
    (should-not (re-search-forward (le::denote--heading-regexp "foo=1") nil t))))

(ert-deftest le::denote--state-regexp/group-1-is-the-dashed-keyword ()
  "Group 1 is the keyword itself -- what a state flip replaces in place."
  (le-denote-test--with-org
      "* REFINE-INTERVIEWING foo=1 alpha\n"
    (goto-char (le-denote-test--bol "foo=1"))
    (should (looking-at (le::denote--state-regexp)))
    (should (equal (match-string 1) "REFINE-INTERVIEWING"))))

(provide 'le-denote-test)
;;; le-denote-test.el ends here
