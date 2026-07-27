;;; le-denote-test.el --- Tests for le-denote's subtree exporter  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for the functional core of `le::denote-export-subtree': region ->
;; root spans, the multi-root plan fold, and the two identity guards.  Nothing
;; here touches a denote silo or creates a file -- the buffer-based helpers run
;; in a temp Org buffer, the rest are pure.  Run in batch:
;;
;;   emacs -Q -batch -L . -L <elpaca-builds>/denote \
;;     -l le-denote.el -l le-denote-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'le-denote)

(defmacro le-denote-test--with-org (text &rest body)
  "Evaluate BODY in an Org temp buffer holding TEXT, point at `point-min'."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (delay-mode-hooks (org-mode))
     (insert ,text)
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

(ert-deftest le::denote-export--region-roots/expands-outward-to-shallowest ()
  "Reaching a shallower heading lifts the walk to that level, not below it."
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


;;;; le::denote-export--non-task-roots

(ert-deftest le::denote-export--non-task-roots/flags-container-heading ()
  "A root without a TODO state is reported by its headline."
  (le-denote-test--with-org
      "* 2026-07-26\n** TODO foo=1 alpha\n"
    (should (equal (le::denote-export--non-task-roots
                    (le::denote-export--region-roots (point-min) (point-max)))
                   '("2026-07-26")))))

(ert-deftest le::denote-export--non-task-roots/task-roots-pass ()
  "Roots that all carry a TODO state produce no complaint."
  (le-denote-test--with-org
      "* TODO foo=1 alpha\n* TODO foo=2 beta\n"
    (should-not (le::denote-export--non-task-roots
                 (le::denote-export--region-roots (point-min) (point-max))))))


;;;; le::denote-export--plan

(ert-deftest le::denote-export--plan/nodes-across-roots-in-order ()
  "Nodes from every root are folded into one list, in buffer order."
  (le-denote-test--with-org
      "* TODO foo=1 alpha\n** TODO foo=1=1 beta\n* TODO foo=2 gamma\n"
    (let ((plan (le::denote-export--plan
                 (le::denote-export--region-roots (point-min) (point-max)))))
      (should (equal (mapcar (lambda (n) (plist-get n :code)) (plist-get plan :nodes))
                     '("foo=1" "foo=1=1" "foo=2")))
      (should-not (plist-get plan :uncoded))
      (should-not (plist-get plan :duplicates)))))

(ert-deftest le::denote-export--plan/uncoded-in-any-root-suppresses-nodes ()
  "An uncoded node in the second root voids the whole plan's :nodes."
  (le-denote-test--with-org
      "* TODO foo=1 alpha\n* TODO refactor parser\n"
    (let ((plan (le::denote-export--plan
                 (le::denote-export--region-roots (point-min) (point-max)))))
      (should (equal (plist-get plan :uncoded) '("refactor parser")))
      (should-not (plist-get plan :nodes)))))

(ert-deftest le::denote-export--plan/duplicate-code-across-roots ()
  "One code=path claimed by two roots is reported in :duplicates."
  (le-denote-test--with-org
      "* TODO foo=1 alpha\n* TODO foo=1 alpha again\n"
    (let ((plan (le::denote-export--plan
                 (le::denote-export--region-roots (point-min) (point-max)))))
      (should (equal (plist-get plan :duplicates) '("foo=1"))))))

(ert-deftest le::denote-export--plan/regions-are-the-roots ()
  ":regions echoes the spans it was handed."
  (le-denote-test--with-org
      "* TODO foo=1 alpha\n* TODO foo=2 beta\n"
    (let ((roots (le::denote-export--region-roots (point-min) (point-max))))
      (should (equal (plist-get (le::denote-export--plan roots) :regions) roots)))))


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

(provide 'le-denote-test)
;;; le-denote-test.el ends here
