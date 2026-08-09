;;; le-cci-session-ids-test.el --- Tests for per-instance CLI conversation ids  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for `le::cci-session-ids--add', the pure core that maintains the
;; per-instance list, and for `le::cci-session-id-record', the thin shell that
;; resolves an instance to its buffer and stores there.
;;
;; The cases that carry weight are the ones a scalar field would get wrong.  An
;; instance accumulates conversations: /clear mints a fresh uuid in the same
;; terminal and resume attaches an old one, so recording must APPEND rather
;; than replace, and must keep the newest at the head since that is the live
;; conversation.  Re-recording the same uuid -- what a compact fires -- must
;; not grow the list or disturb order.
;;
;; Run in batch:
;;
;;   emacs -Q -batch -L "$PWD" \
;;     -l "$PWD/le-cci-session-ids.el" -l "$PWD/le-cci-session-ids-test.el" \
;;     -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'le-cci-session-ids)

(defmacro le::cci-session-ids-test--with-instance (session-id buffer &rest body)
  "Run BODY with SESSION-ID resolving to an instance whose buffer is BUFFER.
Any other session id resolves to nothing.  BUFFER may be nil, standing for
an instance registered before its terminal was attached."
  (declare (indent 2) (debug (form form body)))
  `(cl-letf (((symbol-function 'claude-code-ide-mcp--get-session-by-id)
              (lambda (id) (when (equal id ,session-id) (list 'session ,buffer))))
             ((symbol-function 'claude-code-ide-mcp-session-buffer)
              (lambda (session) (nth 1 session))))
     ,@body))

(defmacro le::cci-session-ids-test--with-buffer (var &rest body)
  "Run BODY with VAR bound to a live throwaway buffer."
  (declare (indent 1) (debug (symbolp body)))
  `(let ((,var (generate-new-buffer "*cci: test*")))
     (unwind-protect (progn ,@body)
       (kill-buffer ,var))))

;;;; The pure core: an instance accumulates conversations

(ert-deftest le::cci-session-ids--add/newest-first ()
  "Each new conversation goes to the head, since the head is the live one."
  (should (equal (le::cci-session-ids--add nil "a") '("a")))
  (should (equal (le::cci-session-ids--add '("a") "b") '("b" "a")))
  (should (equal (le::cci-session-ids--add '("b" "a") "c") '("c" "b" "a"))))

(ert-deftest le::cci-session-ids--add/re-record-moves-to-front-without-growing ()
  "A compact re-declares the id it already had: no duplicate, no reordering below."
  (should (equal (le::cci-session-ids--add '("b" "a") "b") '("b" "a")))
  ;; Resuming an older conversation makes it current again.
  (should (equal (le::cci-session-ids--add '("c" "b" "a") "a") '("a" "c" "b"))))

(ert-deftest le::cci-session-ids--add/ignores-missing-id ()
  "A hook that could not read an id must not push nil onto the list."
  (should (equal (le::cci-session-ids--add '("a") nil) '("a")))
  (should (equal (le::cci-session-ids--add '("a") "") '("a")))
  (should (equal (le::cci-session-ids--add nil nil) nil)))

(ert-deftest le::cci-session-ids--add/does-not-mutate-input ()
  "The list lives in a buffer-local; rebuilding it must not edit the old value."
  (let* ((original (list "b" "a"))
         (copy (copy-sequence original)))
    (le::cci-session-ids--add original "a")
    (should (equal original copy))))

;;;; The shell: resolving an instance to the buffer that holds its list

(ert-deftest le::cci-session-id-record/stores-in-the-instance-buffer ()
  "The hook names an instance, not a buffer, so the id must land in its terminal."
  (le::cci-session-ids-test--with-buffer buf
                                         (le::cci-session-ids-test--with-instance "claude-proj-1" buf
                                                                                  (le::cci-session-id-record "claude-proj-1" "uuid-1")
                                                                                  (le::cci-session-id-record "claude-proj-1" "uuid-2")
                                                                                  (should (equal (le::cci-session-ids-for-buffer buf) '("uuid-2" "uuid-1"))))))

(ert-deftest le::cci-session-id-record/leaves-other-instances-alone ()
  "Two instances of one project must not share a history."
  (le::cci-session-ids-test--with-buffer buf
                                         (le::cci-session-ids-test--with-instance "claude-proj-1" buf
                                                                                  (le::cci-session-id-record "claude-proj-2" "uuid-other")
                                                                                  (should-not (le::cci-session-ids-for-buffer buf)))))

(ert-deftest le::cci-session-id-record/tolerates-an-unresolvable-instance ()
  "A CLI started outside Emacs, or one whose buffer is not attached yet,
returns nil rather than erroring -- the hook fires on every session start
regardless of who launched it."
  (le::cci-session-ids-test--with-instance "claude-proj-1" nil
                                           (should-not (le::cci-session-id-record "claude-proj-1" "uuid-1"))
                                           (should-not (le::cci-session-id-record "unknown-instance" "uuid-1"))))

(ert-deftest le::cci-session-id-record/tolerates-a-killed-buffer ()
  "A terminal killed while its session lingers must not resurrect a dead buffer."
  (let ((buf (generate-new-buffer "*cci: doomed*")))
    (kill-buffer buf)
    (le::cci-session-ids-test--with-instance "claude-proj-1" buf
                                             (should-not (le::cci-session-id-record "claude-proj-1" "uuid-1")))))

(provide 'le-cci-session-ids-test)
;;; le-cci-session-ids-test.el ends here
