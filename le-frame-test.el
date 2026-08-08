;;; le-frame-test.el --- Tests for le-frame's IDE pane routing  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for the three-way pane split: `le::frame-cc-buffer-p',
;; `le::frame-bottom-left-buffer-p' and `le::frame-top-left-buffer-p'.
;;
;; The split is the whole contract -- the `display-buffer-alist' rules and
;; `le::project-flip' both consult it, so a buffer claimed by two panes (or by
;; none) desyncs display routing from flip scoping.  Every case below therefore
;; asserts a buffer's membership in ALL THREE panes, not just the interesting
;; one.
;;
;; claude-code-ide's session table is stubbed rather than built: constructing a
;; real session would drag in the terminal backend for no gain, since what is
;; under test is the routing, not the table.  The stub also pins the DIRECTORY
;; NORMALIZATION -- claude-code-ide compares project directories with `string=',
;; so a workspace root that reaches it unslashed silently matches nothing.
;;
;; Run in batch:
;;
;;   emacs -Q -batch -L "$PWD" -L <elpaca-builds>/w \
;;     -l "$PWD/le-frame.el" -l "$PWD/le-frame-test.el" \
;;     -f ert-run-tests-batch-and-exit
;;
;; The paths must be absolute: `-l le-frame.el' is resolved against `load-path'
;; rather than the working directory, so the relative form fails to load.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'le-frame)

(defvar le::frame-test--asked-for nil
  "Directory the last stubbed session lookup was called with.")

;; Marks the variable special HERE too.  claude-code-ide's own `defcustom' does
;; that in a live Emacs, but without it the focus test's `let' is lexical and
;; the stub closes over the test's own binding instead of reading the dynamic
;; one the function establishes -- passing or failing for the wrong reason.
(defvar claude-code-ide-focus-on-open)

(defmacro le::frame-test--with-ide (root sessions &rest body)
  "Run BODY with workspace ROOT and SESSIONS as the project's Claude buffers.
SESSIONS is an alist of (PROJECT-DIR . BUFFERS).  The lookup records its
argument in `le::frame-test--asked-for' so a test can pin normalization.
`popper-popup-p' is stubbed nil so the bottom-left pane is decided by
magit-status alone."
  (declare (indent 2) (debug (form form body)))
  `(let ((le::frame-test--asked-for nil))
     (cl-letf (((symbol-function 'w-current)
                (lambda () (when ,root (list :name "test" :project-root ,root))))
               ((symbol-function 'popper-popup-p) (lambda (_buf) nil))
               ((symbol-function 'claude-code-ide-mcp-session-buffer) #'identity)
               ((symbol-function 'claude-code-ide-mcp--sessions-for-project)
                (lambda (dir)
                  (setq le::frame-test--asked-for dir)
                  (cdr (assoc dir ,sessions)))))
       ,@body)))

(defmacro le::frame-test--with-buffer (name &rest body)
  "Run BODY with a live buffer NAME bound to `buf', killed afterwards."
  (declare (indent 1) (debug (form body)))
  `(let ((buf (generate-new-buffer ,name)))
     (unwind-protect (progn ,@body)
       (kill-buffer buf))))

(defun le::frame-test--panes (buf)
  "Return the panes claiming BUF, as a list of `cc', `bottom' and `top'."
  (delq nil (list (and (le::frame-cc-buffer-p buf) 'cc)
                  (and (le::frame-bottom-left-buffer-p buf) 'bottom)
                  (and (le::frame-top-left-buffer-p buf) 'top))))

;;;; The three-way split

(ert-deftest le::frame-cc-buffer-p/workspace-instance-goes-to-cc-pane ()
  "A Claude terminal of the workspace's project is claimed by the cc pane only."
  (le::frame-test--with-buffer "*cci: proj*"
    (le::frame-test--with-ide "/tmp/proj/" (list (cons "/tmp/proj/" (list buf)))
      (should (equal (le::frame-test--panes buf) '(cc))))))

(ert-deftest le::frame-cc-buffer-p/second-instance-also-goes-to-cc-pane ()
  "Every instance of the project routes to the cc pane, not just the visible one."
  (le::frame-test--with-buffer "*cci: proj*"
    (le::frame-test--with-buffer "*cci: proj:review*"
      (let ((second buf))
        (le::frame-test--with-ide "/tmp/proj/"
            (list (cons "/tmp/proj/" (list (get-buffer "*cci: proj*") second)))
          (should (equal (le::frame-test--panes second) '(cc))))))))

(ert-deftest le::frame-cc-buffer-p/foreign-instance-is-not-claimed ()
  "A Claude terminal of ANOTHER project is not the workspace's, so it falls
through to the top-left pane like any other buffer."
  (le::frame-test--with-buffer "*cci: other*"
    (le::frame-test--with-ide "/tmp/proj/" (list (cons "/tmp/other/" (list buf)))
      (should (equal (le::frame-test--panes buf) '(top))))))

(ert-deftest le::frame-top-left-buffer-p/plain-file-buffer ()
  "An ordinary buffer belongs to the top-left pane and to no other."
  (le::frame-test--with-buffer "le-frame.el"
    (le::frame-test--with-ide "/tmp/proj/" (list (cons "/tmp/proj/" nil))
      (should (equal (le::frame-test--panes buf) '(top))))))

(ert-deftest le::frame-bottom-left-buffer-p/workspace-magit-status ()
  "The workspace's magit-status belongs to the bottom-left pane and no other."
  (le::frame-test--with-buffer "magit: proj"
    (with-current-buffer buf
      (setq major-mode 'magit-status-mode
            default-directory "/tmp/proj/"))
    (le::frame-test--with-ide "/tmp/proj/" (list (cons "/tmp/proj/" nil))
      (should (equal (le::frame-test--panes buf) '(bottom))))))

;;;; Session-table plumbing

(ert-deftest le::frame-cc-buffer-p/normalizes-the-workspace-root ()
  "The root reaches the session table slashed, however the workspace spells it.
claude-code-ide compares project directories with `string=', so an unslashed
root would match no session at all."
  (le::frame-test--with-buffer "*cci: proj*"
    (le::frame-test--with-ide "/tmp/proj" (list (cons "/tmp/proj/" (list buf)))
      (should (le::frame-cc-buffer-p buf))
      (should (equal le::frame-test--asked-for "/tmp/proj/")))))

(ert-deftest le::frame-cc-buffer-p/skips-dead-session-buffers ()
  "A session whose terminal has been killed contributes nothing."
  (let ((dead (generate-new-buffer "*cci: gone*")))
    (kill-buffer dead)
    (le::frame-test--with-buffer "le-frame.el"
      (le::frame-test--with-ide "/tmp/proj/" (list (cons "/tmp/proj/" (list dead)))
        (should-not (le::frame-cc-buffer-p buf))
        (should (equal (le::frame--workspace-cc-buffers) nil))))))

(ert-deftest le::frame-cc-buffer-p/no-workspace-claims-nothing ()
  "Outside a workspace there is no project to scope to, so nothing is a cc buffer."
  (le::frame-test--with-buffer "*cci: proj*"
    (le::frame-test--with-ide nil (list (cons "/tmp/proj/" (list buf)))
      (should-not (le::frame-cc-buffer-p buf))
      (should (equal (le::frame-test--panes buf) '(top))))))

(ert-deftest le::frame-cc-buffer-p/accepts-a-buffer-name ()
  "`display-buffer-alist' calls its condition with a buffer NAME, not a buffer."
  (le::frame-test--with-buffer "*cci: proj*"
    (le::frame-test--with-ide "/tmp/proj/" (list (cons "/tmp/proj/" (list buf)))
      (should (le::frame-cc-buffer-p (buffer-name buf))))))

;;;; The display action

;; `le::frame-display-cc' declines rather than answering with the very window
;; the caller asked to avoid.  consult picks its preview window by calling
;; `switch-to-buffer-other-window' on the original buffer: hand it the cc
;; window and, being dedicated, it sends every later candidate bouncing into
;; another pane with nothing to restore it.

(defmacro le::frame-test--with-delegate (&rest body)
  "Run BODY with the claude-code-ide display delegate stubbed.
Binds `calls' to a list the stub pushes its BUFFER argument onto."
  (declare (indent 0) (debug (body)))
  `(let ((calls nil))
     (cl-letf (((symbol-function 'claude-code-ide--display-buffer-in-side-window)
                (lambda (buffer &optional _tile) (push buffer calls) 'the-cc-window)))
       ,@body)))

(ert-deftest le::frame-display-cc/declines-the-window-it-was-told-to-avoid ()
  "With `inhibit-same-window' and BUFFER already selected, decline untouched."
  (le::frame-test--with-buffer "*cci: proj*"
    (le::frame-test--with-delegate
      (set-window-buffer (selected-window) buf)
      (should-not (le::frame-display-cc buf '((inhibit-same-window . t))))
      (should-not calls))))

(ert-deftest le::frame-display-cc/displays-when-the-buffer-is-elsewhere ()
  "`inhibit-same-window' bars only the SELECTED window, not the cc window."
  (le::frame-test--with-buffer "*cci: proj*"
    (le::frame-test--with-delegate
      (set-window-buffer (selected-window) (get-buffer-create "*scratch*"))
      (should (eq (le::frame-display-cc buf '((inhibit-same-window . t)))
                  'the-cc-window))
      (should (equal calls (list buf))))))

(ert-deftest le::frame-display-cc/displays-when-nothing-is-inhibited ()
  "Without `inhibit-same-window' the selected window is fair game."
  (le::frame-test--with-buffer "*cci: proj*"
    (le::frame-test--with-delegate
      (set-window-buffer (selected-window) buf)
      (should (eq (le::frame-display-cc buf nil) 'the-cc-window))
      (should (equal calls (list buf))))))

(ert-deftest le::frame-display-cc/suppresses-claude-code-ide-focus ()
  "Selection is the caller's business, so the delegate must not grab focus."
  (le::frame-test--with-buffer "*cci: proj*"
    (let ((claude-code-ide-focus-on-open t) seen)
      (cl-letf (((symbol-function 'claude-code-ide--display-buffer-in-side-window)
                 (lambda (_buffer &optional _tile)
                   (setq seen claude-code-ide-focus-on-open)
                   'the-cc-window)))
        (le::frame-display-cc buf nil)
        (should-not seen)
        (should (eq claude-code-ide-focus-on-open t))))))

(provide 'le-frame-test)
;;; le-frame-test.el ends here
