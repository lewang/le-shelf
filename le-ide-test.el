;;; le-ide-test.el --- Tests for le-ide's hostable-window predicate  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for `le::ide-hostable-window', the guard that keeps
;; `le::cci-prompt2-edit' from popping up a frame it cannot recover from.
;;
;; The predicate is a pure read of window state, so it runs unchanged in batch:
;; a batch frame has a real window that can be dedicated, split, and given a
;; side window.  The cases pin the two exclusions separately -- dedication and
;; the `window-side' parameter -- because the real failure (a frame holding
;; only the claude-code-ide session) carries BOTH, and a predicate testing
;; either one alone would still pass that case while missing near misses.
;;
;; Run in batch:
;;
;;   emacs -Q -batch -L "$PWD" -L <elpaca-builds>/w \
;;     -l "$PWD/le-ide.el" -l "$PWD/le-ide-test.el" \
;;     -f ert-run-tests-batch-and-exit
;;
;; The paths must be absolute: `-l le-ide.el' is resolved against `load-path'
;; rather than the working directory, so the relative form fails to load.

;;; Code:

(require 'ert)
(require 'le-ide)

(defmacro le::ide-test--with-layout (&rest body)
  "Run BODY on a scratch single-window layout, restoring the frame afterwards."
  (declare (indent 0) (debug (body)))
  `(save-window-excursion
     (delete-other-windows)
     ,@body))

(ert-deftest le::ide-test-hostable-plain-window ()
  "An ordinary window hosts, and is the window returned."
  (le::ide-test--with-layout
    (should (eq (le::ide-hostable-window) (selected-window)))))

(ert-deftest le::ide-test-hostable-sole-dedicated-window ()
  "A frame whose only window is dedicated has nowhere to host."
  (le::ide-test--with-layout
    (set-window-dedicated-p (selected-window) t)
    (should-not (le::ide-hostable-window))))

(ert-deftest le::ide-test-hostable-weak-dedication-blocks ()
  "Weak dedication blocks too -- `get-lru-window' rejects any non-nil flag."
  (le::ide-test--with-layout
    (set-window-dedicated-p (selected-window) 'weak)
    (should-not (le::ide-hostable-window))))

(ert-deftest le::ide-test-hostable-skips-dedicated-window ()
  "With one dedicated and one free window, the free one is returned."
  (le::ide-test--with-layout
    (let ((other (split-window)))
      (set-window-dedicated-p (selected-window) t)
      (should (eq (le::ide-hostable-window) other)))))

(ert-deftest le::ide-test-hostable-skips-side-window ()
  "A side window is not hostable, so a dedicated main window leaves none.
This is the reported layout: the claude-code-ide session window is both a side
window and dedicated."
  (let ((buf (get-buffer-create "*le::ide-test-side*")))
    (unwind-protect
        (le::ide-test--with-layout
          (let ((side (display-buffer-in-side-window
                       buf '((side . bottom) (window-height . 4)))))
            (should side)
            (set-window-dedicated-p (selected-window) t)
            (should-not (le::ide-hostable-window))))
      (kill-buffer buf))))

(provide 'le-ide-test)
;;; le-ide-test.el ends here
