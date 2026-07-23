;;; le-frame.el --- window/display helpers  -*- lexical-binding: t -*-

;;; Commentary:

;; Shared window/display-buffer helpers: the left-column display action and the
;; workspace magit-status predicate (used by the IDE layout in `le-ide'), plus
;; an ediff clean-slate setup.

;;; Code:

(require 'cl-lib)

(declare-function w-current "w" ())
(declare-function popper-popup-p "popper" (buf))
(declare-function ediff-setup-windows-plain "ediff-wind" (buffer-a buffer-b buffer-c control-buffer))

;;;; ---------------------------------------------------------------
;;;; Window layout — display-buffer helpers
;;;; ---------------------------------------------------------------

(defvar le::frame-transient-window-height 17
  "Height in lines for the transient menu window.")

(defvar le::frame-magit-window-height 20
  "Height in lines for the short window when splitting a single-window column.")

;;;###autoload
(defun le::frame-display-left (buffer alist)
  "Display BUFFER in the first column of the frame.
ALIST entry (size . tall|short) controls behavior (default short):
- tall: pick taller window / after split display in original top
- short: pick shorter window / after split display in new bottom"
  (let* ((tallp (eq (alist-get 'size alist) 'tall))
         (first-col (cl-remove-if-not
                     (lambda (w) (zerop (car (window-edges w))))
                     (window-list nil 'no-mini))))
    (if (cdr first-col)
        (let ((target (cl-reduce (lambda (a b)
                                   (if (funcall (if tallp #'> #'<)
                                                (window-height a) (window-height b))
                                       a b))
                                 first-col)))
          (window--display-buffer buffer target 'reuse alist))
      (when (car first-col)
        (let ((new-win (split-window (car first-col)
                                     (- le::frame-magit-window-height) 'below)))
          (if tallp
              (window--display-buffer buffer (car first-col) 'reuse alist)
            (window--display-buffer buffer new-win 'window alist)))))))

;;;###autoload
(defun le::frame-magit-status-p (buf _act)
  "Return non-nil if BUF is a magit-status buffer for the current workspace
or project."
  (when-let* ((b (get-buffer buf))
              (_ (eq (buffer-local-value 'major-mode b) 'magit-status-mode))
              (root (expand-file-name
                     (or (plist-get (w-current) :project-root)
                         default-directory))))
    (string= root (expand-file-name (buffer-local-value 'default-directory b)))))

;;;; ---------------------------------------------------------------
;;;; IDE pane routing predicates
;;;; ---------------------------------------------------------------

;; Which of the two left-column IDE panes a buffer displays in, factored out of
;; the `display-buffer-alist' rules so the buffer-flip entrances
;; (`le::project-flip') can reuse the SAME routing when scoping a flip to a
;; pane.  The alist is ordered short-before-tall, so the effective membership is
;; mutually exclusive: bottom-left claims magit-status + popups, top-left gets
;; everything else.

;;;###autoload
(defun le::frame-bottom-left-buffer-p (buf &optional act)
  "Return non-nil if BUF displays in the short bottom-left IDE pane.
That pane holds the workspace magit-status and any popper popup."
  (or (le::frame-magit-status-p buf act)
      (popper-popup-p (get-buffer buf))))

;;;###autoload
(defun le::frame-top-left-buffer-p (buf &optional _act)
  "Return non-nil if BUF displays in the tall top-left IDE pane.
Everything that does not route to the bottom-left pane (see
`le::frame-bottom-left-buffer-p'): project file buffers, dired, shells."
  (not (le::frame-bottom-left-buffer-p buf)))

;;;; ---------------------------------------------------------------
;;;; Ediff: clean slate before window setup
;;;; ---------------------------------------------------------------

;;;###autoload
(defun le::ediff-setup-windows-plain (buffer-A buffer-B buffer-C control-buffer)
  "Like `ediff-setup-windows-plain' but start from a single window."
  (let ((ignore-window-parameters t))
    (delete-other-windows
     (cl-find-if-not #'window-dedicated-p (window-list))))
  (ediff-setup-windows-plain buffer-A buffer-B buffer-C control-buffer))

(provide 'le-frame)
;;; le-frame.el ends here
