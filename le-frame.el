;;; le-frame.el --- window/display helpers  -*- lexical-binding: t -*-

;;; Commentary:

;; Shared window/display-buffer helpers: the left-column display action and the
;; workspace magit-status predicate (used by the IDE layout in `le-ide'), plus
;; an ediff clean-slate setup.

;;; Code:

(require 'cl-lib)
(require 'w)

(declare-function popper-popup-p "popper" (buf))
(declare-function ediff-setup-windows-plain "ediff-wind" (buffer-a buffer-b buffer-c control-buffer))
(declare-function claude-code-ide-mcp--sessions-for-project "claude-code-ide-mcp" (project-dir))
(declare-function claude-code-ide-mcp-session-buffer "claude-code-ide-mcp" (session))
(declare-function claude-code-ide--display-buffer-in-side-window "claude-code-ide" (buffer &optional tile))

(defvar claude-code-ide-focus-on-open)

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

;; Which IDE pane a buffer displays in, factored out of the
;; `display-buffer-alist' rules so the buffer-flip entrances
;; (`le::project-flip') can reuse the SAME routing when scoping a flip to a
;; pane.  Membership is mutually exclusive: the cc pane claims the workspace's
;; Claude terminals, bottom-left claims magit-status + popups, top-left gets
;; everything else.  The alist is ordered cc, then short-before-tall.

(defun le::frame--workspace-cc-buffers ()
  "Return the live Claude terminal buffers of the current workspace's project.
Membership comes from claude-code-ide's own session table, not from each
buffer's `default-directory' -- in a terminal buffer that tracks the shell's
cwd, so it drifts as soon as Claude changes directory."
  (when-let* ((root (plist-get (w-current) :project-root)))
    (delq nil
          (mapcar (lambda (session)
                    (when-let* ((buf (claude-code-ide-mcp-session-buffer session)))
                      (and (buffer-live-p buf) buf)))
                  (claude-code-ide-mcp--sessions-for-project
                   (file-name-as-directory (expand-file-name root)))))))

;;;###autoload
(defun le::frame-cc-buffer-p (buf &optional _act)
  "Return non-nil if BUF is a Claude terminal of the current workspace.
Every instance of the project qualifies, not just the visible one, so a
second instance routes to the same pane the first one occupies."
  (when-let* ((b (get-buffer buf)))
    (memq b (le::frame--workspace-cc-buffers))))

;;;###autoload
(defun le::frame-bottom-left-buffer-p (buf &optional act)
  "Return non-nil if BUF displays in the short bottom-left IDE pane.
That pane holds the workspace magit-status and any popper popup."
  (or (le::frame-magit-status-p buf act)
      (popper-popup-p (get-buffer buf))))

;;;###autoload
(defun le::frame-top-left-buffer-p (buf &optional act)
  "Return non-nil if BUF displays in the tall top-left IDE pane.
Everything claimed by neither the cc pane (`le::frame-cc-buffer-p') nor the
bottom-left one (`le::frame-bottom-left-buffer-p'): project file buffers,
dired, shells."
  (not (or (le::frame-bottom-left-buffer-p buf act)
           (le::frame-cc-buffer-p buf act))))

;;;###autoload
(defun le::frame-display-cc (buffer alist)
  "Display BUFFER in its project's Claude side window, as claude-code-ide would.
A `display-buffer' action function that delegates rather than spelling out a
`display-buffer-in-side-window' rule of its own: claude-code-ide owns the slot
arithmetic that collapses a project's instances onto one window, the width
function, the hard dedication, and the last-used bookkeeping.

Decline when ALIST forbids the selected window and BUFFER already shows
there, so `display-buffer' goes on to its other actions.  Claiming it would
answer `switch-to-buffer-other-window' with the very window it asked to avoid
-- which is how consult picks its preview window, and a dedicated one sends
every later candidate bouncing into another pane.

`claude-code-ide-focus-on-open' is suppressed because an action function
displays and leaves selection to its caller -- `pop-to-buffer' selects the
window anyway, so the paths that want focus still get it."
  (unless (and (cdr (assq 'inhibit-same-window alist))
               (eq (window-buffer (selected-window)) buffer))
    (let ((claude-code-ide-focus-on-open nil))
      (claude-code-ide--display-buffer-in-side-window buffer))))

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
