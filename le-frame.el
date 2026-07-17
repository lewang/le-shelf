;;; le-frame.el --- frame tools  -*- lexical-binding: t -*-

(require 'cl-lib)

(declare-function w-current "w" ())
(declare-function project-root "project" (project))
(declare-function ediff-setup-windows-plain "ediff-wind" (buffer-a buffer-b buffer-c control-buffer))
(declare-function claude-code-ide "claude-code-ide" (&optional force-dir))
(declare-function claude-code-ide-switch-to-buffer "claude-code-ide" ())
(declare-function claude-code-ide--session-buffer-p "claude-code-ide" (buffer))
(declare-function le::magit-status-noselect "le-magit" (&optional dir))
(declare-function le::debug-message "le-debug" (format-string &rest args))

(defvar claude-code-ide-window-width)
(defvar recentf-list)

(defmacro le-frame-with-max (&rest body)
  "provide max-width and height"
  `(pcase-let ((`(,_ ,_ ,max-width ,max-height)
                (alist-get 'geometry (car (display-monitor-attributes-list)))))
     ,@body))

;;;###autoload
(defun le-frame-left-two-thirds ()
  (interactive)
  (le-frame-with-max
   (set-frame-size (selected-frame) (* 2 (/ max-width 3)) max-height t)
   (set-frame-position (selected-frame) 0 0)))

;;;###autoload
(defun le-frame-right-two-thirds ()
  (interactive)
  (le-frame-with-max
   (set-frame-size (selected-frame) (- (* 2 (/ max-width 3)) 17) max-height t)
   (set-frame-position (selected-frame) (- (- (/ max-width 3) 10) -10) 0)))

;;;###autoload
(defun le-right-two-thirds ()
  (interactive)
  (le-frame-with-max
   (set-frame-size (selected-frame) (- (* 2 (/ max-width 3)) 17) max-height t)
   (set-frame-position (selected-frame) (- (- (/ max-width 3) 10) -10) 0)))


;;;###autoload
(defun le-frame-right-one-thirds ()
  (interactive)
  (le-frame-with-max
   (set-frame-position (selected-frame) (* 2 (/ max-width 3)) 0)
   (set-frame-size (selected-frame) (* 1 (/ max-width 3)) max-height t)))

;;;###autoload
(defun le-frame-center-third ()
  (interactive)
  (le-frame-with-max
   (set-frame-position (selected-frame) (/ max-width 3) 0)
   (set-frame-size (selected-frame) (- (* 1 (/ max-width 3)) 20)  max-height t)))

;;;###autoload
(defun le-frame-left-half ()
  (interactive)
  (le-frame-with-max
   (set-frame-position (selected-frame) 0 0)
   (set-frame-size (selected-frame) (* 1 (/ max-width 2)) max-height t)))

;;;###autoload
(defun le-frame-right-half ()
  (interactive)
  (le-frame-with-max
   (set-frame-position (selected-frame) (/ max-width 2) 0)
   (set-frame-size (selected-frame) (* 1 (/ max-width 2)) max-height t)))

;;;###autoload
(defun le-frame-full-screen ()
  (interactive)
  (le-frame-with-max
   (set-frame-position (selected-frame) 0 0)
   (set-frame-size (selected-frame) (* 1 (/ max-width 1)) max-height t)))

;;; transient persistent menu for sizing and moving frame around

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
(defun le::frame--right-window ()
  "Return the sole window in the right column, or nil.
Only matches when the frame has a 2-column split with exactly one
window on the right side."
  (let* ((first-col (cl-remove-if-not
                     (lambda (w) (zerop (car (window-edges w))))
                     (window-list nil 'no-mini)))
         (right-col (cl-remove-if
                     (lambda (w) (zerop (car (window-edges w))))
                     (window-list nil 'no-mini))))
    (when (= 1 (length right-col))
      (car right-col))))

;;;###autoload
(defun le::frame--magit-status-p (buf _act)
  "Return non-nil if BUF is a magit-status buffer for the current workspace or project."
  (when-let* ((b (get-buffer buf))
              (_ (eq (buffer-local-value 'major-mode b) 'magit-status-mode))
              (root (expand-file-name
                     (or (plist-get (w-current) :project-root)
                         default-directory))))
    (string= root (expand-file-name (buffer-local-value 'default-directory b)))))

;;;; ---------------------------------------------------------------
;;;; IDE layout
;;;; ---------------------------------------------------------------

;;;###autoload
(defun le::ide--compatible-layout ()
  "Check if the current window layout matches the expected 3-pane topology.
Returns a plist (:cc-win W :top-left W :bottom-left W) when compatible,
nil otherwise.

Compatibility requires:
- Exactly 3 windows (excluding minibuffer)
- Left column (x=0) has exactly 2 windows
- Right window holds the CCI session for the current workspace"
  (let* ((wins (window-list nil 'no-mini))
         (left-wins (cl-remove-if-not
                     (lambda (w)
                       (zerop (car (window-edges w))))
                     wins))
         (right-wins (cl-set-difference wins left-wins)))
    (when (and (= (length wins) 3)
               (= (length left-wins) 2)
               (= (length right-wins) 1))
      (let* ((cc-win  (car right-wins))
             (cc-buf  (window-buffer cc-win))
             (ws-root (plist-get (w-current) :project-root)))
        (when (and ws-root
                   (claude-code-ide--session-buffer-p cc-buf)
                   (string= (expand-file-name ws-root)
                            (expand-file-name
                             (buffer-local-value 'default-directory cc-buf))))
          (let* ((sorted (sort left-wins
                               (lambda (a b)
                                 (< (nth 1 (window-edges a))
                                    (nth 1 (window-edges b))))))
                 (top (car sorted))
                 (bottom (cadr sorted)))
            (list :cc-win cc-win
                  :top-left top
                  :bottom-left bottom)))))))

;;;###autoload
(defun le::ide--adjust-windows (layout)
  "Resize windows in LAYOUT to match target IDE dimensions.
LAYOUT is a plist with :cc-win, :top-left, and :bottom-left windows."
  (let ((cc-win (plist-get layout :cc-win))
        (bottom-left (plist-get layout :bottom-left)))
    (let ((width-delta (- claude-code-ide-window-width (window-width cc-win))))
      (when (and (not (zerop width-delta))
                 (window-resizable cc-win width-delta t))
        (condition-case nil
            (window-resize cc-win width-delta t)
          (error nil))))
    (let ((height-delta (- le::frame-magit-window-height (window-height bottom-left))))
      (when (and (not (zerop height-delta))
                 (window-resizable bottom-left height-delta nil))
        (condition-case nil
            (window-resize bottom-left height-delta nil)
          (error nil))))))

;;;###autoload
(defun le::ide--file-in-project-p (file project-root)
  "Return non-nil if FILE belongs to the project at PROJECT-ROOT.
Resolves FILE's own project and checks that its root matches
PROJECT-ROOT, so files in nested sub-projects are excluded -- with one
exception: when FILE's own project root satisfies
`claude-code-ide-parent-session-predicate' (e.g. a `.le-playground'
scratchpad worktree, which carries its own `.git' and so resolves as its
own project) and sits directly inside PROJECT-ROOT, FILE counts as
belonging to PROJECT-ROOT and thus qualifies for the top-left window.
Reusing that predicate keeps the scratchpad-belongs-to-parent rule in one
place -- the same knob that reparents a Claude session (see
`claude-code-ide--maybe-resolve-parent')."
  (when-let* ((proj (project-current nil (file-name-directory file)))
              (root (project-root proj)))
    (or (file-equal-p root project-root)
        (and (bound-and-true-p claude-code-ide-parent-session-predicate)
             (funcall claude-code-ide-parent-session-predicate root)
             (file-equal-p (file-name-directory (directory-file-name root))
                           project-root)))))

;;;###autoload
(defun le::ide-reset (project-root)
  "Reset frame to standard 3-pane Claude Code IDE layout.

+-----------+------------------+
| project   | claude-code-ide  |
| file/     |                  |
| dired     |                  |
+-----------+                  |
| magit     |                  |
| status    |                  |
+-----------+------------------+

Top-left: MRU file-visiting buffer for the project, or dired of
the project root if none exists.  Bottom-left: existing
magit-status buffer, or a freshly created one.

Two layout strategies:
- Compatible: when the frame already has the right 3-pane shape
  (2 left-column windows + 1 right cc window), resize and set
  buffers non-destructively, preserving window history
  (winner-mode, etc.).
- Incompatible: delete all other windows and rebuild from scratch."
  (interactive (list (or (plist-get (w-current) :project-root)
                         (expand-file-name (project-root (project-current))))))
  (let* ((project-buf (or (cl-find-if (lambda (buf)
                                        (and (not (with-current-buffer buf
                                                    (derived-mode-p 'magit-status-mode)))
                                             (when-let* ((file (buffer-file-name buf)))
                                               (le::ide--file-in-project-p file project-root))))
                                      (buffer-list (selected-frame)))
                          (when-let* ((file (cl-find-if
                                             (lambda (f)
                                               (le::ide--file-in-project-p f project-root))
                                             recentf-list)))
                            (find-file-noselect file))
                          (dired-noselect project-root))))
    (le::debug-message "ide-reset: project-root=%s project-buf=%s" project-root project-buf)
    (let ((compatiblep (le::ide--compatible-layout))
          bottom-left)
      (if compatiblep
          (progn
            (le::debug-message "ide-reset: compatible layout, reusing")
            (select-window (plist-get compatiblep :top-left))
            (switch-to-buffer project-buf nil t)
            (setq bottom-left (plist-get compatiblep :bottom-left)))
        ;; Not compatible: tear down to a single known-good window, then build.
        (le::debug-message "ide-reset: incompatible layout, rebuilding")
        ;; `ignore-window-parameters' lets us delete side windows (e.g. the
        ;; claude-code-ide window).  When invoked from a dedicated window with no
        ;; normal sibling, fall back to it.
        (let ((ignore-window-parameters t))
          (delete-other-windows
           (or (cl-find-if-not #'window-dedicated-p (window-list))
               (selected-window))))
        ;; Survivor may be dedicated or itself a side window (reset invoked from
        ;; one); demote it to a plain window so the split is safe (no side window
        ;; -> no Emacs 31 split-window recursion) and `switch-to-buffer' does not
        ;; error on a dedicated window.
        (let ((win (selected-window)))
          (set-window-dedicated-p win nil)
          (set-window-parameter win 'window-side nil))
        (switch-to-buffer project-buf nil t)
        (setq bottom-left (split-window (selected-window)
                                        (- le::frame-magit-window-height) 'below)))
      (le::debug-message "ide-reset: starting claude-code-ide")
      (save-selected-window
        (let ((default-directory project-root))
          (condition-case nil
              (claude-code-ide-switch-to-buffer)
            (user-error (claude-code-ide)))))
      ;; Place magit directly in the bottom-left pane -- no dependence on the
      ;; display-buffer-alist routing or on `le::ide--compatible-layout'.
      (le::debug-message "ide-reset: placing magit in bottom-left")
      (if (window-live-p bottom-left)
          (with-selected-window bottom-left
            (let ((display-buffer-overriding-action '(display-buffer-same-window)))
              (le::magit-status-noselect project-root)))
        (le::magit-status-noselect project-root))
      (le::debug-message "ide-reset: adjusting window sizes")
      (le::ide--adjust-windows (le::ide--compatible-layout))
      (le::debug-message "ide-reset: done"))))

;;;; ---------------------------------------------------------------
;;;; Role-aware prev-buffer skip
;;;; ---------------------------------------------------------------

;; When a buffer shown in an IDE pane is killed/buried, keep the window on its
;; ROLE instead of whatever `switch-to-prev-buffer' pulls from history: the
;; top-left pane stays on project buffers, the bottom-left pane prefers the
;; workspace's magit-status (then project buffers).  Installed as
;; `switch-to-prev-buffer-skip' (a filter: non-nil skips a candidate).

(defun le::frame--skip-unless (window keeper-p buffer)
  "Return non-nil to SKIP BUFFER for WINDOW unless it is a role keeper.
KEEPER-P is a predicate on a buffer.  Skip BUFFER only when it fails KEEPER-P
AND some live buffer on WINDOW's frame -- other than BUFFER and other than the
buffer WINDOW is being moved off of (`window-buffer', the one killed/buried) --
passes KEEPER-P, i.e. a reachable keeper is available to fall back on.  If
BUFFER is itself a keeper, or no other reachable keeper exists, return nil (do
not skip), so the window is never stranded (on the kill path an all-skipped
result would delete it)."
  (and (not (funcall keeper-p buffer))
       (let ((old (window-buffer window)))
         (seq-find (lambda (b)
                     (and (not (eq b buffer))
                          (not (eq b old))
                          (buffer-live-p b)
                          (funcall keeper-p b)))
                   (buffer-list (window-frame window))))
       t))

(defun le::frame--workspace-magit-status (&optional exclude)
  "Return the current workspace's live magit-status buffer, or nil.
EXCLUDE, when non-nil, is not considered (e.g. the buffer being killed).
Relies on the caller having selected the relevant window/frame so that
`le::frame--magit-status-p' resolves the intended workspace."
  (seq-find (lambda (b)
              (and (buffer-live-p b)
                   (not (eq b exclude))
                   (le::frame--magit-status-p b nil)))
            (buffer-list)))

;;;###autoload
(defun le::frame-prev-buffer-skip (window buffer _bury-or-kill)
  "`switch-to-prev-buffer-skip' predicate that keeps each IDE pane on its role.
Only acts inside the recognized 3-pane layout for WINDOW's own frame
\(`le::ide--compatible-layout'); elsewhere returns nil so Emacs behaves
normally.  In that layout:
- top-left (project): skip BUFFER unless it is a workspace project buffer
  (per `le::ide--file-in-project-p', which counts `.le-playground' scratchpad
  buffers as part of the parent project);
- bottom-left (magit): prefer the workspace's live magit-status; if none,
  fall back to workspace project buffers;
- right (cci) / anything else: leave alone.
Never skips so as to strand the window (see `le::frame--skip-unless')."
  (with-selected-window window
    (when-let* ((layout (le::ide--compatible-layout))
                (ws-root (plist-get (w-current) :project-root)))
      (cl-flet ((keeper-project-p (b)
                  (when-let* ((f (buffer-file-name b)))
                    (le::ide--file-in-project-p f ws-root))))
        (cond
         ((eq window (plist-get layout :top-left))
          (le::frame--skip-unless window #'keeper-project-p buffer))
         ((eq window (plist-get layout :bottom-left))
          (if-let* ((ms (le::frame--workspace-magit-status (window-buffer window))))
              (le::frame--skip-unless window (lambda (b) (eq b ms)) buffer)
            (le::frame--skip-unless window #'keeper-project-p buffer)))
         (t nil))))))

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
