;;; le-frame.el --- frame tools  -*- lexical-binding: t -*-


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

Expected topology:
- Exactly 3 windows (excluding minibuffer)
- Left column (x=0) has exactly 2 windows"
  (let* ((wins (window-list nil 'no-mini))
         (left-wins (cl-remove-if-not
                     (lambda (w)
                       (zerop (car (window-edges w))))
                     wins))
         (right-wins (cl-set-difference wins left-wins)))
    (when (and (= (length wins) 3)
               (= (length left-wins) 2)
               (= (length right-wins) 1))
      (let* ((sorted (sort left-wins
                           (lambda (a b)
                             (< (nth 1 (window-edges a))
                                (nth 1 (window-edges b))))))
             (top (car sorted))
             (bottom (cadr sorted)))
        (list :cc-win (car right-wins)
              :top-left top
              :bottom-left bottom)))))

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
  "Return non-nil if FILE belongs exactly to the project at PROJECT-ROOT.
Unlike `file-in-directory-p', this resolves FILE's own project and
checks that its root matches PROJECT-ROOT, so files in nested
sub-projects are excluded."
  (when-let* ((proj (project-current nil (file-name-directory file))))
    (file-equal-p (project-root proj) project-root)))

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
    (message "ide-reset: project-root=%s project-buf=%s" project-root project-buf)
    ;; prepare layout
    (if-let* ((layout (le::ide--compatible-layout)))
        (progn
          (message "ide-reset: compatible layout, selecting top-left")
          (select-window (plist-get layout :top-left)))
      (message "ide-reset: incompatible layout, delete-other-windows")
      (delete-other-windows
       (cl-find-if-not #'window-dedicated-p (window-list))))
    (message "ide-reset: switch-to-buffer %s" project-buf)
    (switch-to-buffer project-buf nil t)
    ;; now display-buffer-alist takes over
    (message "ide-reset: starting claude-code-ide")
    (save-selected-window
      (let ((default-directory project-root))
        (condition-case nil
            (claude-code-ide-switch-to-buffer)
          (user-error (claude-code-ide)))))
    (message "ide-reset: starting magit-status-noselect")
    (le::magit-status-noselect project-root)
    ;; final adjustments
    (message "ide-reset: adjusting window sizes")
    (le::ide--adjust-windows (le::ide--compatible-layout))
    (message "ide-reset: done")))

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
