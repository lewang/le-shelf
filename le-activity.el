;;; le-activity.el --- Buffer and activity utilities -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Le Wang
;; Keywords: convenience
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This package provides utilities for buffer visiting and activity management.

;;; Code:

(require 'activities)

(defun le-activity--dir-from-buffer (ab)
  "Extract a directory from activities-buffer struct AB.
If the buffer is live, use its `default-directory'.  Otherwise
fall back to the bookmark's `project-dir' or `filename' property."
  (let ((live-buf (get-buffer (activities-buffer-name ab))))
    (cond
     (live-buf
      (buffer-local-value 'default-directory live-buf))
     ((activities-buffer-bookmark ab)
      (or (bookmark-prop-get (activities-buffer-bookmark ab) 'project-dir)
          (when-let* ((fn (bookmark-prop-get (activities-buffer-bookmark ab) 'filename)))
            (file-name-directory fn))))
     ((activities-buffer-filename ab)
      (file-name-directory (activities-buffer-filename ab))))))

(defun le-activity--project-dir-from-state (state)
  "Return the project directory from STATE's claude-code-ide buffer.
Scans window state leaves for a buffer whose bookmark has a
`claude-code-ide-bookmark-handler'.  Returns the `project-dir'
bookmark property, or `default-directory' if the buffer is live."
  (catch 'found
    (activities--mapcar-window-state-leafs
     (activities-activity-state-window-state state)
     (lambda (leaf)
       (let* ((params (map-elt (cdr leaf) 'parameters))
              (ab (map-elt params 'activities-buffer)))
         (when (and ab (activities-buffer-bookmark ab)
                    (eq 'claude-code-ide-bookmark-handler
                        (bookmark-prop-get (activities-buffer-bookmark ab) 'handler)))
           (throw 'found (le-activity--dir-from-buffer ab))))))
    nil))

(defun le-activity--target-dir (target)
  "Return the directory associated with TARGET.
TARGET is a buffer name or file path."
  (if-let* ((buf (get-buffer target)))
      (buffer-local-value 'default-directory buf)
    (when (file-exists-p target)
      (if (file-directory-p target)
          (file-name-as-directory target)
        (file-name-directory (expand-file-name target))))))

(defun le-activity--find-best (target-dir)
  "Return the activity whose `le::project-root' best matches TARGET-DIR.
Best match is the activity with the longest `le::project-root'
that is a prefix of TARGET-DIR."
  (let ((target-dir (expand-file-name (file-name-as-directory target-dir)))
        best best-len)
    (message "le-activity: target-dir=%s" target-dir)
    (dolist (entry activities-activities)
      (pcase-let* ((`(,name . ,activity) entry)
                   (dir (map-elt (activities-activity-etc activity) 'le::project-root)))
        (when dir
          (message "le-activity:   activity=%s le::project-root=%s" name dir)
          (when (and (string-prefix-p dir target-dir)
                     (or (null best-len) (> (length dir) best-len)))
            (message "le-activity:   match! activity=%s dir=%s len=%d" name dir (length dir))
            (setq best activity
                  best-len (length dir))))))
    (message "le-activity: best=%s" (and best (activities-activity-name best)))
    best))

(defun le-activity--save-project-root (activity &rest _)
  "Set `le::project-root' in ACTIVITY's etc if not already set.
Derives it from the claude-code-ide buffer in the activity's state."
  (unless (map-elt (activities-activity-etc activity) 'le::project-root)
    (let* ((state (or (activities-activity-last activity)
                      (activities-activity-default activity)))
           (dir (when state
                  (le-activity--project-dir-from-state state))))
      (when dir
        (setf (map-elt (activities-activity-etc activity) 'le::project-root)
              (expand-file-name (file-name-as-directory dir)))))))

(defun le-activity--reset-project-root (activity &rest _)
  "Clear `le::project-root' from ACTIVITY's etc so it gets re-derived on next save."
  (setf (activities-activity-etc activity)
        (map-delete (activities-activity-etc activity) 'le::project-root)))

(advice-add 'activities-save :before #'le-activity--save-project-root)
(advice-add 'activities-revert :before #'le-activity--reset-project-root)

(defun le-activity--find-frame (activity)
  "Return the frame whose tab has ACTIVITY, or nil.
Searches all frames when `activities-tabs-mode' is active."
  (when (bound-and-true-p activities-tabs-mode)
    (let ((name (activities-activity-name activity)))
      (catch 'found
        (dolist (frame (frame-list))
          (dolist (tab (funcall tab-bar-tabs-function frame))
            (when-let* ((tab-activity (alist-get 'activity (cdr tab))))
              (when (equal name (activities-activity-name tab-activity))
                (throw 'found frame)))))
        nil))))

;;;###autoload
(defun le-activity-visit (target)
  "Visit TARGET in its matching activity.
TARGET is a buffer name or file path.  If an activity is found
whose buffers share the same directory tree, switch to that
activity (resuming it if necessary) and display TARGET there.
Otherwise use the default display action."
  (interactive "sBuffer or file: ")
  (let* ((target-dir (le-activity--target-dir target))
         (activity (when target-dir
                     (le-activity--find-best target-dir))))
    (message "le-activity-visit: target=%s target-dir=%s activity=%s tabs-mode=%s"
             target target-dir
             (and activity (activities-activity-name activity))
             (bound-and-true-p activities-tabs-mode))
    (if activity
        (progn
          ;; Select the frame that owns this activity's tab before resuming.
          (when-let* ((frame (le-activity--find-frame activity)))
            (message "le-activity-visit: selecting frame %s" frame)
            (select-frame-set-input-focus frame))
          (message "le-activity-visit: resuming activity %s (active=%s)"
                   (activities-activity-name activity)
                   (activities-activity-active-p activity))
          (activities-resume activity)
          (message "le-activity-visit: after resume, current=%s"
                   (and (activities-current)
                        (activities-activity-name (activities-current))))
          (pop-to-buffer (if (get-buffer target)
                             target
                           (find-file-noselect target))))
      ;; No matching activity: default action.
      (pop-to-buffer (if (get-buffer target)
                         target
                       (find-file-noselect target))))))

;;;###autoload
(with-eval-after-load 'embark
  (define-key embark-buffer-map (kbd "v") #'le-activity-visit)
  (define-key embark-file-map   (kbd "v") #'le-activity-visit))

(provide 'le-activity)
;;; le-activity.el ends here
