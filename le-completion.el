;;; le-completion.el --- Completion utility functions -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Le Wang
;; Keywords: completion
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Completion helpers for cape-dabbrev and friends.

;;; Code:

(require 'le-project)                    ; le::project-root-here

;;;###autoload
(defun le::cape-dabbrev-buffers ()
  "Return buffers for cape-dabbrev: same project root or same major mode.
Searches frame-local buffer list.  Falls back to current buffer.

The root comes from `le::project-root-here', so it follows the tab's `w'
workspace and is normalized -- `project-root' answers `~'-abbreviated while
`default-directory' is expanded, which made the prefix test below fail for
every buffer.

The root test is plain path CONTAINMENT, so a nested child project's buffers
count as nearby -- deliberate here, where the question is which buffers hold
words worth completing, not which project owns them."
  (let ((mode major-mode)
        (root (le::project-root-here))
        result)
    (dolist (buf (buffer-list (selected-frame)))
      (when (or (and root
                     (string-prefix-p root (buffer-local-value 'default-directory buf)))
                (eq mode (buffer-local-value 'major-mode buf)))
        (push buf result)))
    (or (nreverse result) (list (current-buffer)))))

(provide 'le-completion)
;;; le-completion.el ends here
