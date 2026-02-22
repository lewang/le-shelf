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

;;;###autoload
(defun le::cape-dabbrev-buffers ()
  "Return buffers for cape-dabbrev: same project root or same major mode.
Searches frame-local buffer list.  Falls back to current buffer."
  (let ((mode major-mode)
        (root (when-let* ((proj (project-current)))
                (project-root proj)))
        result)
    (dolist (buf (buffer-list (selected-frame)))
      (when (or (and root
                     (string-prefix-p root (buffer-local-value 'default-directory buf)))
                (eq mode (buffer-local-value 'major-mode buf)))
        (push buf result)))
    (or (nreverse result) (list (current-buffer)))))

(provide 'le-completion)
;;; le-completion.el ends here
