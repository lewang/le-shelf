;;; le-debug.el --- Debug logging for le-shelf -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Le Wang
;; Keywords: debugging

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Conditional debug messaging gated by `le::debug'.

;;; Code:

;;;###autoload
(defvar le::debug nil
  "When non-nil, enable verbose debug messages in le-shelf functions.")

;;;###autoload
(defun le::debug-message (format-string &rest args)
  "Like `message' but only when `le::debug' is non-nil."
  (when le::debug
    (apply #'message format-string args)))

(provide 'le-debug)

;;; le-debug.el ends here
