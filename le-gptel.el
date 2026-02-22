;;; le-gptel.el --- gptel utilities -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Le Wang
;; Keywords: ai

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Helper functions for gptel integration.

;;; Code:

;;;###autoload
(defun le::gptel-fill (beg end)
  (when (bound-and-true-p gptel-mode)
    (save-excursion
      (goto-char beg)
      (fill-region (save-excursion
		     (beginning-of-defun)
		     (point))
		   (save-excursion
		     (end-of-defun)
		     (point))))))

(provide 'le-gptel)
;;; le-gptel.el ends here
