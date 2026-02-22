;;; le-magit.el --- Magit utility commands -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Le Wang
;; Keywords: vc
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Magit utility commands: noselect status display, conditional refresh.

;;; Code:

;;;###autoload
(defun le::magit-status-noselect (&optional dir)
  "Show magit-status for DIRECTORY without selecting the window.
Refreshes the buffer and scrolls to the top."
  (interactive)
  (require 'magit)
  (let* ((magit-display-buffer-noselect t)
         (buf (magit-status-setup-buffer (or dir default-directory))))
    (with-current-buffer buf
      (magit-refresh)
      (unless (equal (window-buffer (selected-window))
                     buf)
        (goto-char (point-min))))))

;;;###autoload
(defun le::magit-refresh-maybe (directory)
  "Refresh the magit-status buffer for DIRECTORY if one exists."
  (interactive (list (or (magit-toplevel) default-directory)))
  (when-let* ((default-directory directory)
              (buf (magit-get-mode-buffer 'magit-status-mode)))
    (with-current-buffer buf (magit-refresh))))

(provide 'le-magit)
;;; le-magit.el ends here
