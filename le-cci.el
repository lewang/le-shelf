;;; le-cci.el --- Claude Code IDE utility functions -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Le Wang
;; Keywords: tools
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Utility functions for Claude Code IDE integration: vterm helpers,
;; server focus management, and prompt file setup.

;;; Code:

;;;###autoload
(defun le::vterm-send-C-g ()
  "Send C-g to the vterm process."
  (interactive)
  (vterm-send-key (kbd "C-g")))

;;;###autoload
(defun le::server-focus-on-claude-code-ide-on-exit ()
  "Focus on the vterm window after server-done.
Used as a `server-done-hook' to return to the CCI terminal."
  (when-let ((vterm-window
              (get-window-with-predicate
               (lambda (window)
                 (with-current-buffer (window-buffer window)
                   (eq major-mode 'vterm-mode))))))
    (select-window vterm-window)))

;;;###autoload
(defun le::claude-prompt-file-setup ()
  "Setup buffer-local hooks for Claude prompt files.

Auto-return to vterm window after editing Claude prompt files.

Should have this setting: (setq server-window \\='pop-to-buffer)"
  (when (and (buffer-file-name)
             (string-match-p "^claude-prompt-.*\\.md$"
                             (file-name-nondirectory (buffer-file-name))))
    (add-hook 'server-done-hook 'le::server-focus-on-claude-code-ide-on-exit nil t)))

(provide 'le-cci)
;;; le-cci.el ends here
