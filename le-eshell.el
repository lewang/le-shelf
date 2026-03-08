;;; le-eshell.el --- Eshell prompt with git status -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Le Wang
;; Keywords: eshell

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Eshell prompt showing exit status, cwd, and git branch/dirty state via magit.

;;; Code:

(require 'eshell)
(require 'em-prompt)

;;;###autoload
(defun le-eshell-prompt ()
  "Eshell prompt with git status via magit."
  (let* ((cwd (abbreviate-file-name (eshell/pwd)))
         (ref (magit-get-shortname "HEAD"))
         (stat (magit-file-status))
         (x-stat eshell-last-command-status)
         (git-chunk
          (if ref
              (format "%s%s%s "
                      (propertize (if stat "[" "(") 'font-lock-face (list :foreground (if stat "red" "green")))
                      (propertize ref 'font-lock-face '(:foreground "yellow"))
                      (propertize (if stat "]" ")") 'font-lock-face (list :foreground (if stat "red" "green"))))
            "")))
    (propertize
     (format "%s %s %s$ "
             (if (< 0 x-stat)
                 (format (propertize "!%s" 'font-lock-face '(:foreground "red")) x-stat)
               (propertize "➤" 'font-lock-face (list :foreground "green")))
             (propertize cwd 'font-lock-face '(:foreground "#45babf"))
             git-chunk)
     'read-only t
     'front-sticky   '(font-lock-face read-only)
     'rear-nonsticky '(font-lock-face read-only))))

(provide 'le-eshell)
;;; le-eshell.el ends here
