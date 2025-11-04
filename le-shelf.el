;;; le-shelf.el --- Miscellaneous misc utilities -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Lewis Wang
;; Keywords: miscellaneous
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This package provides miscellaneous utilities.

;;; Code:


;;;###autoload
(defun le-cycle-spacing (arg)
  (interactive "*P")
  (cycle-spacing (if (eq arg nil)
                     '-
                   arg))
  ;; remove spacek ") )" in lisps
  (if (save-excursion
        (goto-char (- (point) 2))
        (looking-at ") )"))
      (backward-delete-char 1)))



;;;###autoload
(defun le-is-text (&optional pos)
  "Return t if POS should be treated as text.

That is comment or string in programming modes or anywhere in text mode."
  (setq pos (or pos
                (point)))
  (save-excursion
    (goto-char pos)
    (or (not comment-start)
        (syntax-ppss-context (syntax-ppss)))))

(defun le-beginning-of-sexp-scope (arg)
  "Go to beginning of current sexp's scope.  If point doesn't
move then go backward up one sexp level."
  (if (le-is-text)
      (forward-sentence-default-function (* -1 arg))
    (let ((orig-point (point)))
      (paredit-backward-up arg)
      (unless (= (+ (point) 1) orig-point)
        (forward-char))
      (point))))

;;;###autoload
(defun le-forward-sentence-smart (arg)
  "Go to end of sexp.  Invoke again to move out of sexp.

When inside strings, move by sentence instead.

Approprfiate for use as `forward-sentence-function'"
  (or arg (setq arg 1))
  (if (< arg 0)
      (le-beginning-of-sexp-scope (* -1 arg))
    (let ((list
           ;; Get list of positions end of current context and before ")", or
           ;; end of sentence choose next one.
           `(,@(save-excursion
                 (ignore-errors
                   (paredit-forward-up arg)
                   (list (1- (point))
                         (point))))
             ,@(when (le-is-text)
                 (save-excursion
                   (forward-sentence-default-function arg)
                   (list (point)))))))
      (when list
        (goto-char (apply 'min
                          ;; second call must move.
                          (if (eq last-command this-command)
                              (delq (point) list)
                            list)))))))


(provide 'le-shelf)
