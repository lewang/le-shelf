;;; le-shelf.el --- Miscellaneous misc utilities -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Le Wang
;; Keywords: miscellaneous
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (popper "0.1") (claude-code-ide "0.1") (buffer-flip "10.0") (denote "4.2.3"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This package provides miscellaneous utilities.

;;; Code:

(defun le-cycle-spacing--region ()
  "Return the whitespace `le-cycle-spacing' deletes next, as (BEG . END).

The whitespace still standing around point decides the step, so no press
count is tracked:

  1. whitespace after point, up to the end of the line,
  2. whitespace before point, back to the beginning of the line,
  3. with point at the beginning of a line, the line break before it,
     merging this line into the previous one.

Only whitespace touching point is ever considered.  Return nil when there
is none."
  (let ((right (save-excursion (skip-chars-forward " \t") (point)))
        (left (save-excursion (skip-chars-backward " \t") (point))))
    (cond ((> right (point)) (cons (point) right))
          ((< left (point)) (cons left (point)))
          ((and (bolp) (not (bobp)))
           (cons (save-excursion (skip-chars-backward " \t\n") (point))
                 (point))))))

;;;###autoload
(defun le-cycle-spacing (&optional arg)
  "Delete the whitespace around point one step at a time.

Successive presses clear the space to the right, then the space to the
left, then the line break -- see `le-cycle-spacing--region'.  Whitespace
that does not touch point is left alone, and a press with none to delete
only reports that.

With prefix ARG, defer to `cycle-spacing'."
  (interactive "*P")
  (if arg
      (cycle-spacing arg)
    (if-let* ((region (le-cycle-spacing--region)))
        (delete-region (car region) (cdr region))
      (message "No whitespace around point"))))



(defun le-is-text (&optional pos)
  "Return t if POS should be treated as text.

That is comment or string in programming modes or anywhere in text mode."
  (setq pos (or pos
                (point)))
  (save-excursion
    (goto-char pos)
    (or (not comment-start)
        (syntax-ppss-context (syntax-ppss)))))

;;;###autoload
(defun le-forward-sentence-smart (arg)
  "Go to end of sexp.  Invoke again to move out of sexp.

When inside strings, move by sentence instead.

Approprfiate for use as `forward-sentence-function'"
  (or arg (setq arg 1))
  (let ((list
         ;; Get list of positions end of current context and before ")", or
         ;; end of sentence choose next one.
         `(,@(save-excursion
               (ignore-errors
                 (paredit-forward-up arg)
                 (list (- (point) (if (< arg 0)
                                      -1 1))
                       (point))))
           ,@(when (le-is-text)
               (save-excursion
                 (forward-sentence-default-function arg)
                 (list (point)))))))
    (when list
      (goto-char (apply (if (< arg 0)
                            'max
                          'min)
                        ;; second call must move.
                        (if (eq last-command this-command)
                            (delq (point) list)
                          list))))))


;;;###autoload
(defun le::paredit-RET ()
  "paredit hack needed for 27 beta
     Wraps `paredit-RET' to provide a sensible minibuffer experience."
  (interactive)
  (cond ((minibufferp) (call-interactively 'read--expression-try-read))
        ((memq major-mode '(cider-repl-mode)) (call-interactively 'cider-repl-return))
        ((call-interactively 'paredit-RET))))

(provide 'le-shelf)
