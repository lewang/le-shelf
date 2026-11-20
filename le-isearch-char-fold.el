;;; le-isearch-char-fold.el --- Regexp isearch that folds accented characters -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Le Wang
;; Keywords: matching
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Built-in isearch cannot combine regexp search with character folding:
;; `char-fold-to-regexp' quotes its whole input as a literal.  This file
;; folds only the literal characters of a regexp, so "caf.$" finds "café"
;; and "e+" matches "éèe", while operators, escapes, bracket expressions,
;; intervals and group numbers keep their regexp meaning.
;;
;; Entry points are `le::isearch-forward-regexp-char-fold' and
;; `le::isearch-backward-regexp-char-fold'.  They run isearch in its
;; non-regexp mode with `le::char-fold-regexp' as the regexp function, which
;; turns the typed string into the regexp actually searched.

;;; Code:

(require 'cl-lib)
(require 'char-fold)
(require 'isearch)

(defconst le::char-fold-regexp--escaped-literals ".*+?[^$\\"
  "Characters that a backslash turns into a literal, and so get folded.")

(defun le::char-fold-regexp--bracket-end (regexp start)
  "Return the index just past the bracket expression at START in REGEXP.
START indexes the opening \"[\".  An unterminated expression ends at
the end of REGEXP."
  (let ((len (length regexp))
        (j (1+ start)))
    (when (and (< j len) (eq (aref regexp j) ?^)) (cl-incf j))
    (when (and (< j len) (eq (aref regexp j) ?\])) (cl-incf j))
    (while (and (< j len) (not (eq (aref regexp j) ?\])))
      (if (and (eq (aref regexp j) ?\[)
               (< (1+ j) len)
               (eq (aref regexp (1+ j)) ?:))
          (setq j (if-let* ((close (string-search ":]" regexp (+ j 2))))
                      (+ close 2)
                    len))
        (cl-incf j)))
    (min (1+ j) len)))

(defun le::char-fold-regexp--through (regexp needle from)
  "Return the index just past NEEDLE in REGEXP at or after FROM.
Return the length of REGEXP when NEEDLE does not occur."
  (if-let* ((pos (string-search needle regexp from)))
      (+ pos (length needle))
    (length regexp)))

(defun le::char-fold-regexp (regexp &optional lax)
  "Return REGEXP with each of its literal characters character-folded.
Operators, backslash constructs, bracket expressions, intervals and
explicit group numbers are kept verbatim.  A run of literals is folded
as a whole, so ligatures such as \"ff\" -> \"ﬀ\" still match, except
that the character a postfix operator applies to is folded on its own.
LAX is passed to `char-fold-to-regexp' for whitespace handling."
  (let ((len (length regexp))
        (i 0)
        run out)
    (cl-flet* ((emit (piece) (push piece out))
               (flush ()
                 (when run
                   (emit (char-fold-to-regexp (concat (nreverse run)) lax))
                   (setq run nil)))
               (postfix (end)
                 (when run
                   (let ((last (pop run)))
                     (flush)
                     (emit (concat "\\(?:"
                                   (char-fold-to-regexp (string last) lax)
                                   "\\)"))))
                 (emit (substring regexp i end))
                 (setq i end))
               (verbatim (end)
                 (flush)
                 (emit (substring regexp i end))
                 (setq i end)))
      (while (< i len)
        (let ((c (aref regexp i))
              (n (and (< (1+ i) len) (aref regexp (1+ i)))))
          (cond
           ((and (eq c ?\\) (null n))
            (verbatim len))
           ((eq c ?\\)
            (cond
             ((memq n (string-to-list le::char-fold-regexp--escaped-literals))
              (push n run)
              (setq i (+ i 2)))
             ((eq n ?{)
              (postfix (le::char-fold-regexp--through regexp "\\}" i)))
             ((and (eq n ?\() (< (+ i 2) len) (eq (aref regexp (+ i 2)) ??))
              (verbatim (le::char-fold-regexp--through regexp ":" i)))
             ((memq n '(?_ ?s ?S ?c ?C))
              (verbatim (min (+ i 3) len)))
             (t (verbatim (+ i 2)))))
           ((eq c ?\[)
            (verbatim (le::char-fold-regexp--bracket-end regexp i)))
           ((memq c '(?* ?+ ??))
            (postfix (1+ i)))
           ((memq c '(?^ ?$ ?.))
            (verbatim (1+ i)))
           (t
            (push c run)
            (cl-incf i)))))
      (flush))
    (apply #'concat (nreverse out))))

(put 'le::char-fold-regexp 'isearch-message-prefix "char-fold regexp ")

;;;###autoload
(defun le::isearch-forward-regexp-char-fold (&optional no-recursive-edit)
  "Search forward for a regexp whose literal characters fold accents.
Typing \"e\" matches \"é\", \"è\" and so on, while regexp operators
keep their meaning.  NO-RECURSIVE-EDIT is as in `isearch-forward'."
  (interactive "p")
  (isearch-mode t nil nil (not no-recursive-edit) #'le::char-fold-regexp))

;;;###autoload
(defun le::isearch-backward-regexp-char-fold (&optional no-recursive-edit)
  "Search backward for a regexp whose literal characters fold accents.
See `le::isearch-forward-regexp-char-fold'.  NO-RECURSIVE-EDIT is as in
`isearch-backward'."
  (interactive "p")
  (isearch-mode nil nil nil (not no-recursive-edit) #'le::char-fold-regexp))

(provide 'le-isearch-char-fold)
;;; le-isearch-char-fold.el ends here
