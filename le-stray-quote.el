;;; le-stray-quote.el --- Locate an unbalanced double-quote -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Le Wang
;; Keywords: convenience
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Find a stray, unmatched double-quote in a buffer.

;; When `"' has string syntax (text and Org buffers), a single unmatched
;; quote flips the parser's in-string state for the rest of the buffer, so
;; `electric-pair-mode' starts inserting a lone `"' instead of a pair, and the
;; confusion persists until the quote count is even again.
;;
;; `"' is a symmetric delimiter, so counting alone can't say which quote lost
;; its partner -- any odd-indexed quote splits the buffer into even-before and
;; even-after.  But a bogus "string" gives itself away: it swallows a blank
;; line or an Org heading, which real quoted prose never spans.  Once one
;; quote is unmatched, every gap after it is mis-parsed as a string, so those
;; structure-crossing spans cascade from the stray quote onward -- and the
;; first one sits right at it.

;;; Code:

(defun le::find-stray-quote--suspect-spans ()
  "Return suspicious `\"'-delimited string spans in the accessible buffer.
Each element is (BEG . END) for a string, per the buffer syntax, whose
text swallows a blank line or an Org heading -- the usual fallout of an
unmatched quote earlier in the buffer."
  (save-excursion
    (syntax-ppss-flush-cache (point-min))
    (goto-char (point-min))
    (let ((state nil) spans)
      (while (< (point) (point-max))
        (let ((p (point))
              (new (parse-partial-sexp (point) (point-max) nil nil state 'syntax-table)))
          (when (and (nth 3 state) (not (nth 3 new)))
            (let ((beg (nth 8 state)) (end (point)))
              (when (string-match-p "\n[ \t]*\n\\|\n\\*+ "
                                    (buffer-substring-no-properties beg end))
                (push (cons beg end) spans))))
          (setq state new)
          (when (= (point) p) (forward-char 1))))     ; never stall
      (nreverse spans))))

;;;###autoload
(defun le::find-stray-quote ()
  "Find and jump to an unbalanced double-quote in the current buffer.
Report the total `\"' count and its parity, then move point to the first
string span that swallows a blank line or an Org heading -- the tell-tale
of a stray quote when `\"' has string syntax.  Widens first; any narrowing
is restored afterwards."
  (interactive)
  (save-restriction
    (widen)
    (let* ((n (save-excursion (how-many "\"" (point-min) (point-max))))
           (oddp (= 1 (% n 2)))
           (parity (if oddp "ODD -- one quote is unmatched" "even"))
           (spans (le::find-stray-quote--suspect-spans)))
      (cond
       (spans
        (let ((beg (caar spans)))
          (push-mark)
          (goto-char beg)
          (when (fboundp 'pulse-momentary-highlight-one-line)
            (pulse-momentary-highlight-one-line (point)))
          (message "%d quotes (%s); %d suspect span(s), jumped to first (line %d)."
                   n parity (length spans) (line-number-at-pos beg))))
       (oddp
        (message "%d quotes (ODD) but no blank-line/heading-crossing span.  \
Binary-search: narrow to a half and re-run `how-many' on the quotes." n))
       (t
        (message "%d quotes (even), no suspect spans -- quotes look balanced." n))))))

(provide 'le-stray-quote)
;;; le-stray-quote.el ends here
