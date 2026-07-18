;;; le-cci-prompt2-mention.el --- CCI prompt editor: @-mention capture  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Le Wang
;; Keywords: tools
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Active-region / point-subject capture for the Claude Code prompt
;; composer (see `le-cci-prompt2'): turn the source buffer's region (or
;; the enclosing TODO/DOING heading) into a Claude Code "@PATH#RANGE"
;; @-mention plus a "re: SUBJECT" line, and compose the edit buffer's
;; initial content around them.

;;; Code:

(require 'org)

(defun le::cci-prompt2--capture-region-ref ()
  "If the current buffer has an active region and visits a file,
return (:file F :beg B :end E :subject S) with 1-based inclusive line numbers,
else nil.  FILE may not yet exist on disk (e.g. an unsaved buffer) — the
caller is responsible for offering to save it before referencing it.
SUBJECT is the org heading title of the entry containing the region
start (stars, TODO keyword, priority, and tags stripped), or nil when the
buffer is not org-mode or the region starts above the first heading."
  (when (and (use-region-p) buffer-file-name)
    (let* ((rb (region-beginning))
           (re (region-end))
           (beg (line-number-at-pos rb))
           (end (line-number-at-pos re))
           (subject (when (derived-mode-p 'org-mode)
                      (save-excursion
                        (goto-char rb)
                        (unless (org-before-first-heading-p)
                          (org-back-to-heading t)
                          (let ((h (string-trim
                                    (substring-no-properties
                                     (org-get-heading t t t t)))))
                            (unless (string-empty-p h) h)))))))
      ;; If the region ends exactly at a line start, that trailing line isn't
      ;; really selected — exclude it (standard idiom).
      (when (and (> re rb) (save-excursion (goto-char re) (bolp)))
        (setq end (1- end)))
      (list :file buffer-file-name :beg beg :end end :subject subject))))

(defun le::cci-prompt2--point-todo-doing-ancestors ()
  "Return (SUBJECT . POINT) for each heading enclosing point whose TODO
keyword is TODO or DOING, closest first.  SUBJECT has stars, keyword,
priority, and tags stripped; POINT is the heading's buffer position,
used to derive a location reference to it.  Walks from the heading
containing point up through each ancestor to the top, filtering to
just the qualifying ones -- an ancestor without a TODO/DOING keyword is
skipped but does not stop the walk.  Empty (nil) when the buffer isn't
org-mode, point is above the first heading, or no ancestor qualifies."
  (when (and (derived-mode-p 'org-mode) (not (org-before-first-heading-p)))
    (save-excursion
      (org-back-to-heading t)
      (let (candidates)
        (while (progn
                 (when (member (org-get-todo-state) '("TODO" "DOING"))
                   (push (cons (string-trim
                                (substring-no-properties
                                 (org-get-heading t t t t)))
                               (point))
                         candidates))
                 (org-up-heading-safe)))
        (nreverse candidates)))))

(defun le::cci-prompt2--capture-point-subject ()
  "Return a region-ref-shaped plist for the TODO/DOING heading enclosing
point, or nil if none qualifies.  When more than one ancestor heading
qualifies, prompts for which one, defaulting to the closest.  Includes
:file/:beg/:end (the heading's own line, so the eventual @-mention
points straight at it) when the buffer visits a file, else just
:subject.  Used as the fallback to `le::cci-prompt2--capture-region-ref'
when no region is active, so a \"re: SUBJECT\" line and a location
reference still get added based on what task point is currently in."
  (when-let* ((candidates (le::cci-prompt2--point-todo-doing-ancestors)))
    (let* ((chosen (if (cdr candidates)
                        (completing-read "Subject heading: " candidates nil t
                                         nil nil (caar candidates))
                      (caar candidates)))
           (pos (cdr (assoc chosen candidates))))
      (if buffer-file-name
          (let ((line (line-number-at-pos pos)))
            (list :file buffer-file-name :beg line :end line :subject chosen))
        (list :subject chosen)))))

(defun le::cci-prompt2--file-reference (file beg end root)
  "Return the Claude Code @-mention \"@PATH#RANGE\" for FILE lines BEG..END.
PATH is relative to ROOT when FILE is under it, absolute otherwise.
RANGE collapses to a single number when BEG = END."
  (let* ((abs (expand-file-name file))
         (root* (file-name-as-directory (expand-file-name root)))
         (path (if (string-prefix-p root* abs) (substring abs (length root*)) abs))
         (range (if (= beg end) (number-to-string beg) (format "%d-%d" beg end))))
    (format "@%s#%s" path range)))

(defun le::cci-prompt2--compose-prompt-content (base-text region-ref)
  "Return (STRING . POINT) for a prompt buffer's initial content.
BASE-TEXT seeds the composing line (e.g. captured CLI prompt text, an
existing draft, or the empty string).  REGION-REF, when non-nil, is a
plist as returned by `le::cci-prompt2--capture-region-ref' with an
added :ref-string key (the already-formatted @-mention) — its
:subject, if any, becomes a leading \"re: SUBJECT\" line, and
:ref-string is appended after BASE-TEXT (trimmed of trailing
whitespace) separated by a blank line.  POINT lands on the composing
line: after the \"re:\" line when present, otherwise at the top; with
no REGION-REF, POINT is at the end of BASE-TEXT."
  (if region-ref
      (let* ((subject (plist-get region-ref :subject))
             (ref (plist-get region-ref :ref-string))
             (heading (if subject (concat "re: " subject "\n\n") ""))
             (body (concat heading (string-trim-right base-text) "\n\n" ref)))
        (cons body (1+ (length heading))))
    (cons base-text (1+ (length base-text)))))

(provide 'le-cci-prompt2-mention)
;;; le-cci-prompt2-mention.el ends here
