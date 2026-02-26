;;; le-diff.el --- Diff viewing utilities -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Le Wang
;; Keywords: vc, diff
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; View unified diffs as smerge conflicts with full file context and
;; word-level refinement highlighting.

;;; Code:

(declare-function diff-find-file-name "diff-mode")
(declare-function diff-beginning-of-hunk "diff-mode")
(declare-function diff-end-of-hunk "diff-mode")
(declare-function diff-hunk-next "diff-mode")
(declare-function smerge-find-conflict "smerge-mode")
(declare-function smerge-refine "smerge-mode")
(declare-function magit-file-at-point "magit-diff")
(declare-function magit-commit-at-point "magit-git")

(defun le::diff--parse-hunk-changes (hunk-start hunk-end)
  "Parse unified diff lines between HUNK-START and HUNK-END.
Returns a list of change records in order:
  (context LINE-TEXT)     — a context line
  (change OLD-LINES NEW-LINES) — a group of consecutive -/+ lines
where OLD-LINES and NEW-LINES are each a string (possibly empty)."
  (save-excursion
    (goto-char hunk-start)
    ;; Skip the @@ header line
    (forward-line 1)
    (let ((changes nil)
          (old-accum "")
          (new-accum "")
          (in-change nil))
      (while (< (point) hunk-end)
        (let* ((line-start (point))
               (ch (char-after))
               (line-end (line-end-position))
               (content (buffer-substring (1+ line-start) line-end)))
          (pcase ch
            (?-
             (setq in-change t)
             (setq old-accum (concat old-accum content "\n")))
            (?+
             (setq in-change t)
             (setq new-accum (concat new-accum content "\n")))
            (_  ; context line (space) or other
             (when in-change
               (push (list 'change old-accum new-accum) changes)
               (setq old-accum "" new-accum "" in-change nil))
             (push (list 'context (concat content "\n")) changes))))
        (forward-line 1))
      ;; Flush any trailing change
      (when in-change
        (push (list 'change old-accum new-accum) changes))
      (nreverse changes))))

(defun le::diff--collect-hunks (diff-buf)
  "Collect parsed hunks from DIFF-BUF (must be in `diff-mode').
Returns list of (OLD-START OLD-LEN CHANGES) in forward order."
  (let ((hunks nil))
    (with-current-buffer diff-buf
      (save-excursion
        (goto-char (point-min))
        (condition-case nil
            (progn
              (diff-beginning-of-hunk t)
              (while t
                (when (looking-at "^@@ -\\([0-9]+\\),?\\([0-9]*\\)")
                  (let* ((old-start (string-to-number (match-string 1)))
                         (old-len-str (match-string 2))
                         (old-len (if (string-empty-p old-len-str) 1
                                    (string-to-number old-len-str)))
                         (hunk-beg (point))
                         (hunk-end (save-excursion (diff-end-of-hunk) (point)))
                         (changes (le::diff--parse-hunk-changes hunk-beg hunk-end)))
                    (push (list old-start old-len changes) hunks)))
                (diff-hunk-next)))
          (error nil))))
    (or (nreverse hunks)
        (user-error "No hunks found"))))

(defun le::diff--build-smerge-buffer (source-buf hunks buf-name)
  "Build a smerge buffer named BUF-NAME from SOURCE-BUF and HUNKS.
SOURCE-BUF should contain the old (pre-diff) file content.
HUNKS is a list from `le::diff--collect-hunks'.
Returns the smerge buffer."
  (let ((smerge-buf (generate-new-buffer buf-name))
        (src-line 1))
    (with-current-buffer source-buf
      (save-excursion
        (goto-char (point-min))
        (dolist (hunk hunks)
          (pcase-let ((`(,start ,len ,changes) hunk))
            (let ((skip (- start src-line)))
              (when (> skip 0)
                (let ((beg (point)))
                  (forward-line skip)
                  (princ (buffer-substring beg (point)) smerge-buf))))
            (dolist (change changes)
              (pcase change
                (`(context ,text)
                 (forward-line 1)
                 (princ text smerge-buf))
                (`(change ,old-text ,new-text)
                 (let ((n-old (if (string-empty-p old-text) 0
                                (length (split-string old-text "\n" t)))))
                   (forward-line n-old)
                   (princ (concat "<<<<<<< ORIGINAL\n"
                                  old-text
                                  "=======\n"
                                  new-text
                                  ">>>>>>> MODIFIED\n")
                          smerge-buf)))))
            (setq src-line (+ start len))))
        (princ (buffer-substring (point) (point-max)) smerge-buf)))
    (with-current-buffer smerge-buf
      (delay-mode-hooks
        (funcall (buffer-local-value 'major-mode source-buf)))
      (font-lock-mode 1)
      (smerge-mode 1)
      (goto-char (point-min))
      (while (smerge-find-conflict)
        (smerge-refine))
      (goto-char (point-min)))
    smerge-buf))

;;;###autoload
(defun le::diff-view-as-smerge ()
  "View the current diff as smerge conflicts in a buffer with full file context.

Creates a new buffer from the original (pre-diff) file content
with per-change conflict markers (no context lines inside
conflicts).  Enables `smerge-mode' with word-level refinement.

Invoke from a `diff-mode' buffer."
  (interactive)
  (unless (derived-mode-p 'diff-mode)
    (user-error "Not in diff-mode"))
  (let* ((file (diff-find-file-name nil t))
         (source-buf (find-file-noselect file))
         (hunks (le::diff--collect-hunks (current-buffer)))
         (smerge-buf (le::diff--build-smerge-buffer
                      source-buf hunks
                      (format "*smerge:%s*" (file-name-nondirectory file)))))
    (pop-to-buffer smerge-buf)))

;;;###autoload
(defun le::magit-diff-view-as-smerge ()
  "View the file at point as smerge conflicts with full file context.

Gets the commit and file from the current magit section,
generates a diff, and displays it as smerge conflicts with
word-level refinement.

Invoke from a `magit-revision-mode' or `magit-diff-mode' buffer."
  (interactive)
  (unless (derived-mode-p 'magit-diff-mode)
    (user-error "Not in a magit diff buffer"))
  (let* ((file (or (magit-file-at-point)
                   (user-error "No file at point")))
         (rev (magit-commit-at-point))
         (default-directory (magit-toplevel))
         (diff-buf (generate-new-buffer " *magit-smerge-diff*"))
         (source-buf (generate-new-buffer " *magit-smerge-src*")))
    (unwind-protect
        (progn
          ;; Generate unified diff
          (with-current-buffer diff-buf
            (if rev
                (call-process "git" nil t nil
                              "diff" (concat rev "~1") rev "--" file)
              ;; Unstaged diff in magit-diff-mode
              (call-process "git" nil t nil "diff" "--" file))
            (diff-mode))
          ;; Get old file content
          (with-current-buffer source-buf
            (if rev
                (call-process "git" nil t nil
                              "show" (concat rev "~1:" file))
              (insert-file-contents (expand-file-name file)))
            ;; Set major mode based on filename
            (let ((buffer-file-name (expand-file-name file)))
              (delay-mode-hooks (set-auto-mode))))
          (let* ((hunks (le::diff--collect-hunks diff-buf))
                 (smerge-buf (le::diff--build-smerge-buffer
                              source-buf hunks
                              (format "*smerge:%s*"
                                      (file-name-nondirectory file)))))
            (pop-to-buffer smerge-buf)))
      (kill-buffer diff-buf)
      (kill-buffer source-buf))))

(with-eval-after-load 'magit-diff
  (define-key magit-diff-mode-map (kbd "C-c s") #'le::magit-diff-view-as-smerge))

(provide 'le-diff)

;;; le-diff.el ends here
