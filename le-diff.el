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
(declare-function diff-hunk-text "diff-mode")
(declare-function smerge-find-conflict "smerge-mode")
(declare-function smerge-refine "smerge-mode")

;;;###autoload
(defun le::diff-view-as-smerge ()
  "View the current diff as smerge conflicts in a buffer with full file context.

Creates a new buffer starting from the original (pre-diff) file
content with conflict markers for each hunk.  Enables
`smerge-mode' with word-level refinement.

The buffer uses the old side of the diff as base content and
shows the new side in the MODIFIED section of each conflict.

Invoke from a `diff-mode' buffer."
  (interactive)
  (unless (derived-mode-p 'diff-mode)
    (user-error "Not in diff-mode"))
  (let* ((file (diff-find-file-name nil t))
         (source-buf (find-file-noselect file))
         (hunks nil))
    ;; Collect hunks: (old-start old-len old-text new-text)
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
                       (hunk-text (buffer-substring
                                   (point)
                                   (save-excursion (diff-end-of-hunk) (point))))
                       (old-text (diff-hunk-text hunk-text nil nil))
                       (new-text (diff-hunk-text hunk-text t nil)))
                  (push (list old-start old-len old-text new-text) hunks)))
              (diff-hunk-next)))
        (error nil)))
    (unless hunks
      (user-error "No hunks found"))
    ;; Reverse hunks to forward order for building the buffer sequentially.
    (setq hunks (nreverse hunks))
    (let ((smerge-buf (generate-new-buffer
                       (format "*smerge:%s*" (file-name-nondirectory file))))
          (src-line 1))
      (with-current-buffer smerge-buf
        ;; Build buffer: copy unchanged lines from source, insert conflicts.
        (with-current-buffer source-buf
          (save-excursion
            (goto-char (point-min))
            (dolist (hunk hunks)
              (pcase-let ((`(,start ,len ,old-text ,new-text) hunk))
                ;; Copy unchanged lines before this hunk
                (let ((skip (- start src-line)))
                  (when (> skip 0)
                    (let ((beg (point)))
                      (forward-line skip)
                      (let ((text (buffer-substring beg (point))))
                        (with-current-buffer smerge-buf
                          (insert text))))))
                ;; Skip old lines in source
                (forward-line len)
                (setq src-line (+ start len))
                ;; Insert conflict markers
                (with-current-buffer smerge-buf
                  (insert "<<<<<<< ORIGINAL\n"
                          old-text
                          "=======\n"
                          new-text
                          ">>>>>>> MODIFIED\n"))))
            ;; Copy remaining lines after last hunk
            (let ((rest (buffer-substring (point) (point-max))))
              (with-current-buffer smerge-buf
                (insert rest)))))
        (delay-mode-hooks
          (funcall (buffer-local-value 'major-mode source-buf)))
        (smerge-mode 1)
        (goto-char (point-min))
        (while (smerge-find-conflict)
          (smerge-refine))
        (goto-char (point-min)))
      (pop-to-buffer smerge-buf))))

(provide 'le-diff)

;;; le-diff.el ends here
