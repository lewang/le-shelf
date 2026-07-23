;;; le-org.el --- Org-mode utilities -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Le Wang
;; Keywords: org

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Org-mode helper functions: async link insertion with title fetching.

;;; Code:

(eval-when-compile
  (require 'org)                        ; org-with-wide-buffer macro
  (require 'dired))
(declare-function dired-get-marked-files "dired")
(declare-function org-attach-dir "org-attach")
(declare-function org-map-entries "org")
(defvar org-attach-id-dir)

(defun le::--extract-html-title ()
  "Extract page title from HTML in current buffer.
Tries og:title meta tag first, then <title> tag.  Handles HTML entities."
  (goto-char (point-min))
  (let ((case-fold-search t)
        raw)
    (cond
     ;; og:title — works on JS-heavy sites like Reddit
     ((re-search-forward
       "<meta[^>]*property=[\"']og:title[\"'][^>]*content=[\"']\\([^\"']*\\)[\"']" nil t)
      (setq raw (match-string 1)))
     ((progn (goto-char (point-min))
             (re-search-forward
              "<meta[^>]*content=[\"']\\([^\"']*\\)[\"'][^>]*property=[\"']og:title[\"']" nil t))
      (setq raw (match-string 1)))
     ;; aria-label="Post Title: ..." — Reddit specific fallback
     ((progn (goto-char (point-min))
             (re-search-forward "aria-label=\"Post Title: \\([^\"]*\\)\"" nil t))
      (setq raw (match-string 1)))
     ;; standard <title> tag
     ((progn (goto-char (point-min))
             (re-search-forward "<title[^>]*>\\([^<]*\\)</title>" nil t))
      (setq raw (match-string 1))))
    (when raw
      (setq raw (string-trim raw))
      ;; decode common HTML entities
      (setq raw (replace-regexp-in-string "&amp;" "&" raw))
      (setq raw (replace-regexp-in-string "&lt;" "<" raw))
      (setq raw (replace-regexp-in-string "&gt;" ">" raw))
      (setq raw (replace-regexp-in-string "&quot;" "\"" raw))
      (setq raw (replace-regexp-in-string "&#39;" "'" raw))
      (setq raw (replace-regexp-in-string "&#x27;" "'" raw))
      ;; escape org link brackets
      (setq raw (replace-regexp-in-string "\\[" "{{" raw))
      (setq raw (replace-regexp-in-string "\\]" "}}" raw))
      (if (string-empty-p raw) nil raw))))

(defvar le::--org-link-placeholder "...fetching title...")

;;;###autoload
(defun le::org-insert-link (&optional arg)
  "Insert an Org link.
Without a prefix, defer to `org-insert-link' (interactive target and
description; edits a link at point).  With a prefix ARG, insert a link
from the clipboard URL, asynchronously fetching the page title."
  (interactive "P")
  (if arg
      (le::--org-insert-link-from-clipboard)
    (call-interactively #'org-insert-link)))

(defun le::--org-insert-link-from-clipboard ()
  "Insert an Org link from the clipboard URL, async-fetching its <title>."
  (let ((url (current-kill 0)))
    (insert (format "[[%s][]]" url))
    (backward-char 2)
    (let ((marker (point-marker))
          (placeholder le::--org-link-placeholder))
      (insert placeholder)
      (let ((url-user-agent "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:128.0) Gecko/20100101 Firefox/128.0"))
        (url-retrieve
         url
         (lambda (_status marker url placeholder)
           (let ((title (condition-case nil
                            (le::--extract-html-title)
                          (error nil))))
             (when (buffer-live-p (marker-buffer marker))
               (with-current-buffer (marker-buffer marker)
                 (save-excursion
                   (goto-char marker)
                   (delete-region (point) (+ (point) (length placeholder)))
                   (insert (or title (replace-regexp-in-string "^https?://" "" url))))))
             (set-marker marker nil)))
         (list marker url placeholder)
         t t)))))

;;; Bulk-move Org files together with their org-attach data

(defun le::--org-file-attach-dirs (file)
  "Return a de-duplicated list of existing org-attach dirs referenced by FILE.
Each element is an absolute directory name (with trailing slash).  Visits
FILE if it is not already in a buffer, and kills that buffer afterwards
unless it was already open or got modified."
  (require 'org-attach)
  (let* ((existing (find-buffer-visiting file))
         (buf (or existing (find-file-noselect file)))
         dirs)
    (unwind-protect
        (with-current-buffer buf
          (org-with-wide-buffer
           (goto-char (point-min))
           (org-map-entries
            (lambda ()
              (let ((d (ignore-errors (org-attach-dir nil))))
                (when (and d (file-directory-p d))
                  (push (file-name-as-directory (expand-file-name d)) dirs)))))))
      (unless (or existing (buffer-modified-p buf))
        (kill-buffer buf)))
    (delete-dups dirs)))

;;;###autoload
(defun le::org-dired-move-with-attachments (target)
  "Move the marked Dired files to TARGET, taking org-attach data along.

For each marked Org file, attachment directories that live under the
file's own `data/' tree (per `org-attach-id-dir') are moved to the
matching location under TARGET, so ID-based attachment links keep
resolving.  Attachments held outside that tree -- e.g. an absolute
`:DIR:' property -- are left in place and reported.  Non-Org files are
moved as-is.  Any buffer visiting a moved file is re-pointed at its new
path.

Meant to be run from a Dired buffer; operates on the marked files (or the
file at point when nothing is marked)."
  (interactive
   (progn
     (unless (derived-mode-p 'dired-mode)
       (user-error "Run this in a Dired buffer"))
     (list (read-directory-name "Move marked files to directory: "))))
  (require 'org-attach)
  (let* ((files (dired-get-marked-files))
         (target (file-name-as-directory (expand-file-name target)))
         (moved-files 0)
         (moved-dirs 0)
         (left-dirs nil)
         (errors nil))
    (unless files (user-error "No marked files"))
    ;; Pre-scan so we can show a summary and confirm before touching disk.
    (let ((plan (mapcar
                 (lambda (f)
                   (cons f (and (string-match-p "\\.org\\'" f)
                                (le::--org-file-attach-dirs f))))
                 files)))
      (unless (yes-or-no-p
               (format "Move %d file(s) and %d attachment dir(s) to %s? "
                       (length plan)
                       (apply #'+ (mapcar (lambda (x) (length (cdr x))) plan))
                       (abbreviate-file-name target)))
        (user-error "Aborted"))
      (unless (file-directory-p target) (make-directory target t))
      (dolist (entry plan)
        (let* ((file (expand-file-name (car entry)))
               (src-dir (file-name-directory file))
               (attach-root (file-name-as-directory
                             (expand-file-name org-attach-id-dir src-dir)))
               (dest-file (expand-file-name (file-name-nondirectory file) target)))
          (condition-case err
              (progn
                (when (file-exists-p dest-file)
                  (error "Destination exists: %s" dest-file))
                ;; 1. Move attachment dirs belonging to this file's data tree.
                (dolist (d (cdr entry))
                  (if (file-in-directory-p d attach-root)
                      (let* ((rel (file-relative-name (directory-file-name d) src-dir))
                             (dest (expand-file-name rel target)))
                        (make-directory (file-name-directory (directory-file-name dest)) t)
                        (rename-file (directory-file-name d) (directory-file-name dest))
                        (setq moved-dirs (1+ moved-dirs)))
                    (push d left-dirs)))
                ;; 2. Move the file, keeping any visiting buffer in sync.
                (let ((buf (find-buffer-visiting file)))
                  (rename-file file dest-file)
                  (when buf
                    (with-current-buffer buf
                      (set-visited-file-name dest-file t t))))
                (setq moved-files (1+ moved-files)))
            (error (push (format "%s: %s"
                                 (file-name-nondirectory file)
                                 (error-message-string err))
                         errors)))))
      (when (derived-mode-p 'dired-mode) (revert-buffer))
      (message "Moved %d file(s), %d attachment dir(s)%s%s"
               moved-files moved-dirs
               (if left-dirs
                   (format "; left %d external attachment dir(s) in place"
                           (length (delete-dups left-dirs)))
                 "")
               (if errors
                   (format "; %d error(s): %s"
                           (length errors)
                           (mapconcat #'identity (nreverse errors) "; "))
                 "")))))

(provide 'le-org)
;;; le-org.el ends here
