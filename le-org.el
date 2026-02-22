;;; le-org.el --- Org-mode utilities -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Le Wang
;; Keywords: org

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Org-mode helper functions: async link insertion with title fetching.

;;; Code:

;;;###autoload
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
(defun le::org-insert-link ()
  "Insert org link from clipboard URL, asynchronously fetching the page title."
  (interactive)
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

(provide 'le-org)
;;; le-org.el ends here
