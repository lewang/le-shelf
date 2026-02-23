;;; le-markdown.el --- Markdown yasnippet helpers -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Le Wang
;; Keywords: markdown

;; This file is not part of GNU Emacs.

;;; Commentary:

;; YASnippet helper functions for reference-style markdown links and images.

;;; Code:

(require 'cl-lib)

(cl-defun yas-generate-sandwich (&key prefix (chunk-end (point-max)) suffix (suffix-force-newline t))
  "Generate a snippet that wraps buffer text from point to CHUNK-END.
The \"meat\" of the sandwich is the chunk of text sucked in from the buffer.
1. Delete buffer between point and CHUNK-END.
2. Expand snippet: PREFIX + chunk-string + SUFFIX.

This is useful when you want to generate a snippet on the fly
which contains a chunk of the buffer."
  (let ((chunk-end-marker (make-marker))
        chunk)
    (set-marker chunk-end-marker chunk-end)
    (set-marker-insertion-type chunk-end-marker t)
    (when suffix-force-newline
      (save-excursion
        (goto-char chunk-end-marker)
        (if (or (not (bolp))
                (eq (point-max) (point-min)))
            (insert "\n"))))
    (setq chunk (replace-regexp-in-string "`"
                                          "\\`"
                                          (buffer-substring (point) chunk-end-marker)
                                          nil
                                          t))
    (delete-region (point) chunk-end-marker)
    (let ((yas-indent-line nil))
      (yas-expand-snippet (concat prefix chunk suffix)))))

;;;###autoload
(defun yas-markdown-link-by-ref ()
  "Expand a snippet for inserting a reference-style link.
Inserts [text][id] at point and [id]: URL at end of document.

With region selected, use region as link text and ask only for the URL.
With universal prefix-arg, insert image instead."
  (interactive)
  (if (consp current-prefix-arg)
      (yas-markdown-insert-image-by-ref)
    (yas-generate-sandwich :prefix (format "[%s][%s]$0"
                                           (or yas-selected-text "${3:text}")
                                           (if yas-selected-text
                                               ""
                                             "${2:id}"))
                           :suffix (format "[%s]: ${1:http://yahoo.com}%s"
                                           (or yas-selected-text "$2")
                                           (if yas-selected-text
                                               ""
                                             "${4: \"${5:title}\"}")))))

;;;###autoload
(defun yas-markdown-insert-image-by-ref ()
  "Expand a snippet for inserting a reference-style image.
Inserts ![alt][id] at point and [id]: URL at end of document.
\"alt/title\" should always be specified."
  (interactive)
  (yas-generate-sandwich :prefix (format "![%s][%s]$0"
                                         (or yas-selected-text "${3:alt}")
                                         (or yas-selected-text "${2:id}"))
                         :suffix (format "[%s]: ${1:http://foo.com/foo.png} \"%s\""
                                         (or yas-selected-text "$2")
                                         (or yas-selected-text "${4:title}"))))

(provide 'le-markdown)

;;; le-markdown.el ends here
