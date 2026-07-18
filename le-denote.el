;;; le-denote.el --- Denote-file media capture shared by org and prompt2  -*- lexical-binding: t; -*-

;;; Commentary:

;; Clipboard media -> a denote-named file, factored into a kernel that two
;; callers share.
;;
;; `le::denote-write-media' is the kernel: given a directory, a MIME type, and
;; the raw selection data, it mints a denote identifier, picks the extension
;; from the MIME subtype, writes the bytes to <id>--<title>.<ext>, and returns
;; (PATH . ID).  It is scope-agnostic: the identifier's used-id "bump-a-second"
;; scan reads `denote-directory', so a caller that wants the scan scoped to a
;; particular silo simply `let'-binds `denote-directory' around the call.
;;
;; `le::denote-yank-media' is the org-facing command built on the kernel: it
;; reads a clipboard image, saves it BESIDE the current file (that file's own
;; directory, not `denote-directory'), and inserts a [[denote:<id>]] link at
;; point.  The link resolves wherever the current file's directory is covered
;; by the active `denote-directory' (i.e. inside a denote silo).
;;
;; `le-cci-prompt2's own `yank-media-handler' calls the same kernel and then
;; does its own two-sided referencing (an @-mention for the Claude CLI plus a
;; [[denote:<id>]] line in the log file); see le-cci-prompt2.el.

;;; Code:

(require 'denote)
(require 'yank-media)

(defun le::denote-write-media (dir mimetype data &optional title)
  "Write media DATA into DIR as a denote file; return (PATH . ID).
MIMETYPE is a symbol like `image/png'; its subtype picks the extension
\(with the svg+xml -> svg exception).  TITLE is the denote title component,
sluggified by denote (default \"screenshot\"), so the file is named
<id>--<title>.<ext>.  ID is minted by `denote-get-identifier-function', whose
used-id scan reads `denote-directory' -- `let'-bind it around this call to
scope the scan.  DATA is written verbatim (`no-conversion')."
  (let* ((dir (file-name-as-directory dir))   ; denote-format-file-name insists on a trailing /
         (id (funcall denote-get-identifier-function nil (current-time)))
         (subtype (cadr (split-string (symbol-name mimetype) "/")))
         (ext (concat "." (if (equal subtype "svg+xml") "svg" subtype)))
         (path (denote-format-file-name dir id nil (or title "screenshot") ext nil)))
    (let ((coding-system-for-write 'no-conversion))
      (write-region data nil path nil 'silent))
    (cons path id)))

;;;###autoload
(defun le::denote-yank-media (&optional prompt-title)
  "Paste a clipboard image beside the current file as a denote attachment.
Save the image as <id>--screenshot.<ext> in the current file's own directory
\(NOT `denote-directory') and insert a [[denote:<id>]] link at point.  With
PROMPT-TITLE (\\[universal-argument]), read a denote title to use in place of
\"screenshot\".

Signals a `user-error' if the buffer is not visiting a file, or if the
clipboard holds no image Emacs can handle.  The denote link resolves only
where the file's directory is covered by the active `denote-directory' (i.e.
inside a denote silo)."
  (interactive "P")
  (unless buffer-file-name
    (user-error "Buffer is not visiting a file"))
  (let ((type (car (funcall yank-media-autoselect-function
                            (yank-media--find-matching-media "image/.*")))))
    (unless type
      (user-error "No image on the clipboard"))
    (let* ((data (yank-media--get-selection type))
           (dir (file-name-directory buffer-file-name))
           (title (if prompt-title (read-string "Denote title: ") "screenshot"))
           ;; No `denote-directory' override: the silo's dir-local value scopes
           ;; both the id-uniqueness scan and the [[denote:id]] link's lookup.
           (id (cdr (le::denote-write-media dir type data title))))
      (insert (format "[[denote:%s]]" id)))))

(provide 'le-denote)
;;; le-denote.el ends here
