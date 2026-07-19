;;; le-denote.el --- Denote helpers: media capture + org-subtree export  -*- lexical-binding: t; -*-

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
(require 'org)

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

;;;; Export an org subtree into denote task files

;; `le::denote-export-subtree' relocates already-coded task nodes out of a
;; journal subtree into their own files under <silo>/task/, one file per
;; code=path (node-per-file fan-out).  Each new file keeps the org heading
;; verbatim (promoted to level 1) plus its LOGBOOK and body; #+title is the
;; heading's description and #+signature is its code=path.  It is the inverse
;; of `denote-org-extract-org-subtree', which demotes the heading into #+title
;; and drops the TODO state.  The tool never invents identity: a task node
;; without a code=path aborts the whole run.
;;
;; Functional core (`le::denote-export--scan') returns a plan with no side
;; effects; the interactive shell previews it, confirms, then creates files
;; (flushed to disk) BEFORE deleting the source subtree.

(defun le::denote-export--parse-code (headline)
  "Split HEADLINE \"code=path description\" into (CODE . DESC).
CODE is a code=path id: a dash-free [a-z0-9]+ stem followed by at least one
=<integer> segment, so an uncoded headline like \"refactor parser\" returns
nil.  DESC is the remaining text (empty string when absent).  Return nil when
HEADLINE carries no code=path."
  (when (string-match
         (rx bos
             (group (+ (any "a-z0-9")) (+ "=" (+ (any "0-9"))))
             (opt (+ (any " \t")) (group (* nonl)))
             eos)
         headline)
    (cons (match-string 1 headline)
          (or (match-string 2 headline) ""))))

(defun le::denote-export--promote (text)
  "Promote org subtree TEXT so its top heading sits at level 1.
Strip (top-level - 1) leading stars from every heading line, shifting the
whole subtree up uniformly.  Descendant headings stay strictly below their
new level-1 root."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (let ((drop (if (looking-at "\\*+") (1- (length (match-string 0))) 0)))
      (when (> drop 0)
        (while (re-search-forward "^\\(\\*+\\) " nil t)
          (replace-match
           (concat (make-string (max 1 (- (length (match-string 1)) drop)) ?*) " ")
           t t))))
    ;; Collapse the blank runs left by cut task children, and end in one newline.
    (concat (string-trim-right
             (replace-regexp-in-string "\n\\{3,\\}" "\n\n" (buffer-string)))
            "\n")))

(defun le::denote-export--deepest-task ()
  "Return the position of a maximum-level TODO heading in the buffer, or nil."
  (let (best best-level)
    (org-map-entries
     (lambda ()
       (when (org-get-todo-state)
         (let ((lvl (org-current-level)))
           (when (or (null best-level) (> lvl best-level))
             (setq best-level lvl best (point)))))))
    best))

(defun le::denote-export--extract-contents (subtree-text)
  "Return an alist (CODE . CONTENT) for each task node in SUBTREE-TEXT.
CONTENT is the node's subtree promoted to level 1 -- its own heading, drawers
and body plus any non-task descendants, but not its task descendants, which
are cut first (deepest-first) into their own entries."
  (let ((result '()))
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      (insert subtree-text)
      (let (target)
        (while (setq target (le::denote-export--deepest-task))
          (goto-char target)
          (org-back-to-heading t)
          (let* ((code (car (le::denote-export--parse-code (org-get-heading t t t t))))
                 (beg (point))
                 (end (save-excursion (org-end-of-subtree t) (point)))
                 (text (buffer-substring-no-properties beg end)))
            (push (cons code (le::denote-export--promote text)) result)
            (delete-region beg end)))))
    result))

(defun le::denote-export--scan ()
  "Scan the subtree at point and return an export plan plist.
Point must be on the subtree root heading.  The plan has:
  :region  (BEG . END) of the source subtree in the current buffer;
  :uncoded a list of headline strings for task nodes lacking a code=path;
  :nodes   node plists (:code :state :title :keywords :content), present only
           when :uncoded is empty.
No side effects: the content pass runs on a copy of the subtree text."
  (org-back-to-heading t)
  (let* ((beg (org-entry-beginning-position))
         (end (save-excursion (org-end-of-subtree t) (point)))
         (subtree-text (buffer-substring-no-properties beg end))
         (meta '())
         (uncoded '()))
    (org-map-entries
     (lambda ()
       (when (org-get-todo-state)
         (let* ((headline (org-get-heading t t t t))
                (parsed (le::denote-export--parse-code headline)))
           (if parsed
               (push (list :code (car parsed)
                           :state (org-get-todo-state)
                           :title (cdr parsed)
                           :keywords (mapcar #'substring-no-properties (org-get-tags)))
                     meta)
             (push headline uncoded)))))
     nil 'tree)
    (setq meta (nreverse meta)
          uncoded (nreverse uncoded))
    (if uncoded
        (list :region (cons beg end) :uncoded uncoded)
      (let ((contents (le::denote-export--extract-contents subtree-text)))
        (list :region (cons beg end)
              :nodes (mapcar
                      (lambda (m)
                        (append m (list :content
                                        (cdr (assoc (plist-get m :code) contents)))))
                      meta))))))

(defun le::denote-export--preview (nodes task-dir)
  "Show, in *denote-export*, the files NODES will create in TASK-DIR.
The buffer is in `special-mode', so `q' buries it."
  (with-current-buffer (get-buffer-create "*denote-export*")
    (special-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (format "denote export: %d task node(s) -> %s\n\n"
                      (length nodes) task-dir))
      (dolist (node nodes)
        (insert (format "  %-16s ->  %s\n"
                        (plist-get node :code)
                        (file-name-nondirectory
                         (denote-format-file-name
                          task-dir
                          (funcall denote-get-identifier-function nil (current-time))
                          (plist-get node :keywords)
                          (or (plist-get node :title) "")
                          ".org"
                          (plist-get node :code))))))
      (insert "\n(identifiers above are illustrative; real ones are minted at creation)\n")
      (insert "The source subtree will then be deleted from the journal buffer.\n"))
    (goto-char (point-min))
    (display-buffer (current-buffer))))

;;;###autoload
(defun le::denote-export-subtree ()
  "Export the org subtree at point into denote task files, one per code=path.
Point must be on a task-state heading in a buffer under a denote silo.  Every
heading in the subtree carrying a TODO state is a node; each becomes a file in
<`denote-directory'>/task/ named by its code=path, keeping the heading verbatim
\(promoted to level 1), its LOGBOOK and body, and any non-task descendants.

Refuse the whole run if any task node lacks a code=path.  Preview first; when
more than one file will be created, confirm before writing.  Files are created
and flushed to disk BEFORE the source subtree is deleted from the buffer."
  (interactive)
  (unless (and (derived-mode-p 'org-mode) (org-at-heading-p) (org-get-todo-state))
    (user-error "Point must be on a task-state heading"))
  (let* ((journal-buf (current-buffer))
         (plan (le::denote-export--scan))
         (uncoded (plist-get plan :uncoded)))
    (when uncoded
      (user-error "Refusing to export: %d uncoded task node(s): %s"
                  (length uncoded) (string-join uncoded "; ")))
    (let* ((nodes (plist-get plan :nodes))
           (region (plist-get plan :region))
           (task-dir (file-name-as-directory
                      (expand-file-name "task" (car (denote-directories))))))
      (unless nodes (user-error "No task nodes to export"))
      (le::denote-export--preview nodes task-dir)
      (let ((proceed (or (= (length nodes) 1)
                         (yes-or-no-p (format "Export %d task nodes into %s? "
                                              (length nodes) task-dir)))))
        ;; Dismiss the preview once the user has answered (bury, don't kill).
        (quit-windows-on "*denote-export*")
        (when proceed
          (make-directory task-dir t)
          ;; `denote' opens each new file with `find-file'; keep that churn from
          ;; disturbing the user's window layout.
          (save-window-excursion
            (let ((denote-kill-buffers t))
              (dolist (node nodes)
                ;; `denote' drifts `current-buffer' (find-file then kill), so
                ;; re-assert the journal buffer each call: it carries the silo's
                ;; buffer-local `denote-directory', which is what validates
                ;; TASK-DIR.  Without this, calls after the first fall back to the
                ;; global `denote-directory' and land in the wrong silo.
                (with-current-buffer journal-buf
                  (denote (or (plist-get node :title) "")
                          (plist-get node :keywords)
                          'org
                          task-dir
                          nil
                          (plist-get node :content)
                          (plist-get node :code))))))
          (with-current-buffer journal-buf
            (delete-region (car region) (cdr region))
            (save-buffer))
          (message "Exported %d task node(s) to %s" (length nodes) task-dir))))))

(provide 'le-denote)
;;; le-denote.el ends here
