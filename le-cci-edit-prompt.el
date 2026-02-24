;;; le-cci-edit-prompt.el --- Claude Code prompt editor with history  -*- lexical-binding: t; -*-

;;; Commentary:
;; Opens a buffer for composing Claude Code prompts.
;; M-p/M-n traverse history from the latest session JSONL file,
;; lazy-loading in batches of 5.  C-c C-c to accept, C-c C-k to cancel.
;;
;; Prompts entered via C-c C-c are saved to a per-project ring as
;; (TIMESTAMP . TEXT) cons cells.  JSONL entries also carry timestamps.
;; History is a timestamp-based merge sort of both sources with dedup.
;; The ring acts as a write-ahead buffer; entries confirmed in JSONL
;; are lazily purged during merge.

;;; Code:

(require 'json)
(require 'cl-lib)
(require 'time-date)

;;;; Session ID stack (set as buffer-local in cci vterm buffers)

(defvar-local le::cci--session-id-stack nil
  "Stack of Claude CLI session IDs, most recent first.
Populated by SessionStart hook via emacsclient.
Set in the cci vterm buffer, not the prompt buffer.")

(defvar claude-code-ide-mcp-server--sessions)
(defvar claude-code-ide--routing-token)
(defvar claude-code-ide--routing-tokens)

(defun le::cci--register-session-id (routing-token session-id)
  "Push SESSION-ID onto the session ID stack for the buffer matching ROUTING-TOKEN.
Returns non-nil on success, nil if no matching buffer found.
Called by the SessionStart hook via emacsclient."
  (if-let* ((session (gethash routing-token claude-code-ide-mcp-server--sessions))
            (buf (plist-get session :buffer))
            ((buffer-live-p buf)))
      (with-current-buffer buf
        (unless (equal session-id (car le::cci--session-id-stack))
          (push session-id le::cci--session-id-stack))
        t)
    (message "le::cci--register-session-id: no buffer for routing token %s" routing-token)
    nil))

;;;; Per-project prompt ring (global, not buffer-local)

(defvar le::cci--prompt-rings (make-hash-table :test 'equal)
  "Hash table mapping expanded project roots to lists of (TIMESTAMP . TEXT) cons cells.
Most recent first.  These are prompts entered via C-c C-c.
Timestamps are integer seconds from `float-time'.")

(defvar le::cci--prompt-ring-max-size 100
  "Maximum number of entries per project in the prompt ring.")

(defun le::cci--ring-push (project-root text)
  "Push TEXT onto the prompt ring for PROJECT-ROOT with current timestamp.
Deduplicates: if TEXT is already the most recent entry, skip."
  (let* ((key (expand-file-name project-root))
         (ring (gethash key le::cci--prompt-rings)))
    (unless (equal text (cdr-safe (car ring)))
      (let ((new-ring (cons (cons (floor (float-time)) text) ring)))
        (when (> (length new-ring) le::cci--prompt-ring-max-size)
          (setcdr (nthcdr (1- le::cci--prompt-ring-max-size) new-ring) nil))
        (puthash key new-ring le::cci--prompt-rings)))))

;;;; Buffer-local state

(cl-defstruct (le::cci--state (:copier nil))
  "State for a CCI prompt buffer."
  (project-root nil :documentation "Expanded project root for this prompt.")
  (history nil :documentation "Merged history as (TIMESTAMP . TEXT) cons cells, most recent first.")
  (position nil :documentation "Current index into history, or nil.")
  (saved-input nil :documentation "Buffer contents saved before history navigation.")
  (ring-index 0 :documentation "How far into the prompt ring we've merged.")
  (files nil :documentation "List of JSONL files, most recent first.")
  (file-index 0 :documentation "Index into files list (current file being read).")
  (offsets nil :documentation "Vector of byte offsets for current file (oldest first).")
  (loaded-count 0 :documentation "Entries loaded from current file's offsets.")
  (finish-callback nil :documentation "Function called with buffer text on C-c C-c.")
  (saved-winconf nil :documentation "Window configuration saved before prompt buffer.")
  (header-line-default nil :documentation "Original header-line-format for restoration."))

(defvar-local le::cci--st nil
  "Prompt buffer state, a `le::cci--state' struct.")

(defvar le::cci--history-batch-size 5
  "Number of history entries to load per batch.")

;;;; Project / session file helpers

(defun le::cci--project-key (project-root)
  "Derive the Claude Code project key from PROJECT-ROOT.
Replaces / and . with -."
  (replace-regexp-in-string "[/.]" "-" (directory-file-name (expand-file-name project-root))))

(defun le::cci--session-jsonl-files (project-root)
  "Return list of JSONL files for the active Claude Code session, most recent first.
Reads the session ID stack from the cci vterm buffer for PROJECT-ROOT."
  (let* ((project-root (expand-file-name project-root))
         (key (le::cci--project-key project-root))
         (projects-dir (expand-file-name key "~/.claude/projects/"))
         (stack (le::cci--get-session-id-stack project-root)))
    (delq nil
          (mapcar (lambda (session-id)
                    (let ((file (expand-file-name (concat session-id ".jsonl") projects-dir)))
                      (when (file-exists-p file) file)))
                  stack))))

(defun le::cci--get-session-id-stack (project-root)
  "Get the session ID stack from the cci vterm buffer for PROJECT-ROOT.
`claude-code-ide--routing-tokens' maps directory → routing-token."
  (let ((project-root (expand-file-name project-root)))
    (catch 'found
      (maphash (lambda (dir routing-token)
                 (when-let* (((string= (expand-file-name dir) project-root))
                             (session (gethash routing-token claude-code-ide-mcp-server--sessions))
                             (buf (plist-get session :buffer))
                             ((buffer-live-p buf)))
                   (throw 'found (buffer-local-value 'le::cci--session-id-stack buf))))
               claude-code-ide--routing-tokens))))

;;;; Byte-offset scanning

(defun le::cci--scan-message-offsets (jsonl-file)
  "Scan JSONL-FILE and return a vector of byte offsets for user message lines.
Offsets are oldest first.  Filters to only lines with extractable text content."
  (let (offsets)
    (with-temp-buffer
      (insert-file-contents jsonl-file)
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((bol (line-beginning-position))
               (eol (line-end-position))
               (line (buffer-substring-no-properties bol eol)))
          (when (le::cci--extract-message-entry line)
            (push (1- (position-bytes bol)) offsets)))
        (forward-line 1)))
    (vconcat (nreverse offsets))))

;;;; Extract message entry from a single JSONL line

(defun le::cci--extract-message-entry (line)
  "Parse a JSONL LINE and extract (TIMESTAMP . TEXT) for user messages, or nil.
TIMESTAMP is integer seconds.  Returns nil for non-user messages or empty text."
  (let ((obj (ignore-errors (json-parse-string line :object-type 'alist))))
    (when (and obj (equal (alist-get 'type obj) "user"))
      (let* ((message (alist-get 'message obj))
             (content (alist-get 'content message))
             (ts-str (alist-get 'timestamp obj))
             (timestamp (if ts-str
                            (floor (float-time (date-to-time ts-str)))
                          0))
             (text
              (cond
               ((stringp content)
                (let ((trimmed (string-trim content)))
                  (unless (string-empty-p trimmed) trimmed)))
               ((vectorp content)
                (let ((t2 (string-trim
                           (mapconcat
                            (lambda (block)
                              (if (and (equal (alist-get 'type block) "text")
                                       (alist-get 'text block))
                                  (alist-get 'text block)
                                ""))
                            content " "))))
                  (unless (string-empty-p t2) t2))))))
        (when text
          (cons timestamp text))))))

;;;; Lazy batch loading

(defun le::cci--current-file (st)
  "Return the current JSONL file for state ST, or nil."
  (nth (le::cci--state-file-index st) (le::cci--state-files st)))

(defun le::cci--advance-to-next-file (st)
  "Move ST to the next JSONL file in the stack.
Returns non-nil if a next file was available and its offsets were scanned."
  (let ((next-idx (1+ (le::cci--state-file-index st))))
    (when (< next-idx (length (le::cci--state-files st)))
      (setf (le::cci--state-file-index st) next-idx)
      (setf (le::cci--state-loaded-count st) 0)
      (setf (le::cci--state-offsets st)
            (le::cci--scan-message-offsets (nth next-idx (le::cci--state-files st))))
      t)))

(defun le::cci--load-history-batch ()
  "Load the next batch of JSONL history entries.
Works backwards from the end of the offsets vector.
When current file is exhausted, advances to the next file in the stack."
  (let* ((st le::cci--st)
         (file (le::cci--current-file st))
         (offsets (le::cci--state-offsets st)))
    ;; If current file exhausted, try next
    (when (and file offsets
               (>= (le::cci--state-loaded-count st) (length offsets)))
      (when (le::cci--advance-to-next-file st)
        (setq file (le::cci--current-file st))
        (setq offsets (le::cci--state-offsets st))))
    (when (and file offsets)
      (let* ((total (length offsets))
             (already (le::cci--state-loaded-count st))
             (remaining (- total already))
             (batch-size (min le::cci--history-batch-size remaining))
             (start-idx (- total already batch-size))
             new-entries)
        (when (> batch-size 0)
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (let ((file-size (file-attribute-size (file-attributes file))))
              (dotimes (i batch-size)
                (let* ((idx (+ start-idx i))
                       (offset (aref offsets idx))
                       (end (min file-size (+ offset (* 1024 1024)))))
                  (erase-buffer)
                  (insert-file-contents-literally file nil offset end)
                  (goto-char (point-min))
                  (let* ((eol (or (line-end-position) (point-max)))
                         (line (decode-coding-string
                                (buffer-substring-no-properties (point-min) eol)
                                'utf-8))
                         (entry (le::cci--extract-message-entry line)))
                    (when entry
                      (push entry new-entries)))))))
          (setf (le::cci--state-history st)
                (append (le::cci--state-history st) new-entries))
          (cl-incf (le::cci--state-loaded-count st) batch-size))))))

(defun le::cci--history-can-load-more-p ()
  "Return non-nil if more history batches can be loaded.
Checks both current file and remaining files in the stack."
  (let ((st le::cci--st))
    (or (and (le::cci--state-offsets st)
             (< (le::cci--state-loaded-count st)
                (length (le::cci--state-offsets st))))
        (< (1+ (le::cci--state-file-index st))
           (length (le::cci--state-files st))))))

;;;; Ring merge (timestamp-based merge-sort with dedup)

(defun le::cci--merge-ring (st)
  "Merge ring entries into history using timestamp-based merge sort.
Both ring and history (JSONL) are newest-first lists of (TIMESTAMP . TEXT).
Deduplicates: entries with identical text within 60s are considered the same;
the JSONL version is kept and the ring entry is purged.

Updates `ring-index' in ST to track merge progress for lazy loading."
  (let* ((root (le::cci--state-project-root st))
         (ring (gethash root le::cci--prompt-rings))
         (ring-entries (nthcdr (le::cci--state-ring-index st) ring))
         (jsonl (le::cci--state-history st))
         (purged nil)
         result)
    ;; Two-pointer merge, both newest-first
    (while (or ring-entries jsonl)
      (cond
       ;; Both have entries — check for dedup then take the newer one
       ((and ring-entries jsonl)
        (let ((r-ts (car (car ring-entries)))
              (r-text (cdr (car ring-entries)))
              (j-ts (car (car jsonl)))
              (j-text (cdr (car jsonl))))
          (if (and (equal r-text j-text)
                   (<= (abs (- r-ts j-ts)) 60))
              ;; Duplicate — keep JSONL version, mark ring entry for purge
              (progn
                (push (car ring-entries) purged)
                (push (car jsonl) result)
                (setq ring-entries (cdr ring-entries))
                (setq jsonl (cdr jsonl)))
            ;; Not a duplicate — take the newer one
            (if (>= r-ts j-ts)
                (progn
                  (push (car ring-entries) result)
                  (setq ring-entries (cdr ring-entries)))
              (push (car jsonl) result)
              (setq jsonl (cdr jsonl))))))
       ;; Only ring entries left
       (ring-entries
        (push (car ring-entries) result)
        (setq ring-entries (cdr ring-entries)))
       ;; Only JSONL entries left
       (t
        (push (car jsonl) result)
        (setq jsonl (cdr jsonl)))))
    (setf (le::cci--state-history st) (nreverse result))
    (setf (le::cci--state-ring-index st)
          (length (gethash root le::cci--prompt-rings)))
    ;; Lazy purge: remove matched ring entries
    (when purged
      (let ((ring-list (gethash root le::cci--prompt-rings)))
        (dolist (entry purged)
          (setq ring-list (delq entry ring-list)))
        (puthash root ring-list le::cci--prompt-rings)))))

;;;; History navigation commands

(defun le::cci--update-header-for-history (st)
  "Update header line to show position and relative timestamp."
  (let* ((pos (le::cci--state-position st))
         (entry (nth pos (le::cci--state-history st)))
         (timestamp (car entry)))
    (setq header-line-format
          (format " History [%d] %s ago    (M-p older, M-n newer, C-c C-k cancel)"
                  (1+ pos)
                  (seconds-to-string (- (float-time) timestamp) 'expanded 'abbrev)))))

(defun le::cci--restore-header (st)
  "Restore the original header line."
  (setq header-line-format (le::cci--state-header-line-default st)))

(defun le::cci-history-previous ()
  "Navigate to the previous (older) history entry."
  (interactive)
  (let ((st le::cci--st))
    (if (null (le::cci--state-position st))
        (progn
          (setf (le::cci--state-saved-input st) (buffer-string))
          (setf (le::cci--state-position st) 0))
      (cl-incf (le::cci--state-position st)))
    (while (and (>= (le::cci--state-position st)
                    (length (le::cci--state-history st)))
                (le::cci--history-can-load-more-p))
      (le::cci--load-history-batch))
    (if (>= (le::cci--state-position st)
            (length (le::cci--state-history st)))
        (progn
          (if (zerop (le::cci--state-position st))
              (setf (le::cci--state-position st) nil)
            (cl-decf (le::cci--state-position st)))
          (message "End of history"))
      (let* ((inhibit-read-only t)
             (entry (nth (le::cci--state-position st)
                         (le::cci--state-history st))))
        (erase-buffer)
        (insert (cdr entry))
        (le::cci--update-header-for-history st)))))

(defun le::cci-history-next ()
  "Navigate to the next (newer) history entry."
  (interactive)
  (let ((st le::cci--st))
    (cond
     ((null (le::cci--state-position st))
      (message "End of history"))
     ((zerop (le::cci--state-position st))
      (setf (le::cci--state-position st) nil)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (or (le::cci--state-saved-input st) "")))
      (le::cci--restore-header st))
     (t
      (cl-decf (le::cci--state-position st))
      (let* ((inhibit-read-only t)
             (entry (nth (le::cci--state-position st)
                         (le::cci--state-history st))))
        (erase-buffer)
        (insert (cdr entry))
        (le::cci--update-header-for-history st))))))

;;;; Major mode

(defun le::cci-prompt-finish ()
  "Accept the prompt and run the finish callback."
  (interactive)
  (let* ((st le::cci--st)
         (text (string-trim (buffer-string)))
         (root (le::cci--state-project-root st))
         (winconf (le::cci--state-saved-winconf st))
         (callback (le::cci--state-finish-callback st)))
    (unless (string-empty-p text)
      (le::cci--ring-push root text))
    (kill-buffer (current-buffer))
    (when winconf (set-window-configuration winconf))
    (when callback (funcall callback text))))

(defun le::cci-prompt-cancel ()
  "Cancel the prompt buffer."
  (interactive)
  (let ((winconf (le::cci--state-saved-winconf le::cci--st)))
    (kill-buffer (current-buffer))
    (when winconf (set-window-configuration winconf))))

(defvar-keymap le::cci-prompt-mode-map
  :doc "Keymap for `le::cci-prompt-mode'."
  "M-p" #'le::cci-history-previous
  "M-n" #'le::cci-history-next
  "C-c C-c" #'le::cci-prompt-finish
  "C-c C-k" #'le::cci-prompt-cancel)

(define-derived-mode le::cci-prompt-mode text-mode "CCI-Prompt"
  "Major mode for composing Claude Code prompts.
\\[le::cci-prompt-finish] to accept, \\[le::cci-prompt-cancel] to cancel.
\\[le::cci-history-previous]/\\[le::cci-history-next] for history.")

;;;; Initialize history state in the current buffer

(defun le::cci--history-init (project-root)
  "Initialize history state for the current prompt buffer.
PROJECT-ROOT is the project directory to find session history for.
Loads the first JSONL batch from the most recent session file,
then merges ring entries that are newer than the first sync point."
  (let ((root (expand-file-name project-root)))
    (setf (le::cci--state-project-root le::cci--st) root)
    (let ((files (le::cci--session-jsonl-files root)))
      (when files
        (setf (le::cci--state-files le::cci--st) files)
        (setf (le::cci--state-file-index le::cci--st) 0)
        (setf (le::cci--state-offsets le::cci--st)
              (le::cci--scan-message-offsets (car files)))
        (le::cci--load-history-batch)))
    (le::cci--merge-ring le::cci--st)))

;;;; Main entry point

;;;###autoload
(defun le::cci-edit-prompt ()
  "Open a buffer for composing a Claude Code prompt.
M-p/M-n traverse history from the latest session.  C-c C-c finishes."
  (interactive)
  (let* ((root (claude-code-ide--get-working-directory))
         (short-root (abbreviate-file-name (directory-file-name root)))
         (buf-name (format "*cci-prompt: %s*" short-root))
         (existing (get-buffer buf-name)))
    (if existing
        (pop-to-buffer existing)
      (let ((buf (generate-new-buffer buf-name)))
        (with-current-buffer buf
          (le::cci-prompt-mode)
          (let ((hdr (format " Enter Claude Code prompt for %s    (C-c C-c to send, C-c C-k to cancel)" short-root)))
            (setq le::cci--st
                  (make-le::cci--state
                   :saved-winconf (current-window-configuration)
                   :finish-callback (lambda (text)
                                      (claude-code-ide-send-prompt text))
                   :header-line-default hdr))
            (setq header-line-format hdr))
          (le::cci--history-init root)
          (pop-to-buffer (current-buffer)))))))

(provide 'le-cci-edit-prompt)
;;; le-cci-edit-prompt.el ends here
