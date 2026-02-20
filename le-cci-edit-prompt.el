;;; le-cci-edit-prompt.el --- Claude Code prompt editor with history  -*- lexical-binding: t; -*-

;;; Commentary:
;; Opens a buffer for composing Claude Code prompts.
;; M-p/M-n traverse history from the latest session JSONL file,
;; lazy-loading in batches of 5.  C-c C-c to accept, C-c C-k to cancel.
;;
;; Prompts entered via C-c C-c are also saved to a per-project ring,
;; so they survive even if Claude Code never registered them.
;;
;; History merge: JSONL positions are authoritative.  Ring-only entries
;; (not found in any JSONL batch) appear at the front.

;;; Code:

(require 'json)
(require 'cl-lib)

;;;; Per-project prompt ring (global, not buffer-local)

(defvar le::cci--prompt-rings (make-hash-table :test 'equal)
  "Hash table mapping expanded project roots to lists of prompt strings.
Most recent first.  These are prompts entered via C-c C-c.")

(defun le::cci--ring-push (project-root text)
  "Push TEXT onto the prompt ring for PROJECT-ROOT.
Deduplicates: if TEXT is already the most recent entry, skip."
  (let* ((key (expand-file-name project-root))
         (ring (gethash key le::cci--prompt-rings)))
    (unless (equal text (car ring))
      (puthash key (cons text ring) le::cci--prompt-rings))))

;;;; Buffer-local state

(cl-defstruct (le::cci--state (:copier nil))
  "State for a CCI prompt buffer."
  (project-root nil :documentation "Expanded project root for this prompt.")
  (ring nil :documentation "Ring entries for this project (snapshot at init).")
  (jsonl-history nil :documentation "JSONL history strings, most recent first.")
  (position nil :documentation "Current index into effective history, or nil.")
  (saved-input nil :documentation "Buffer contents saved before history navigation.")
  (offsets nil :documentation "Vector of byte offsets for user messages (oldest first).")
  (loaded-count 0 :documentation "How many entries from the end of offsets have been loaded.")
  (jsonl-seen nil :documentation "Hash table of JSONL-loaded strings for dedup.")
  (file nil :documentation "Path to the JSONL session file.")
  (finish-callback nil :documentation "Function called with buffer text on C-c C-c.")
  (saved-winconf nil :documentation "Window configuration saved before prompt buffer."))

(defvar-local le::cci--st nil
  "Prompt buffer state, a `le::cci--state' struct.")

(defvar le::cci--history-batch-size 5
  "Number of history entries to load per batch.")

;;;; Effective history (merged view)

(defun le::cci--ring-only-entries (st)
  "Return ring entries from ST that haven't appeared in any JSONL batch."
  (let ((seen (le::cci--state-jsonl-seen st)))
    (cl-remove-if (lambda (s) (gethash s seen))
                  (le::cci--state-ring st))))

(defun le::cci--effective-history (st)
  "Return the merged history: ring-only entries, then JSONL entries."
  (append (le::cci--ring-only-entries st)
          (le::cci--state-jsonl-history st)))

;;;; Project / session file helpers

(defun le::cci--project-key (project-root)
  "Derive the Claude Code project key from PROJECT-ROOT.
Replaces / and . with -."
  (replace-regexp-in-string "[/.]" "-" (directory-file-name (expand-file-name project-root))))

(defun le::cci--session-jsonl-file (project-root)
  "Return the JSONL file for the active Claude Code session at PROJECT-ROOT.
Looks up the session ID from `claude-code-ide--session-ids'."
  (let ((project-root (expand-file-name project-root)))
  (when-let* ((session-id (gethash project-root claude-code-ide--session-ids))
              (key (le::cci--project-key project-root))
              (file (expand-file-name
                     (concat session-id ".jsonl")
                     (expand-file-name key "~/.claude/projects/"))))
    (when (file-exists-p file) file))))

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
          (when (le::cci--extract-message-text line)
            (push (1- (position-bytes bol)) offsets)))
        (forward-line 1)))
    (vconcat (nreverse offsets))))

;;;; Extract message text from a single JSONL line

(defun le::cci--extract-message-text (line)
  "Parse a JSONL LINE and extract the user message text, or nil."
  (let ((obj (ignore-errors (json-parse-string line :object-type 'alist))))
    (when (and obj (equal (alist-get 'type obj) "user"))
      (let* ((message (alist-get 'message obj))
             (content (alist-get 'content message)))
        (cond
         ((stringp content)
          (let ((trimmed (string-trim content)))
            (unless (string-empty-p trimmed) trimmed)))
         ((vectorp content)
          (let ((text (string-trim
                       (mapconcat
                        (lambda (block)
                          (if (and (equal (alist-get 'type block) "text")
                                   (alist-get 'text block))
                              (alist-get 'text block)
                            ""))
                        content " "))))
            (unless (string-empty-p text) text))))))))

;;;; Lazy batch loading

(defun le::cci--load-history-batch ()
  "Load the next batch of JSONL history entries.
Works backwards from the end of the offsets vector."
  (let* ((st le::cci--st)
         (file (le::cci--state-file st))
         (offsets (le::cci--state-offsets st))
         (seen (le::cci--state-jsonl-seen st)))
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
                         (text (le::cci--extract-message-text line)))
                    (when text
                      (puthash text t seen)
                      (push text new-entries)))))))
          (setf (le::cci--state-jsonl-history st)
                (append (le::cci--state-jsonl-history st) new-entries))
          (cl-incf (le::cci--state-loaded-count st) batch-size))))))

(defun le::cci--history-can-load-more-p ()
  "Return non-nil if more history batches can be loaded."
  (let ((st le::cci--st))
    (and (le::cci--state-offsets st)
         (< (le::cci--state-loaded-count st)
            (length (le::cci--state-offsets st))))))

;;;; History navigation commands

(defun le::cci-history-previous ()
  "Navigate to the previous (older) history entry."
  (interactive)
  (let ((st le::cci--st))
    (if (null (le::cci--state-position st))
        (progn
          (setf (le::cci--state-saved-input st) (buffer-string))
          (setf (le::cci--state-position st) 0))
      (cl-incf (le::cci--state-position st)))
    ;; Load more batches until we have enough or exhaust offsets
    (let ((hist (le::cci--effective-history st)))
      (while (and (>= (le::cci--state-position st) (length hist))
                  (le::cci--history-can-load-more-p))
        (le::cci--load-history-batch)
        (setq hist (le::cci--effective-history st)))
      (if (>= (le::cci--state-position st) (length hist))
          (progn
            (if (zerop (le::cci--state-position st))
                (setf (le::cci--state-position st) nil)
              (cl-decf (le::cci--state-position st)))
            (message "End of history"))
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (nth (le::cci--state-position st) hist)))))))

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
        (insert (or (le::cci--state-saved-input st) ""))))
     (t
      (cl-decf (le::cci--state-position st))
      (let* ((hist (le::cci--effective-history st))
             (inhibit-read-only t))
        (erase-buffer)
        (insert (nth (le::cci--state-position st) hist)))))))

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
JSONL positions are authoritative; ring-only entries appear at the front."
  (let* ((root (expand-file-name project-root))
         (file (le::cci--session-jsonl-file root))
         (seen (make-hash-table :test 'equal))
         (ring (gethash root le::cci--prompt-rings)))
    (setf (le::cci--state-project-root le::cci--st) root)
    (setf (le::cci--state-jsonl-seen le::cci--st) seen)
    (setf (le::cci--state-ring le::cci--st) ring)
    (when file
      (setf (le::cci--state-file le::cci--st) file)
      (setf (le::cci--state-offsets le::cci--st)
            (le::cci--scan-message-offsets file))
      (le::cci--load-history-batch))))

;;;; Main entry point

;;;###autoload
(defun le::cci-edit-prompt ()
  "Open a buffer for composing a Claude Code prompt.
M-p/M-n traverse history from the latest session.  C-c C-c finishes."
  (interactive)
  (let* ((root (or (when-let* ((proj (project-current)))
                     (project-root proj))
                   default-directory))
         (short-root (abbreviate-file-name (directory-file-name root)))
         (buf (generate-new-buffer (format "*cci-prompt: %s*" short-root))))
    (with-current-buffer buf
      (le::cci-prompt-mode)
      (setq le::cci--st
            (make-le::cci--state
             :saved-winconf (current-window-configuration)
             :finish-callback (lambda (text)
                                (claude-code-ide-send-prompt text))))
      (setq header-line-format
            (format " Enter Claude Code prompt for %s    (C-c C-c to send, C-c C-k to cancel)" short-root))
      (le::cci--history-init root)
      (pop-to-buffer (current-buffer)))))

(provide 'le-cci-edit-prompt)
;;; le-cci-edit-prompt.el ends here
