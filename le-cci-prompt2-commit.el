;;; le-cci-prompt2-commit.el --- CCI prompt editor: commit / cancel  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Le Wang
;; Keywords: tools
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Commit (C-c ') and cancel (C-c C-k) for the Claude Code prompt
;; composer (see `le-cci-prompt2').  Commit writes the draft back, sends
;; it to the CCI session, and flips the log heading to COMMITTED; cancel
;; keeps the draft as ABANDONED or deletes its heading outright.  Focus is
;; returned to whatever window was selected when the edit buffer opened.

;;; Code:

(require 'org)
(require 'org-src)
(require 'le-cci-prompt2-core)
(require 'le-cci-prompt2-log)

(declare-function claude-code-ide-send-prompt "claude-code-ide")

(defun le::cci-prompt2--restore-window (window)
  "Select WINDOW if it is still live; no-op otherwise.
WINDOW is whatever was selected when the edit buffer opened, captured
by `le::cci-prompt2-edit'.  Restoring it on commit or cancel lands
focus back where you were before composing -- the source buffer you
invoked from, or the CCI session -- with no CCI-specific logic here:
the one rule is \"put focus back\".  (A C-M-g handoff pre-selects the
CCI window in `le::cci-prompt2-claude-prompt-file-setup' so the
captured window is the session, not the throwaway temp-file window.)
This is `select-window' on a window object, never
`set-window-configuration', so the restored window's `window-start' is
untouched -- which is what keeps the live CCI window from being
scrolled back on send.  When WINDOW is dead (deleted since capture),
focus is left where the exit put it."
  (when (window-live-p window)
    (select-window window)))

(defun le::cci-prompt2--edit-context ()
  "Validate the current buffer as a live prompt edit buffer and
snapshot everything commit/cancel need: a plist of the state fields
:cci-buf :root :id :origin-window, the trimmed draft
:text, and :marker -- a fresh copy of the block-begin marker, taken
now because org nils its own markers on exit.  The caller clears the
copy when done."
  (let ((st le::cci-prompt2--st))
    (unless (and (derived-mode-p 'le::cci-prompt2-mode)
                 (org-src-edit-buffer-p)
                 st)
      (user-error "Not in a CCI prompt edit buffer"))
    (unless (buffer-live-p (marker-buffer org-src--beg-marker))
      (user-error "Log file buffer was killed; draft only exists in this buffer"))
    (list :cci-buf (le::cci-prompt2--state-cci-buffer st)
          :root (le::cci-prompt2--state-root st)
          :id (le::cci-prompt2--state-heading-id st)
          :origin-window (le::cci-prompt2--state-origin-window st)
          :text (string-trim (buffer-string))
          :marker (copy-marker org-src--beg-marker))))

(defun le::cci-prompt2-commit ()
  "Send the draft to the CCI session and mark its heading COMMITTED.
The write-back and file save happen first, while the heading is still
EDITING, so a failed send leaves a durable draft; only a successful
send flips the state."
  (interactive)
  (let* ((ctx (le::cci-prompt2--edit-context))
         (text (plist-get ctx :text))
         (cci-buf (plist-get ctx :cci-buf))
         (origin-window (plist-get ctx :origin-window))
         (root (plist-get ctx :root))
         (id (plist-get ctx :id))
         (mk (plist-get ctx :marker))
         (subject (le::cci-prompt2--extract-subject text)))
    (when (string-empty-p text)
      (set-marker mk nil)
      (user-error "Empty prompt -- C-c C-k to discard"))
    ;; `switch-invisibly' exits without displaying the source buffer.  The
    ;; source here is the throwaway prompt-log, not something to pop back to
    ;; (`org-edit-src-exit's default is "return to source buffer"); this makes
    ;; the log buffer current for write-back but shows it in no window, and
    ;; killing the edit buffer reverts its own window.  Never
    ;; `set-window-configuration' -- it would re-scroll the live CCI window
    ;; (see [le-cci-prompt2:7]).
    (let ((org-src-window-setup 'switch-invisibly))
      (org-edit-src-exit))
    ;; Current buffer is now the log file, write-back applied.
    (let* ((log-buf (current-buffer))
           (found (le::cci-prompt2--goto-own-heading mk id)))
      (if found
          (le::cci-prompt2--set-heading-subject id subject)
        (message "Heading %s not found in log file; state not updated" id))
      (save-buffer)
      ;; Restore the pre-edit window *before* the send.  `send-prompt' does
      ;; a `sit-for' that forces a redisplay mid-command; restoring first
      ;; means that redisplay already shows the window you came from instead
      ;; of flashing through the log-file window this exit left selected.
      (le::cci-prompt2--restore-window origin-window)
      (condition-case err
          (progn
            (if (buffer-live-p cci-buf)
                (with-current-buffer cci-buf
                  (claude-code-ide-send-prompt text))
              (let ((default-directory root))
                (claude-code-ide-send-prompt text)))
            (with-current-buffer log-buf
              (when (and found (le::cci-prompt2--goto-own-heading mk id))
                (le::cci-prompt2--flip-state-and-save "COMMITTED")))
            (message "Prompt sent to %s"
                     (if (buffer-live-p cci-buf) (buffer-name cci-buf) root)))
        (error
         (message "Send failed (%s); draft kept as EDITING -- M-p recalls it"
                  (error-message-string err)))))
    (set-marker mk nil)))

(defun le::cci-prompt2-cancel ()
  "Cancel the edit.
A non-empty draft prompts for its fate: `a' abandons it, keeping the
heading in the log as ABANDONED; `k' kills it, deleting the heading
outright.  An empty draft is always killed.  The question comes
before any teardown, so \\[keyboard-quit] aborts with the edit
buffer untouched."
  (interactive)
  (let* ((ctx (le::cci-prompt2--edit-context))
         (text (plist-get ctx :text))
         (id (plist-get ctx :id))
         (mk (plist-get ctx :marker))
         (subject (le::cci-prompt2--extract-subject text))
         (keep (and (not (string-empty-p text))
                    (eq ?a (read-char-choice
                            "Draft: [a]bandon (keep in log) or [k]ill (delete)? "
                            '(?a ?k))))))
    (if keep
        (progn
          ;; `switch-invisibly': exit without displaying the log buffer
          ;; (see `le::cci-prompt2-commit').
          (let ((org-src-window-setup 'switch-invisibly))
            (org-edit-src-exit))
          (if (le::cci-prompt2--goto-own-heading mk id)
              (progn
                (le::cci-prompt2--set-heading-subject id subject)
                (le::cci-prompt2--flip-state-and-save "ABANDONED"))
            (message "Heading %s not found in log file; state not updated" id)
            (save-buffer)))
      ;; `switch-invisibly': abort without displaying the log buffer
      ;; (see `le::cci-prompt2-commit').
      (let ((org-src-window-setup 'switch-invisibly))
        (org-edit-src-abort))
      (with-current-buffer (marker-buffer mk)
        (if (le::cci-prompt2--goto-own-heading mk id)
            (progn
              (delete-region (point)
                             (save-excursion (org-end-of-subtree t t) (point)))
              (save-buffer))
          (message "Heading %s not found in log file; nothing deleted" id))))
    (set-marker mk nil)
    (le::cci-prompt2--restore-window (plist-get ctx :origin-window))))

(provide 'le-cci-prompt2-commit)
;;; le-cci-prompt2-commit.el ends here
