;;; le-cci-prompt2.el --- Claude Code prompt editor with an org-file prompt log  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Le Wang
;; Keywords: tools
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; v2 of the Claude Code prompt composer (v1: le-cci-edit-prompt.el,
;; removed at 2f0617e).  Every prompt is a level-1 org heading
;; in the project's prompt-log file --
;; .le-playground/denote/prompt-log/<ts>--claude-code-prompt-log.org --
;; written to disk the moment editing starts, so drafts survive
;; crashes and the log survives restarts.  Per-file TODO states record
;; each prompt's fate: EDITING -> COMMITTED (sent) or ABANDONED (kept
;; on cancel); a cancelled draft can also be deleted outright.  Each
;; headline is titled with the bracketed timestamp of its latest
;; state flip, mirroring the top LOGBOOK line.  Headings sit in two
;; zones, each ascending by that stamp: terminal ones
;; (COMMITTED/ABANDONED) first, EDITING drafts at the file's tail --
;; new drafts append, terminal flips relocate to the zone boundary.
;; The log file is a real denote note: `denote' creates it (denote is
;; a hard dependency now) inside the playground's denote silo,
;; .le-playground/denote/, which /playground-setup bootstraps along
;; with a .dir-locals.el scoping interactive denote commands there.
;;
;; Editing happens in a real `org-edit-special' src-edit buffer: the
;; block language `le::cci-prompt2' resolves to `le::cci-prompt2-mode'
;; via `org-src-get-lang-mode's "-mode" suffixing, with no
;; `org-src-lang-modes' entry.  C-c ' commits (send + COMMITTED),
;; C-c C-k cancels -- the same muscle memory as org-src, whose own
;; bindings are shadowed per-buffer via
;; `minor-mode-overriding-map-alist' (no advice anywhere).  M-p/M-n
;; recall earlier prompts from the log file, merged with the
;; project's entries in Claude Code's own ~/.claude/history.jsonl
;; (pseudo-state CLI) so prompts typed straight into the CLI are
;; recallable too.  A prompt committed here lands in both stores; the
;; merge drops the CLI twin when its timestamp falls within seconds
;; of the heading's COMMITTED flip -- which is why the LOGBOOK stamps
;; carry second precision.  `yank-media' pastes a clipboard image as
;; a denote attachment beside the log file (<ts>--screenshot.png):
;; the prompt text gets a plain "@<path>" mention the CLI resolves at
;; submit, and the log heading a [[denote:<ts>]] line after its
;; block.
;;
;; The CCI-session targeting, region/subject capture, and window-focus
;; behavior are copied from v1 rather than shared, so v1 can later be
;; deleted without untangling.
;;
;; This file is the facade: the `le::cci-prompt2-edit' entry point and
;; the `require's that pull the feature's parts together.  The
;; implementation is split across focused files, each self-contained:
;;   le-cci-prompt2-core.el     state struct, CCI-session resolution, major mode
;;   le-cci-prompt2-mention.el  active-region / point-subject @-mention capture
;;   le-cci-prompt2-log.el      prompt-log file + heading bookkeeping
;;   le-cci-prompt2-history.el  M-p/M-n merged prompt history
;;   le-cci-prompt2-commit.el   commit / cancel
;;   le-cci-prompt2-cli.el      C-M-g temp-file handoff (autoload-driven, standalone)

;;; Code:

(require 'org-src)
(require 'le-cci-prompt2-core)
(require 'le-cci-prompt2-mention)
(require 'le-cci-prompt2-log)
(require 'le-cci-prompt2-history)
(require 'le-cci-prompt2-commit)

;;;; Main entry point

;;;###autoload
(defun le::cci-prompt2-edit (force-choose &optional initial-text)
  "Open an org src edit buffer for composing a Claude Code prompt.
Every invocation appends a fresh EDITING heading to the project's
prompt-log file (created on demand under
.le-playground/denote/prompt-log/) and opens its block via
`org-edit-src-code' -- the draft is on disk from the first moment.
C-c \\=' sends it and marks the heading COMMITTED; C-c C-k cancels,
optionally keeping it as ABANDONED; M-p/M-n recall earlier prompts
\(the log merged with CLI history).

With prefix argument FORCE-CHOOSE, prompt for a CCI session and save
the choice as a buffer-local override.  INITIAL-TEXT, if non-nil,
seeds the draft.  Returns the edit buffer."
  (interactive "P")
  ;; Offer to save the source buffer BEFORE any capture prompt — notably the
  ;; "Subject heading:" ancestor picker in
  ;; `le::cci-prompt2--capture-point-subject'.  We persist often, so the
  ;; save-first prompt must precede every selection (subject heading AND CCI
  ;; session).  Gate on a reference actually being produced for THIS file: an
  ;; active region, or an active (non-terminal) ancestor heading, in a
  ;; file-visiting buffer — both captures reference `buffer-file-name', so
  ;; that is the file to persist.  The `--point-active-ancestors' probe here
  ;; does not prompt (it only walks the outline), so it is safe to run first.
  (when (and buffer-file-name
             (or (use-region-p)
                 (le::cci-prompt2--point-active-ancestors)))
    (cond
     ((not (file-exists-p buffer-file-name))
      (when (y-or-n-p (format "%s is not saved to disk.  Save now to add the reference? " (buffer-name)))
        (save-buffer)))
     ((buffer-modified-p)
      (when (y-or-n-p (format "Save %s before referencing? " (buffer-name)))
        (save-buffer)))))
  (let* ((origin-window (selected-window))
         (region-ref (or (le::cci-prompt2--capture-region-ref)
                         (le::cci-prompt2--capture-point-subject)))
         (target (le::cci-prompt2--resolve-cci-target force-choose))
         (root (car target))
         (cci-buf (cdr target)))
    ;; Now the target root is known: build the on-disk file reference, or
    ;; drop it when the source is still unsaved (offer declined above).
    (when-let* ((file (plist-get region-ref :file)))
      (if (file-exists-p file)
          (setq region-ref
                (plist-put region-ref :ref-string
                           (le::cci-prompt2--file-reference
                            file
                            (plist-get region-ref :beg)
                            (plist-get region-ref :end)
                            root)))
        (setq region-ref nil)))
    (let* ((content (le::cci-prompt2--compose-prompt-content
                     (or initial-text "") region-ref))
           (subject (le::cci-prompt2--extract-subject (car content)))
           (path (le::cci-prompt2--log-file root))
           (file-buf (find-file-noselect path))
           (inserted (le::cci-prompt2--insert-heading
                      file-buf subject (car content)))
           (id (car inserted))
           (body-pos (cdr inserted))
           (siblings (le::cci-prompt2--count-editing-siblings file-buf id))
           edit-buf)
      (with-current-buffer file-buf
        (goto-char body-pos)
        (let ((org-src-window-setup 'other-window))
          (org-edit-src-code nil (format "*cci-prompt2: %s*"
                                         (file-name-nondirectory
                                          (directory-file-name
                                           (buffer-local-value
                                            'default-directory cci-buf))))))
        ;; `org-edit-src-code' pops to the edit buffer, making it current
        ;; for the rest of this body.
        (setq edit-buf (current-buffer)))
      (with-current-buffer edit-buf
        (setq default-directory root)
        (let ((hdr (le::cci-prompt2--header-line (buffer-name cci-buf) siblings)))
          (setq le::cci-prompt2--st
                (make-le::cci-prompt2--state
                 :cci-buffer cci-buf
                 :origin-window origin-window
                 :root root
                 :heading-id id
                 :file-path path
                 :header-line-default hdr))
          (setq header-line-format hdr))
        (goto-char (min (cdr content) (point-max))))
      edit-buf)))

(provide 'le-cci-prompt2)
;;; le-cci-prompt2.el ends here
