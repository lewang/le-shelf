;;; le-cci-prompt2-core.el --- CCI prompt editor: state, session, mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Le Wang
;; Keywords: tools
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Core of the Claude Code prompt composer (see `le-cci-prompt2' for the
;; feature overview): the buffer-local edit-session state struct,
;; resolution of the target CCI session, and the `le::cci-prompt2-mode'
;; major mode with its keymaps and the clipboard-image yank-media handler.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-src)
(require 'denote)
(require 'le-denote)
(require 'w)

(declare-function claude-code-ide--get-working-directory "claude-code-ide")
(declare-function claude-code-ide--get-buffer-name "claude-code-ide")
(declare-function le::cci-prompt2-history-previous "le-cci-prompt2-history")
(declare-function le::cci-prompt2-history-next "le-cci-prompt2-history")
(declare-function le::cci-prompt2-commit "le-cci-prompt2-commit")
(declare-function le::cci-prompt2-cancel "le-cci-prompt2-commit")

(defvar claude-code-ide-mcp-server--sessions)
(defvar claude-code-ide--routing-tokens)

;;;; Buffer-local state

(cl-defstruct (le::cci-prompt2--state (:copier nil))
  "State for a prompt edit buffer."
  (cci-buffer nil :documentation "Target CCI session buffer.")
  (origin-window nil :documentation "Window selected when this edit buffer
opened; focus returns here on commit/cancel.  For a C-M-g handoff the setup
hook selects the CCI window first, so this captures that rather than the
throwaway temp-file window.")
  (root nil :documentation "Expanded project root the prompt targets.")
  (heading-id nil :documentation "Bracketed flip-stamp id of this prompt's
heading, e.g. \"[2026-07-14 Tue 17:47:45]\" -- its EDITING flip time, stable
for the whole edit session (the headline only changes again at the terminal
flip, after which nothing looks it up).")
  (file-path nil :documentation "Path of the log file the heading lives in.")
  (hist-entries nil :documentation "History plists (:id :state :text :ts), newest first; nil when not navigating.")
  (hist-position nil :documentation "Current index into hist-entries, or nil.")
  (hist-saved-input nil :documentation "Draft text stashed before history navigation.")
  (header-line-default nil :documentation "Default header line, restored after history navigation."))

(defvar-local le::cci-prompt2--st nil
  "Prompt edit buffer state, a `le::cci-prompt2--state' struct.")

(defun le::cci-prompt2--header-line (cci-name siblings)
  "Return the default header line for CCI-NAME's prompt edit buffer.
SIBLINGS is the number of other EDITING headings at open time."
  (format " Prompt for %s -- %d other EDITING    (C-c ' send, C-c C-k cancel, M-p/M-n history)"
          cci-name siblings))

;;;; Target CCI session resolution

(defvar-local le::cci-prompt2--target-cci-buffer nil
  "Buffer-local override for the target CCI buffer.
Set when the user picks a CCI session for a buffer that has no
natural mapping.  Persists across `le::cci-prompt2-edit' calls.")

(defun le::cci-prompt2--live-cci-buffers ()
  "Return alist of (BUFFER-NAME . DIRECTORY) for all live CCI sessions."
  (let (result)
    (maphash (lambda (dir tok)
               (when-let* ((session (gethash tok claude-code-ide-mcp-server--sessions))
                            (buf (plist-get session :buffer))
                            ((buffer-live-p buf)))
                 (push (cons (buffer-name buf) dir) result)))
             claude-code-ide--routing-tokens)
    result))

(defun le::cci-prompt2--dir-for-cci-buffer (cci-buf)
  "Return the directory associated with CCI-BUF, or nil."
  (catch 'found
    (maphash (lambda (dir tok)
               (when-let* ((session (gethash tok claude-code-ide-mcp-server--sessions))
                            (buf (plist-get session :buffer))
                            ((eq buf cci-buf)))
                 (throw 'found dir)))
             claude-code-ide--routing-tokens)))

(defun le::cci-prompt2--buffer-cci-buffer ()
  "Return the CCI session buffer mapped from the current buffer's directory.
Resolution is by `default-directory' / project via the routing table, matching
`le::cci-prompt2--resolve-cci-target' step 4.  Nil when no live session maps."
  (when-let* ((root (claude-code-ide--get-working-directory))
              (buf-name (claude-code-ide--get-buffer-name root))
              (buf (get-buffer buf-name)))
    buf))

(defun le::cci-prompt2--workspace-cci-buffer ()
  "Return the CCI session buffer for the current `w' workspace.
Nil when there is no current workspace or root, or no live session maps to
that root."
  (when-let* ((ws (w-current))
              (root (plist-get ws :project-root))
              (buf-name (claude-code-ide--get-buffer-name (expand-file-name root)))
              (buf (get-buffer buf-name)))
    buf))

(defun le::cci-prompt2--order-cci-names (names)
  "Order NAMES: current buffer's cci, workspace's cci, then everything else.
Priority entries not present in NAMES are dropped; duplicates are collapsed
(buffer's and workspace's cci coincide in single-project work)."
  (let* ((priority (delq nil (list (when-let* ((b (le::cci-prompt2--buffer-cci-buffer)))
                                     (buffer-name b))
                                   (when-let* ((b (le::cci-prompt2--workspace-cci-buffer)))
                                     (buffer-name b)))))
         (head (seq-filter (lambda (n) (member n names)) (delete-dups priority))))
    (append head (seq-remove (lambda (n) (member n head)) names))))

(defun le::cci-prompt2--session-completion-table (names)
  "Completion table over NAMES that preserves their given order in the UI."
  (lambda (string pred action)
    (if (eq action 'metadata)
        '(metadata (display-sort-function . identity)
                   (cycle-sort-function . identity))
      (complete-with-action action names string pred))))

(defun le::cci-prompt2--choose-cci-buffer ()
  "Prompt the user to pick a live CCI buffer.
The candidate list is ordered current buffer's cci, workspace's cci, then the
rest; the default (bare RET) is the first entry.
If only one CCI buffer exists, return it without prompting."
  (let* ((alist (le::cci-prompt2--live-cci-buffers))
         (names (le::cci-prompt2--order-cci-names (mapcar #'car alist))))
    (unless names
      (user-error "No live CCI sessions"))
    (if (= (length names) 1)
        (get-buffer (car names))
      (let ((chosen (completing-read
                     "CCI session: "
                     (le::cci-prompt2--session-completion-table names)
                     nil t nil nil (car names))))
        (get-buffer chosen)))))

(defun le::cci-prompt2--resolve-cci-target (force-choose)
  "Resolve the target CCI buffer and its root directory.
When FORCE-CHOOSE is non-nil, always prompt.  When the current
buffer is itself a CCI session buffer, it is the target.
Returns (ROOT . CCI-BUFFER) or signals an error."
  (let (cci-buf)
    (cond
     ;; C-u: always prompt and save
     (force-choose
      (setq cci-buf (le::cci-prompt2--choose-cci-buffer))
      (setq le::cci-prompt2--target-cci-buffer cci-buf))
     ;; Invoked from a CCI session buffer? It is the target.  Resolved via
     ;; the routing-token table, not `default-directory', which is not
     ;; authoritative in terminal buffers.
     ((le::cci-prompt2--dir-for-cci-buffer (current-buffer))
      (setq cci-buf (current-buffer)))
     ;; Saved override still live?
     ((and le::cci-prompt2--target-cci-buffer
           (buffer-live-p le::cci-prompt2--target-cci-buffer))
      (setq cci-buf le::cci-prompt2--target-cci-buffer))
     ;; Resolve from the buffer's own directory, disambiguating when it
     ;; disagrees with the workspace's cci.
     (t
      (let ((buf (le::cci-prompt2--buffer-cci-buffer))
            (ws-buf (le::cci-prompt2--workspace-cci-buffer)))
        (cond
         ;; Buffer's own cci disagrees with the workspace's — disambiguate on
         ;; every invocation, without persisting (cross-project work).
         ((and buf ws-buf (not (eq buf ws-buf)))
          (setq cci-buf (le::cci-prompt2--choose-cci-buffer)))
         ;; Buffer's own cci agrees, or there is no workspace cci — auto-pick.
         (buf
          (setq cci-buf buf))
         ;; No natural mapping — prompt once and remember.
         (t
          (setq cci-buf (le::cci-prompt2--choose-cci-buffer))
          (setq le::cci-prompt2--target-cci-buffer cci-buf))))))
    (let ((dir (le::cci-prompt2--dir-for-cci-buffer cci-buf)))
      (unless dir
        (user-error "Cannot find directory for %s" (buffer-name cci-buf)))
      (cons dir cci-buf))))

;;;; Major mode and keymaps

(defun le::cci-prompt2--yank-media-handler (mimetype data)
  "Save image DATA beside the log file and reference it from both sides.
Registered for image/.* in `le::cci-prompt2-mode', replacing the
handler inherited from org, which is doubly wrong here: its
`org-yank-image-save-method' machinery (yours: attach) needs a
file-backed heading this edit buffer doesn't have, and it inserts an
org link the Claude CLI can't read.  The image becomes a denote
attachment of the prompt log -- saved next to the log file as
<id>--screenshot.<ext>, MIMETYPE picking the extension, the id minted
by `denote-get-identifier-function' with silo-wide bump-a-second
uniqueness.  Two references are minted together: the plain-text
\"@<root-relative-path>\" inserted at point, which the CLI resolves
at submit like any other @-mention (`default-directory' here is the
project root), and a [[denote:<id>]] line appended after the
heading's #+end_src in the log file -- outside the block, where
write-back never touches it -- so the log entry stays org-followable.
The log file is saved; the draft body is not (write-back happens at
C-x C-s or commit, as always)."
  (let* ((st (or le::cci-prompt2--st
                 (user-error "Not in a prompt edit buffer")))
         (root (le::cci-prompt2--state-root st))
         (dir (file-name-directory (le::cci-prompt2--state-file-path st)))
         ;; Denote's used-id scan reads `denote-directory'; scope it to
         ;; the playground silo (prompt-log/'s parent), as
         ;; `le::cci-prompt2--create-log-file' does.
         (denote-directory (file-name-directory (directory-file-name dir)))
         ;; The kernel mints the id (using that scoped scan) and writes
         ;; <id>--screenshot.<ext> into DIR.
         (res (le::denote-write-media dir mimetype data))
         (path (car res))
         (id (cdr res))
         (mk org-src--beg-marker))
    (with-current-buffer (marker-buffer mk)
      (org-with-wide-buffer
       (goto-char mk)
       (org-back-to-heading t)
       (let ((bound (save-excursion (org-end-of-subtree t t) (point))))
         (goto-char mk)
         (unless (re-search-forward "^[ \t]*#\\+end_src" bound t)
           (user-error "No #+end_src under this draft's heading"))
         (forward-line 1)
         (unless (bolp) (insert "\n"))
         ;; Past earlier pastes' link lines: chronological order.
         (while (looking-at "\\[\\[denote:") (forward-line 1))
         (insert (format "[[denote:%s]]\n" id))
         (save-buffer))))
    (unless (or (bolp) (memq (char-before) '(?\s ?\t)))
      (insert " "))
    (insert "@" (file-relative-name path root) " ")))

(defvar-keymap le::cci-prompt2-mode-map
  :doc "Keymap for `le::cci-prompt2-mode'."
  "M-p" #'le::cci-prompt2-history-previous
  "M-n" #'le::cci-prompt2-history-next)

(define-derived-mode le::cci-prompt2-mode org-mode "CCI-Prompt2"
  "Major mode for composing Claude Code prompts in org src blocks.
The block language `le::cci-prompt2' resolves here via
`org-src-get-lang-mode's \"-mode\" suffixing, so `org-edit-src-code'
picks this mode with no `org-src-lang-modes' entry.  C-c \\=' commits
\(sends the prompt), C-c C-k cancels;
\\[le::cci-prompt2-history-previous]/\\[le::cci-prompt2-history-next] \
browse earlier prompts (log file merged with CLI history).
\\[yank-media] pastes a clipboard image as a denote attachment beside
the log file and @-mentions it in the prompt."
  ;; Same TYPES key as org's own registration, so this *replaces* the
  ;; inherited image handler in these buffers (`yank-media-handler'
  ;; does setf alist-get on equal keys); org-mode buffers keep theirs.
  (yank-media-handler "image/.*" #'le::cci-prompt2--yank-media-handler))

(defvar-keymap le::cci-prompt2--edit-map
  :doc "Overrides for `org-src-mode-map' in prompt edit buffers.
Installed per-buffer via `minor-mode-overriding-map-alist' -- as a
minor-mode map, `org-src-mode-map' would otherwise shadow any
major-mode binding of these keys.  The parent keeps the rest of
org-src-mode's bindings (notably C-x C-s = `org-edit-src-save', i.e.
write back and save the log file mid-edit) reachable."
  :parent org-src-mode-map
  "C-c '" #'le::cci-prompt2-commit
  "C-c C-k" #'le::cci-prompt2-cancel)

(defun le::cci-prompt2--src-mode-setup ()
  "Shadow org-src-mode's exit keys in prompt edit buffers.
On `org-src-mode-hook'; a no-op for src buffers of any other language."
  (when (derived-mode-p 'le::cci-prompt2-mode)
    (push (cons 'org-src-mode le::cci-prompt2--edit-map)
          minor-mode-overriding-map-alist)))

(add-hook 'org-src-mode-hook #'le::cci-prompt2--src-mode-setup)

(provide 'le-cci-prompt2-core)
;;; le-cci-prompt2-core.el ends here
