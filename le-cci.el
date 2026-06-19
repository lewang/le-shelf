;;; le-cci.el --- Claude Code IDE utility functions -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Le Wang
;; Keywords: tools
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Utility functions for Claude Code IDE integration: vterm helpers,
;; server focus management, and prompt file setup.

;;; Code:

(declare-function claude-code-ide--session-buffer-p "claude-code-ide" (buffer))

;;;###autoload
(defun le::vterm-send-C-g ()
  "Send C-g to the vterm process."
  (interactive)
  (vterm-send-key (kbd "C-g")))

;;;###autoload
(defun le::server-focus-on-claude-code-ide-on-exit ()
  "Focus the Claude Code window after server-done.
Used as a `server-done-hook' to return to the CCI terminal.
Backend-agnostic: matches the session buffer via
`claude-code-ide--session-buffer-p' rather than a terminal major mode."
  (when-let* ((claude-window
               (get-window-with-predicate
                (lambda (window)
                  (claude-code-ide--session-buffer-p (window-buffer window))))))
    (select-window claude-window)))

;;;###autoload
(defun le::claude-prompt-file-setup ()
  "Setup buffer-local hooks for Claude prompt files.

Auto-return to vterm window after editing Claude prompt files.

Should have this setting: (setq server-window \\='pop-to-buffer)"
  (when (and (buffer-file-name)
             (string-match-p "^claude-prompt-.*\\.md$"
                             (file-name-nondirectory (buffer-file-name))))
    (add-hook 'server-done-hook 'le::server-focus-on-claude-code-ide-on-exit nil t)))

;;; ghostel: keep TUI layout whitespace out of copies + hide the nbsp glyph

;; Claude Code's prompt uses U+00A0 between `❯' and the input.  Emacs flags it
;; with the pink underlined `nobreak-space' glyph, and copying terminal text
;; drags the nbsp (and other layout whitespace) into the system clipboard.

(defconst le::ghostel-space-like-regexp
  (rx (any ?\N{NO-BREAK SPACE}                  ; U+00A0  (Claude Code's prompt separator)
           (?\N{EN QUAD} . ?\N{HAIR SPACE})     ; U+2000–U+200A general-punctuation spaces
           ?\N{NARROW NO-BREAK SPACE}           ; U+202F
           ?\N{MEDIUM MATHEMATICAL SPACE}       ; U+205F
           ?\N{IDEOGRAPHIC SPACE}))             ; U+3000
  "Regexp matching the Unicode layout spaces a TUI emits.
They render like a normal space in a terminal grid but become mystery
characters when copied into other programs; each is replaced with a
plain ASCII space.")

(defconst le::ghostel-zero-width-regexp
  (rx (any ?\N{ZERO WIDTH SPACE}                ; U+200B
           ?\N{ZERO WIDTH NON-JOINER}           ; U+200C
           ?\N{ZERO WIDTH JOINER}               ; U+200D
           ?\N{WORD JOINER}                     ; U+2060  (zero-width, not a space)
           ?\N{ZERO WIDTH NO-BREAK SPACE}))     ; U+FEFF  (BOM)
  "Regexp matching the zero-width characters to drop when copying.")

(defun le::ghostel-normalize-whitespace (string)
  "Return STRING with TUI layout whitespace normalized for copying.
Unicode spaces collapse to a plain ASCII space and zero-width characters
are removed.  Semantic characters (soft hyphen, non-breaking hyphen) are
left intact.  Intended as a `kill-transform-function'."
  (replace-regexp-in-string
   le::ghostel-zero-width-regexp ""
   (replace-regexp-in-string le::ghostel-space-like-regexp " " string t t)
   t t))

;;;###autoload
(defun le::ghostel-clean-copy-setup ()
  "Make the current ghostel buffer copy clean text and hide the nbsp glyph.
Installed on `ghostel-mode-hook'.  Two independent effects:
- `nobreak-char-display' nil stops Emacs drawing the pink underlined glyph
  on the non-breaking space in Claude Code's prompt;
- `kill-transform-function' routes every kill/copy from this buffer through
  `le::ghostel-normalize-whitespace'.  All ghostel copy commands
  (`ghostel-readonly-copy', `ghostel-copy-all', OSC 52, and stock
  `kill-ring-save'/`kill-region') funnel through `kill-new', which consults
  `kill-transform-function', so layout whitespace never reaches the kill
  ring or system clipboard."
  (setq-local nobreak-char-display nil)
  (setq-local kill-transform-function #'le::ghostel-normalize-whitespace))

(provide 'le-cci)
;;; le-cci.el ends here
