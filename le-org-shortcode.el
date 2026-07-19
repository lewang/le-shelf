;;; le-org-shortcode.el --- Fontify le-shelf task-ID shortcodes in Org  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Le Wang
;; Keywords: org

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Font-lock for the task-ID shortcodes used across the project's journal /
;; prompt-log Org files (see the org-tasks workflow).  Two shapes:
;;
;;   full        <code>=<n>[=<n>...]      leproject=4, bufferflip=1=1=1
;;   compressed  \=<n>[=<n>...]           \=4=5   (trailing ref, prefix dropped)
;;
;; <code> is `[a-z][a-z0-9]*'; every path token is digits.  The compressed
;; form carries a leading backslash that escapes Org's verbatim markup -- we
;; fontify from the `=' and leave the `\' unpainted (capture group 1).
;;
;; Shortcodes never collide with Org verbatim (`=...='): a verbatim opener `='
;; must sit on a valid pre-border (BOL / whitespace / punctuation), but a
;; shortcode's leading `=' is ALWAYS preceded by a word char (full form,
;; `leproject=') or a backslash (compressed, `\='), so it can never open a
;; verbatim span.  A bare `=1=' is not a shortcode (no code, no backslash) and
;; is never matched here, so there is nothing to reconcile.  We do pass
;; OVERRIDE to the keyword so the face shows through the `org-level-N' face on
;; headlines -- shortcodes must read in a headline as well as in body text.
;;
;; `le::org-shortcode-setup' is autoloaded and meant for `org-mode-hook' (wired
;; from the `org' `use-package' `:hook' in init).  Font-lock is not a command,
;; so a mode hook stands in for the usual autoloaded `:bind' entry point.

;;; Code:

(defface le::org-shortcode
  '((t :inherit font-lock-constant-face))
  "Face for le-shelf task-ID shortcodes in Org buffers."
  :group 'faces)

(defconst le::org-shortcode--full-re
  "\\_<[a-z][a-z0-9]*=[0-9]+\\(?:=[0-9]+\\)*"
  "Match a full task-ID shortcode: <code>=<n>[=<n>...].
Anchored at a symbol start so it will not fire inside a larger token.")

(defconst le::org-shortcode--compressed-re
  "\\\\\\(=[0-9]+\\(?:=[0-9]+\\)*\\)"
  "Match a compressed shortcode \\=<n>[=<n>...].
Group 1 is the `=...=' run to face; the leading backslash is matched but
left outside the group so it stays unpainted.")

(defvar le::org-shortcode--keywords
  `((,le::org-shortcode--full-re (0 'le::org-shortcode t))
    (,le::org-shortcode--compressed-re (1 'le::org-shortcode t)))
  "Font-lock keywords fontifying task-ID shortcodes with `le::org-shortcode'.
OVERRIDE is on so the face wins over the `org-level-N' headline face.")

;;;###autoload
(defun le::org-shortcode-setup ()
  "Fontify le-shelf task-ID shortcodes in the current Org buffer.
Adds `le::org-shortcode--keywords' buffer-locally; meant for `org-mode-hook'.
Appended so the keyword runs last and its OVERRIDE highlight wins on
headlines."
  (font-lock-add-keywords nil le::org-shortcode--keywords 'append)
  (when font-lock-mode
    (font-lock-flush)))

(provide 'le-org-shortcode)
;;; le-org-shortcode.el ends here
