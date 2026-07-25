;;; le-org-codepath.el --- Fontify and canonicalize le-shelf task-ID codepaths in Org  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Le Wang
;; Keywords: org

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Font-lock and escaping for the task-ID codepaths used across the project's
;; journal / prompt-log Org files (see the org-tasks workflow).  Two shapes:
;;
;;   full        <code>=<n>[=<n>...]      leproject=4, bufferflip=1=1=1
;;   compressed  \=<n>[=<n>...]           \=4=5   (trailing ref, prefix dropped)
;;
;; <code> is `[a-z][a-z0-9]*'; every path token is digits.  The compressed
;; form carries a leading backslash that escapes Org's verbatim markup -- we
;; fontify from the `=' and leave the `\' unpainted (capture group 1).
;;
;; Codepaths never collide with Org verbatim (`=...='): a verbatim opener `='
;; must sit on a valid pre-border (BOL / whitespace / punctuation), but a
;; codepath's leading `=' is ALWAYS preceded by a word char (full form,
;; `leproject=') or a backslash (compressed, `\='), so it can never open a
;; verbatim span.  A bare `=1=' is not a codepath (no code, no backslash) and
;; is never matched here, so there is nothing to reconcile.  We do pass
;; OVERRIDE to the keyword so the face shows through the `org-level-N' face on
;; headlines -- codepaths must read in a headline as well as in body text.
;;
;; The single grammar (`le::org-codepath--path-re') feeds both the fontifier
;; and `le::org-codepath-escape', which programmatic Org writers call to
;; canonicalize a bare compressed ref (`=4=5') to `\=4=5' at write time so the
;; escaping is not the author's burden.
;;
;; `le::org-codepath-setup' is autoloaded and meant for `org-mode-hook' (wired
;; from the `org' `use-package' `:hook' in init).  Font-lock is not a command,
;; so a mode hook stands in for the usual autoloaded `:bind' entry point.

;;; Code:

(defface le::org-codepath
  '((t :inherit font-lock-constant-face))
  "Face for le-shelf task-ID codepaths in Org buffers."
  :group 'faces)

(defconst le::org-codepath--path-re
  "=[0-9]+\\(?:=[0-9]+\\)*"
  "Match a task-ID path run: `=<n>[=<n>...]'.
The grammar tail shared by the full and compressed forms and by the
escaper, so all three read from one definition.")

(defconst le::org-codepath--full-re
  (concat "\\_<[a-z][a-z0-9]*" le::org-codepath--path-re)
  "Match a full task-ID codepath: <code>=<n>[=<n>...].
Anchored at a symbol start so it will not fire inside a larger token.")

(defconst le::org-codepath--compressed-re
  (concat "\\\\\\(" le::org-codepath--path-re "\\)")
  "Match a compressed codepath \\=<n>[=<n>...].
Group 1 is the `=...=' run to face; the leading backslash is matched but
left outside the group so it stays unpainted.")

(defvar le::org-codepath--keywords
  `((,le::org-codepath--full-re (0 'le::org-codepath t))
    (,le::org-codepath--compressed-re (1 'le::org-codepath t)))
  "Font-lock keywords fontifying task-ID codepaths with `le::org-codepath'.
OVERRIDE is on so the face wins over the `org-level-N' headline face.")

;;;###autoload
(defun le::org-codepath-escape (text)
  "Return TEXT with each bare compressed task-ID codepath prefixed with `\\'.
A codepath run (`=<n>[=<n>...]') is escaped only when its leading `=' sits at
the start of TEXT, after whitespace, or after punctuation -- i.e. where it
would otherwise open an Org verbatim span.  A run after a word char (the full
form `foo=1', self-safe) or already after a backslash (`\\=1', idempotent) is
left untouched."
  (replace-regexp-in-string
   (concat "\\(\\`\\|[^\\\\[:alnum:]]\\)\\(" le::org-codepath--path-re "\\)")
   "\\1\\\\\\2"
   text))

;;;###autoload
(defun le::org-codepath-setup ()
  "Fontify le-shelf task-ID codepaths in the current Org buffer.
Adds `le::org-codepath--keywords' buffer-locally; meant for `org-mode-hook'.
Appended so the keyword runs last and its OVERRIDE highlight wins on
headlines."
  (font-lock-add-keywords nil le::org-codepath--keywords 'append)
  (when font-lock-mode
    (font-lock-flush)))

(provide 'le-org-codepath)
;;; le-org-codepath.el ends here
