;;; le-elpaca.el --- Auto-rebuild stale elpaca packages -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Le Wang

;; Author: Le Wang <lewang.dev.26@gmail.com>
;; Keywords: convenience
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Detect and rebuild stale elpaca packages using two-layer staleness detection:
;; Layer 1 compares repo vs build directory mtimes (catches new/deleted files).
;; Layer 2 compares per-file .el vs .elc mtimes (catches in-place edits).

;;; Code:

(declare-function elpaca--queued "elpaca")
(declare-function elpaca<-repo-dir "elpaca")
(declare-function elpaca<-build-dir "elpaca")
(declare-function elpaca<-id "elpaca")
(declare-function elpaca-rebuild "elpaca")
(declare-function elpaca-process-queues "elpaca")

(defun le-elpaca--dir-mtime (dir)
  "Return modification time of DIR as a time value, or nil if DIR doesn't exist."
  (when (file-directory-p dir)
    (file-attribute-modification-time (file-attributes dir))))

(defun le-elpaca--stale-by-dir-mtime-p (repo-dir build-dir)
  "Return non-nil if REPO-DIR mtime is newer than BUILD-DIR mtime."
  (when-let* ((repo-mtime (le-elpaca--dir-mtime repo-dir))
              (build-mtime (le-elpaca--dir-mtime build-dir)))
    (time-less-p build-mtime repo-mtime)))

(defun le-elpaca--stale-by-file-mtime-p (repo-dir build-dir)
  "Return non-nil if any .el in REPO-DIR is newer than its .elc in BUILD-DIR.
Only checks files present in both directories."
  (catch 'stale
    (dolist (build-file (directory-files build-dir t "\\.elc\\'"))
      (let* ((base (file-name-sans-extension (file-name-nondirectory build-file)))
             (repo-el (expand-file-name (concat base ".el") repo-dir)))
        (when (and (file-exists-p repo-el)
                   (time-less-p (file-attribute-modification-time (file-attributes build-file))
                                (file-attribute-modification-time (file-attributes repo-el))))
          (throw 'stale t))))
    (dolist (build-el (directory-files build-dir t "\\.el\\'"))
      (let* ((name (file-name-nondirectory build-el))
             (repo-el (expand-file-name name repo-dir)))
        (when (and (file-exists-p repo-el)
                   (not (string-suffix-p "-autoloads.el" name))
                   (time-less-p (file-attribute-modification-time (file-attributes build-el))
                                (file-attribute-modification-time (file-attributes repo-el))))
          (throw 'stale t))))
    nil))

(defun le-elpaca--stale-packages ()
  "Return list of elpaca package IDs that are stale and need rebuilding."
  (let (stale)
    (dolist (entry (elpaca--queued))
      (let* ((e (cdr entry))
             (id (elpaca<-id e))
             (repo-dir (elpaca<-repo-dir e))
             (build-dir (elpaca<-build-dir e)))
        (when (and repo-dir build-dir
                   (file-directory-p repo-dir)
                   (file-directory-p build-dir)
                   (or (le-elpaca--stale-by-dir-mtime-p repo-dir build-dir)
                       (le-elpaca--stale-by-file-mtime-p repo-dir build-dir)))
          (push id stale))))
    (nreverse stale)))

;;;###autoload
(defun le::elpaca-rebuild-stale ()
  "Detect and rebuild stale elpaca packages.
A package is stale if either:
- Its repo directory mtime is newer than its build directory mtime
  (catches new/deleted files from git pull, etc.)
- Any .el file in the repo is newer than the corresponding .elc in the build
  (catches in-place edits to existing files)."
  (interactive)
  (let ((stale (le-elpaca--stale-packages)))
    (if (null stale)
        (message "All elpaca packages up to date")
      (message "Rebuilding stale packages: %s"
               (mapconcat #'symbol-name stale ", "))
      (dolist (id stale)
        (elpaca-rebuild id))
      (elpaca-process-queues))))

(provide 'le-elpaca)
;;; le-elpaca.el ends here
