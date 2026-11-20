;;; le-isearch-char-fold-test.el --- Tests for char-folded regexps  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for `le::char-fold-regexp', the pure core of the char-folding
;; regexp isearch.
;;
;; Run in batch:
;;
;;   emacs -Q -batch -L "$PWD" -l "$PWD/le-isearch-char-fold-test.el" \
;;     -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'le-isearch-char-fold)

(defun le::char-fold-test--full-match-p (regexp string)
  "Return non-nil when folded REGEXP matches all of STRING."
  (let ((case-fold-search nil))
    (string-match-p (concat "\\`\\(?:" (le::char-fold-regexp regexp) "\\)\\'")
                    string)))

(ert-deftest le::char-fold-regexp-folds-literals ()
  (should (le::char-fold-test--full-match-p "cafe" "café"))
  (should (le::char-fold-test--full-match-p "cafe" "cafe"))
  (should-not (le::char-fold-test--full-match-p "cafe" "cafx")))

(ert-deftest le::char-fold-regexp-keeps-operators ()
  (should (le::char-fold-test--full-match-p "caf.$" "cafX"))
  (should (le::char-fold-test--full-match-p "^re.*e$" "résumé"))
  (should (le::char-fold-test--full-match-p "a\\|e" "é")))

(ert-deftest le::char-fold-regexp-postfix-binds-to-last-char ()
  (should (le::char-fold-test--full-match-p "e+" "éèe"))
  (should (le::char-fold-test--full-match-p "ab*" "a"))
  (should (le::char-fold-test--full-match-p "ab*" "abbb"))
  (should-not (le::char-fold-test--full-match-p "ab*" "abab"))
  (should (le::char-fold-test--full-match-p "ne?" "n"))
  (should (le::char-fold-test--full-match-p "e*?x" "éex")))

(ert-deftest le::char-fold-regexp-keeps-interval-counts ()
  (should (string-search "\\{3\\}" (le::char-fold-regexp "e\\{3\\}")))
  (should (le::char-fold-test--full-match-p "e\\{3\\}" "éeè"))
  (should-not (le::char-fold-test--full-match-p "e\\{3\\}" "ée"))
  (should (le::char-fold-test--full-match-p "e\\{1,2\\}" "é")))

(ert-deftest le::char-fold-regexp-keeps-group-numbers-and-backrefs ()
  (should (string-search "\\(?1:" (le::char-fold-regexp "\\(?1:e\\)\\1")))
  (should (le::char-fold-test--full-match-p "\\(?1:e\\)\\1" "éé"))
  (should-not (le::char-fold-test--full-match-p "\\(e\\)\\1" "éè"))
  (should (le::char-fold-test--full-match-p "\\(?:ab\\)+" "ababáb")))

(ert-deftest le::char-fold-regexp-keeps-bracket-expressions ()
  (should (string-search "[a-c]" (le::char-fold-regexp "[a-c]e")))
  (should (string-search "[]a]" (le::char-fold-regexp "[]a]")))
  (should (string-search "[^[:alpha:]]" (le::char-fold-regexp "[^[:alpha:]]x")))
  (should (le::char-fold-test--full-match-p "[[:digit:]]+e" "42é")))

(ert-deftest le::char-fold-regexp-keeps-backslash-constructs ()
  (should (string-search "\\w+" (le::char-fold-regexp "\\w+")))
  (should (string-search "\\s-" (le::char-fold-regexp "e\\s-e")))
  (should (string-search "\\_<" (le::char-fold-regexp "\\_<e")))
  (should (le::char-fold-test--full-match-p "\\bcafe\\b" "café")))

(ert-deftest le::char-fold-regexp-folds-escaped-literals ()
  (should (le::char-fold-test--full-match-p "a\\.b" "a.b"))
  (should (le::char-fold-test--full-match-p "a\\.b" "a．b"))
  (should-not (le::char-fold-test--full-match-p "a\\.b" "axb"))
  (should (le::char-fold-test--full-match-p "\\*" "*")))

(ert-deftest le::char-fold-regexp-folds-ligatures-in-runs ()
  (should (le::char-fold-test--full-match-p "office" "oﬃce")))

(ert-deftest le::char-fold-regexp-tolerates-incomplete-input ()
  (should (stringp (le::char-fold-regexp "[ab")))
  (should (stringp (le::char-fold-regexp "e\\")))
  (should (stringp (le::char-fold-regexp "e\\{2")))
  (should (stringp (le::char-fold-regexp "\\(?1")))
  (should (stringp (le::char-fold-regexp "[[:alp"))))

(ert-deftest le::char-fold-regexp-empty ()
  (should (equal "" (le::char-fold-regexp ""))))

(provide 'le-isearch-char-fold-test)
;;; le-isearch-char-fold-test.el ends here
