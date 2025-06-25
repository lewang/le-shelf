

(defun le-cycle-spacing (arg)
  (interactive "*P")
  (cycle-spacing (if (eq arg nil)
                     '-
                   arg))
  ;; remove spacek ") )" in lisps
  (if (save-excursion
        (goto-char (- (point) 2))
        (looking-at ") )"))
      (backward-delete-char 1)))
