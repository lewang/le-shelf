;;; le-frame-split.el --- frame sizing/placement commands  -*- lexical-binding: t -*-

;;; Commentary:

;; Interactive commands that snap the current frame to fractions of the
;; monitor's work area (halves, thirds, two-thirds, full screen).  Built-ins
;; only; no dependency on the rest of le-shelf.

;;; Code:

(defmacro le::frame-with-max (&rest body)
  "provide max-width and height"
  `(pcase-let ((`(,_ ,_ ,max-width ,max-height)
                (alist-get 'geometry (car (display-monitor-attributes-list)))))
     ,@body))

;;;###autoload
(defun le::frame-left-two-thirds ()
  (interactive)
  (le::frame-with-max
   (set-frame-size (selected-frame) (* 2 (/ max-width 3)) max-height t)
   (set-frame-position (selected-frame) 0 0)))

;;;###autoload
(defun le::frame-right-two-thirds ()
  (interactive)
  (le::frame-with-max
   (set-frame-size (selected-frame) (- (* 2 (/ max-width 3)) 17) max-height t)
   (set-frame-position (selected-frame) (- (- (/ max-width 3) 10) -10) 0)))

;;;###autoload
(defun le::frame-right-third ()
  (interactive)
  (le::frame-with-max
   (set-frame-position (selected-frame) (* 2 (/ max-width 3)) 0)
   (set-frame-size (selected-frame) (* 1 (/ max-width 3)) max-height t)))

;;;###autoload
(defun le::frame-center-third ()
  (interactive)
  (le::frame-with-max
   (set-frame-position (selected-frame) (/ max-width 3) 0)
   (set-frame-size (selected-frame) (- (* 1 (/ max-width 3)) 20)  max-height t)))

;;;###autoload
(defun le::frame-left-half ()
  (interactive)
  (le::frame-with-max
   (set-frame-position (selected-frame) 0 0)
   (set-frame-size (selected-frame) (* 1 (/ max-width 2)) max-height t)))

;;;###autoload
(defun le::frame-right-half ()
  (interactive)
  (le::frame-with-max
   (set-frame-position (selected-frame) (/ max-width 2) 0)
   (set-frame-size (selected-frame) (* 1 (/ max-width 2)) max-height t)))

;;;###autoload
(defun le::frame-full-screen ()
  (interactive)
  (le::frame-with-max
   (set-frame-position (selected-frame) 0 0)
   (set-frame-size (selected-frame) (* 1 (/ max-width 1)) max-height t)))

(provide 'le-frame-split)
;;; le-frame-split.el ends here
