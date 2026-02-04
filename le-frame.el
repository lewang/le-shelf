;;; le-frame.el --- frame tools  -*- lexical-binding: t -*-


(defmacro le-frame-with-max (&rest body)
  "provide max-width and height"
  `(pcase-let ((`(,_ ,_ ,max-width ,max-height)
                (alist-get 'geometry (car (display-monitor-attributes-list)))))
     ,@body))

;;;###autoload
(defun le-frame-left-two-thirds ()
  (interactive)
  (le-frame-with-max
   (set-frame-size (selected-frame) (* 2 (/ max-width 3)) max-height t)
   (set-frame-position (selected-frame) 0 0)))

;;;###autoload
(defun le-frame-right-two-thirds ()
  (interactive)
  (le-frame-with-max
   (set-frame-size (selected-frame) (- (* 2 (/ max-width 3)) 17) max-height t)
   (set-frame-position (selected-frame) (- (- (/ max-width 3) 10) -10) 0)))

;;;###autoload
(defun le-right-two-thirds ()
  (interactive)
  (le-frame-with-max
   (set-frame-size (selected-frame) (- (* 2 (/ max-width 3)) 17) max-height t)
   (set-frame-position (selected-frame) (- (- (/ max-width 3) 10) -10) 0)))


;;;###autoload
(defun le-frame-right-one-thirds ()
  (interactive)
  (le-frame-with-max
   (set-frame-position (selected-frame) (* 2 (/ max-width 3)) 0)
   (set-frame-size (selected-frame) (* 1 (/ max-width 3)) max-height t)))

;;;###autoload
(defun le-frame-center-third ()
  (interactive)
  (le-frame-with-max
   (set-frame-position (selected-frame) (/ max-width 3) 0)
   (set-frame-size (selected-frame) (- (* 1 (/ max-width 3)) 20)  max-height t)))

;;;###autoload
(defun le-frame-left-half ()
  (interactive)
  (le-frame-with-max
   (set-frame-position (selected-frame) 0 0)
   (set-frame-size (selected-frame) (* 1 (/ max-width 2)) max-height t)))

;;;###autoload
(defun le-frame-right-half ()
  (interactive)
  (le-frame-with-max
   (set-frame-position (selected-frame) (/ max-width 2) 0)
   (set-frame-size (selected-frame) (* 1 (/ max-width 2)) max-height t)))

;;;###autoload
(defun le-frame-full-screen ()
  (interactive)
  (le-frame-with-max
   (set-frame-position (selected-frame) 0 0)
   (set-frame-size (selected-frame) (* 1 (/ max-width 1)) max-height t)))

;;; transient persistent menu fro sizing and moving frame around

(provide 'le-frame)
