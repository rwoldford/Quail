;;; just a note: the first macro has a quote around it so that I can check that
;;; the form it iexpamding to is what I want.
#|
(defmacro with-focused-canvas (canvas &body body)
  `(quote (with-focused-view ,canvas ,@body)))|#
(in-package wb)

(defmacro with-focused-canvas (canvas &body body)
  `(with-focused-view ,canvas ,@body))

(defun canvas-draw-point (x y canvas)
  (let ((p (make-point x (mac-to-canvas-y y canvas))))
    (with-focused-canvas canvas
      (#_MoveTo :long p)
      (#_LineTo :long p))))


(defun canvas-draw-point (x y canvas)
  (let ((new-y (mac-to-canvas-y y canvas)))
    (with-focused-canvas canvas
      (#_MoveTo x new-y)
      (#_LineTo x new-y))))

(defun canvas-draw-points (array canvas)
  (with-focused-canvas canvas
    (loop for i from 0 to (1- (first (array-dimensions array)))
          do (canvas-draw-point (aref array i 0) (aref array i 1) canvas))))

(defmacro set-canvas-pen-mode (canvas mode)
  `(qd::set-pen-mode ,canvas ,mode))

(defun canvas-draw-filled-circle (x y radius canvas)
  (qd::paint-oval canvas
                  (- x radius)
                  (canvas-to-mac-y (+ y radius) canvas)
                  (+ x radius)
                  (canvas-to-mac-y (- y radius) canvas)))