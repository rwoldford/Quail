(in-package :wb)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(canvas-hardcopy)))  ;; cbh

;; added by CBH , rwo

(defgeneric canvas-hardcopy (canvas &key left top width height)
  (:documentation "Produces a hardcopy of the canvas,~
                   or a selected region in canvas coordinates"))


(defmethod canvas-hardcopy ((self canvas) &key left top width height)
  (case
    (pick-one (list :postscript :printer))
    (:postscript
     (canvas-to-ps self))
    (:printer 
       )))


(defmethod window-hardcopy ((self canvas) &optional ignore )
  (declare (ignore ignore))
  (canvas-hardcopy self))

