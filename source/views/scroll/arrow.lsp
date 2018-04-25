;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               arrow.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;
;;;  This file is part of the Views system, a toolkit for interactive statistical
;;;  graphics.
;;;  
;;;
;;;  Authors:
;;;     C.B. Hurley 1992 George Washington University
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :views)

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(arrow control-arrow)))

;;;----------------------------------------------------------------------------------

(defclass arrow ( simple-view) 
  ((direction :accessor scroll-direction-of :initarg :direction :initform :up))
  )

(defclass control-arrow (control-mixin boxed-view-mixin arrow) 
  ((direction :accessor scroll-direction-of :initarg :direction :initform :up))
  )




(defmethod draw-view ((self arrow) &key viewport)
  (with-exposed-viewports self viewport vp
    (draw-arrow self (scroll-direction-of self) vp)))



(defmethod draw-arrow ((self arrow) (dir (eql :up)) viewport)
  (multiple-value-bind (l r b tp ) (bounds-of viewport)
    (let*  ((w (window-of viewport))
            (cx (round (+ l (* .5 (- r l)))))
            (cy (round (+ b (* .5 (- tp b)))))
            (rx (round (* .3 (- r l))))
            (ry (round (* .3 (- tp b))))
            (ax (round (* .1 (- r l))))
            (ay (round (* .3 (- tp b)))))
      (wb:with-pen-values w (draw-style self :color) 1 nil
          (wb:canvas-draw-line w (- cx rx) cy (- cx ax) cy)
          (wb:canvas-draw-line w (+ cx rx) cy (+ cx ax) cy)

          (wb:canvas-draw-line w (- cx ax) (- cy ay) (+ cx ax) (- cy ay))
          (wb:canvas-draw-line w (- cx ax) cy (- cx ax) (- cy ay))
          (wb:canvas-draw-line w (+ cx ax) cy (+ cx ax) (- cy ay))

          (wb:canvas-draw-line w (- cx rx) cy cx (+ cy ry))
          (wb:canvas-draw-line w (+ cx rx) cy  cx (+ cy ry))))))



(defmethod draw-arrow ((self arrow) (dir (eql :down)) viewport)
  (multiple-value-bind (l r b tp ) (bounds-of viewport)
    (let*  ((w (window-of viewport))
            (cx (round (+ l (* .5 (- r l)))))
            (cy (round (+ b (* .5 (- tp b)))))
            (rx (round (* .3 (- r l))))
            (ry (round (* .3 (- b tp))))
            (ax (round (* .1 (- r l))))
            (ay (round (* .3 (- b tp)))))
      (wb:with-pen-values w (draw-style self :color) 1 nil
          (wb:canvas-draw-line w (- cx rx) cy (- cx ax) cy)
          (wb:canvas-draw-line w (+ cx rx) cy (+ cx ax) cy)

          (wb:canvas-draw-line w (- cx ax) (- cy ay) (+ cx ax) (- cy ay))
          (wb:canvas-draw-line w (- cx ax) cy (- cx ax) (- cy ay))
          (wb:canvas-draw-line w (+ cx ax) cy (+ cx ax) (- cy ay))

          (wb:canvas-draw-line w (- cx rx) cy cx (+ cy ry))
          (wb:canvas-draw-line w (+ cx rx) cy  cx (+ cy ry))))))

(defmethod draw-arrow ((self arrow) (dir (eql :left)) viewport)
  (multiple-value-bind (l r b tp ) (bounds-of viewport)
    (let*  ((w (window-of viewport))
            (cx (round (+ l (* .5 (- r l)))))
            (cy (round (+ b (* .5 (- tp b)))))
            (rx (round (* .3 (- r l))))
            (ry (round (* .3 (- tp b))))
            (ax (round (* .3 (- r l))))
            (ay (round (* .1 (- tp b)))))
      (wb:with-pen-values w (draw-style self :color) 1 nil
          (wb:canvas-draw-line w cx (- cy ry) cx (- cy ay))
          (wb:canvas-draw-line w cx (+ cy ry) cx (+  cy ay))

          (wb:canvas-draw-line w (+ cx ax) (- cy ay) (+ cx ax) (+ cy ay))
          (wb:canvas-draw-line w cx (- cy ay)  (+ cx ax) (- cy ay))
          (wb:canvas-draw-line w cx (+ cy ay)  (+ cx ax) (+ cy ay))

          (wb:canvas-draw-line w cx (- cy ry)  (- cx rx) cy)
          (wb:canvas-draw-line w cx (+ cy ry)  (- cx rx) cy)))))


(defmethod draw-arrow ((self arrow) (dir (eql :right)) viewport)
  (multiple-value-bind (l r b tp ) (bounds-of viewport)
    (let*  ((w (window-of viewport))
            (cx (round (+ l (* .5 (- r l)))))
            (cy (round (+ b (* .5 (- tp b)))))
            (rx (round (* .3 (- l r))))
            (ry (round (* .3 (- tp b))))
            (ax (round (* .3 (- l r))))
            (ay (round (* .1 (- tp b)))))
      (wb:with-pen-values w (draw-style self :color) 1 nil
          (wb:canvas-draw-line w cx (- cy ry) cx (- cy ay))
          (wb:canvas-draw-line w cx (+ cy ry) cx (+  cy ay))

          (wb:canvas-draw-line w (+ cx ax) (- cy ay) (+ cx ax) (+ cy ay))
          (wb:canvas-draw-line w cx (- cy ay)  (+ cx ax) (- cy ay))
          (wb:canvas-draw-line w cx (+ cy ay)  (+ cx ax) (+ cy ay))

          (wb:canvas-draw-line w cx (- cy ry)  (- cx rx) cy)
          (wb:canvas-draw-line w cx (+ cy ry)  (- cx rx) cy)))))















      
            
             


    
