;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               surface.lsp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Copyright (c)  1993
;;;                Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1993
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------
;;;
;;;
;;;           Surface plotting
;;;

(in-package :quail-user)

;;; Get the x-grid
(<- x (seq -2  2 1))

;;; y-grid
(<- y (array '(-2 -1.5 -1 0 2)))

;;; A surface function

(defun foo (x y)
  "Some surface"
  (exp (- (+ (* x x) (* y y)))))

;;; Some plots

(surface-plot :x x :y y :surface-function #'foo)
(surface-plot :x x :y x :surface-function #'foo)

;;; Just get the plot
(<- s-p (surface-plot :x x :y y :surface-function #'foo :draw? NIL))

;;; Then draw it.
(draw-view s-p)


;;; Get some heights (need 25 points for x-y pairs y element varies fastest)

(<- z (array (loop for i from 1 to 25 collect (random 100))))

(surface-plot :x x :y x :surface-heights z )



;;; Fancier stuff
;;;

;;; Get a surface
(<- s (make-instance 'surface :x x :y y :surface-function #'foo))

;;; Draw it (in two different windows)
(surface-plot :surface s)
(surface-plot :surface s)


;;; Or could get a surface-view and work with that a la views.
(<- s-v (make-instance 'surface-view :surface  s))


;;; Here's a more interesting surface

(defun my-surface (x y)
    (- (* 3 (expt (- 1 x) 2)
          (exp (- (+ (expt x 2)
                      (expt (+ y 1) 2)))))
       (* 10 (- (/ x 5) (expt x 3) (expt y 5))
          (exp (- (+ (expt x 2) (expt y 2))))
          )
       (* 1/3 (exp (- (+ (expt (+ x 1) 2)
                         (expt y 2)))))))

(<- grid (seq -3 3 .2))

(surface-plot :x grid :y grid
