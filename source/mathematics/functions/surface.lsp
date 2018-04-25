;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               surface.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Copyright (c)  1992
;;;                Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;
;;;
;;;  Authors:
;;;     P. Poirier 1992
;;;     R.W. Oldford 1992
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export 
'(surface x-grid-of y-grid-of surface-heights-of
          surface-function-of calculate-heights)))

(defclass surface ()
  ((x-grid :accessor x-grid-of
           :initarg :x
           :initform NIL
           :documentation
           "The locations of the x grid points.")
   (y-grid :accessor y-grid-of
           :initarg :y
           :initform NIL
           :documentation
           "The locations of the y grid points.")
   (surface-heights
    :accessor surface-heights-of
    :initarg :surface-heights
    :initform NIL
    :documentation
    "The heights of the surface at each (x,y) location ~
     in the grid.  The locations must be arranged in a vector ~
     varying the y coordinate fastest.")
   (surface-function
    :accessor surface-function-of
    :initarg :surface-function
    :initform NIL
    :documentation
    "The function (or fn) of two arguments which when fn-called on ~
     x and y coordinates will return the height of the surface ~
     at each grid location."))
  (:documentation
   "A class representing a three dimensional surface."))

(defmethod initialize-instance :after
  ((self surface) &rest initargs &key surface-function )
  (declare (ignore initargs))
  (when surface-function
    (calculate-heights self))
  )
  

(defun calculate-heights (surface)
  "Calculates and stores the surface heights for each x y pair ~
   of the x-grid and y-grid of the surface using the function ~
   stored as the surface-function-of surface."
  (let* ((heights (surface-heights-of surface))
         (x (x-grid-of surface))
         (y (y-grid-of surface))
         (x-size (array-total-size x))
         (y-size (array-total-size y))
         (surface-fun (surface-function-of surface))
         )
    (unless (and heights
                 (= (array-total-size heights)
                    (* x-size y-size)))
      (setf heights
            (setf (surface-heights-of surface)
                  (array 0 :dimensions (list (* x-size y-size))))))
    (loop
      for i from 0 to (1- x-size)
      do
      (loop for j from 0 to (1- y-size)
            do
            (setf (eref heights (+ (* i y-size) j))
                  (fn-call surface-fun (eref x i) (eref y j)))))))



(defun surface (x y 
                  &rest keyword-pairs
                  &key surface-heights surface-function)
  "Creates and returns a surface object suitable for use in ~
   surface view.  ~
   (:required ~
   (:arg x NIL A 1-dimensional array of the x-coordinates of the surface.) ~
   (:arg y NIL A 1-dimensional array of the x-coordinates of the surface.)) ~
   (:rest ~
   (:arg keyword-pairs NIL Additional optional initialization keyword arguments.)) ~
   (:key ~
   (:arg surface-heights NIL ~
   A 1-dimensional array containing the height of the surface at ~
   every x y pair.  The number of elements in surface-height is the ~
   product of the number of elements in x and y.  The elements ~
   themselves are arranged varying the elements of y fastest.)
   (:arg surface-function NIL If given, a function which when called on each x y pair ~
   will return the corresponding value of surface-heights.)
   )"
  (if
    (and (null surface-heights) (null surface-function))
    (let (result)
      (setf result
            (wb:prompt-user
             :prompt-string (format NIL "Please supply either ~
                                         a surface-function or an array of ~
                                         surface heights.")
             :read-type :eval
             :type T))
      (cond
       ((functionp result)
        (setf surface-function result))
       ((symbolp result)
        (setf surface-function (symbol-function result)))
       (T (setf surface-heights result)))))
  (apply #'make-instance 'surface
         :x x :y y
         :surface-heights surface-heights
         :surface-function surface-function
         keyword-pairs))
