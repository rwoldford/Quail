;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               affinetrans.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;
;;;  This file is part of the Views system, a toolkit for interactive statistical graphics.
;;;  
;;;
;;;  Authors:
;;;     C.B. Hurley 1988-1991 George Washington University
;;;     R.W. Oldford 1988-1991
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :views)

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(affine transform 2d-affine 3d-affine
           make-affine-transform affine-trans-p 
           dimension-of matrix-of  ;;(setf dimension-of )  (setf matrix-of )
           apply-transform apply-transform! compose-transforms error-mismatched-dimensions
           premultiply-transform! rotate-transform! set-to-identity scale-transform!
           translate-transform! invert-transform
           *identity-2d-transform*)))
;;;----------------------------------------------------------------------------------

(defclass affine-transform ()
  ((dimension
    :initarg :dimension
    :accessor dimension-of 
    :documentation " the true dimension of the transform, (n *n)")
   (affine 
    :initarg :affine
    :accessor matrix-of 
    :documentation "an (n+1) *(n+1) array of reals, first n rows and columns ~
                    represent the transformation, the last column represents ~
                    the location shift")
   )
  (:documentation 
   "Affine transformations for viewing 2 and 3 dimensional objects.~
    See Foley and Van Dam for reference. Note the transforms here operate ~
    on columns whereas those in F+VD operate on row vectors"
                  ))


(defgeneric apply-transform (affine-transform item)
  (:documentation "applies the  AFFINE TRANSFORM to ITEM ( list) ~
                  returning the resulting ITEM (list) ~
                  If self is a 2d-affine, item may be a 2d-position or a region"))
 


(defgeneric apply-transform! (affine-transform  item)
  
    (:documentation "applies the affine-transform self to item, placing results in item arguments "))
  
 

(defgeneric compose-transforms (affine-transform1 affine-transform2)
  (:documentation " composes affine-transform1 with affine-transform2 ~
                  returning the resulting AFFINE-TRANSFORM"))
  

(defgeneric  error-mismatched-dimensions (affine-transform1 affine-transform2)
  (:documentation "returns non-nil when the arguments have equal dimension"))
  
(defgeneric  premultiply-transform! (affine-transform multiplier)
                                     
  (:documentation " premultiply AFFINE-TRANSFORM  by another AFFINE-TRANSFORM, ~
                  MULTIPLIER, storing result in AT"))
  
  
(defgeneric rotate-transform! (affine-transform  angle
                               &optional (radians? nil))        
  (:documentation "rotates  AFFINE-TRANSFORM SELF by ANGLE"))
 

(defgeneric set-to-identity (affine-transform)
  (:documentation " AFFINE-TRANSFORM  becomes the identity"))
  
(defgeneric scale-transform! (affine-transform (scale number)
                             &optional &rest other-scales)
  
  (:documentation " scales  AFFINE-TRANSFORM  by SCALE in the x-direction ~
                  and by OTHER-SCALES in the remaining   directions. ~
                  If OTHER-SCALES is  not present, ~
                  SCALE is applied in all to  directions."))

(defgeneric translate-transform! (affine-transform (delta number)
                                 &optional &rest other-deltas)
  (:documentation " scales AFFINE-TRANSFORM by DELTA in the x-direction ~
                  and by OTHER-OTHER-DELTAS in the remaining   directions. ~
                  If OTHER-DELTAS is  not present, ~
                  SCALE is applied in all to  directions."))
  

(defgeneric invert-transform (affine-transform)
  (:documentation " inverts the affine-transform"))
  
(defgeneric make-affine-transform (dimension)
  (:documentation " returns an affine-transform with dimension dimension"))


;;;----------------------------------------------------------------------------------



(defun affine-trans-p (affine) (typep affine 'affine-transform))

(defclass 2d-affine (affine-transform)
  ((affine 
    :documentation "A 3 by 3 array of reals, first 2 rows and colums represent ~
                    the transformation, the last column represents the location shift")
   
   (x-function 
    :accessor x-function 
    :documentation "function applied to object to retrieve its x location")
   
   (y-function 
    :accessor y-function 
    :documentation  "function applied to object to retrieve its y location")
   
   (dimension 
    :initform 2 
    :documentation " the true dimension of the transform, 2 by 2")))

(defclass 3d-affine (affine-transform)
  ((affine 
    :documentation "A 4 by 4 array of reals, first 3 rows and columns ~
                    represent the transformation, the last column represents ~
                    the location shift.")
   
   (x-function 
    :accessor x-function 
    :documentation "Function applied to object to retrieve its x location")
   
   (y-function 
    :accessor y-function 
    :documentation "Function applied to object to retrieve its y location")
   
   (z-function 
    :accessor z-function 
    :documentation "Function applied to object to retrieve its z location")
   
   (dimension 
    :initform 3 
    :documentation " the true dimension of the transform, 3 by 3")))



(defmethod apply-transform ((self 2d-affine) item)
  ;;; applies the TRANSFORM 2D-AFFINE to ITEM  , returning the resulting
  ;;; 2d-POSITION /REGION/LIST
  (if (region-p item)
    (let ((top-right (apply-transform self (top-right-of item)))
          (bottom-left (apply-transform self (bottom-left-of item))))
      (make-region  (2d-position-x bottom-left)
                    (2d-position-x top-right)
                    (2d-position-y bottom-left)
                    (2d-position-y top-right)))
    ;;else
    (multiple-value-bind 
      (x y)
      (if (2d-position-p item)
        (values (2d-position-x item) (2d-position-y item))
        (if (listp item) (values (first item) (second item))
            (values (funcall (x-function self) item)
                    (funcall (y-function self) item))))
      (let ((mat (matrix-of self)) new-x new-y)
        (setq new-x (+ (* x (aref mat 0 0)) (* y (aref mat 0 1))
                       (aref mat 0 2)))
        (setq new-y (+ (* x (aref mat 1 0)) (* y (aref mat 1 1))
                       (aref mat 1 2)))
        (if (listp item) (list new-x new-y)
            
            (make-2d-position new-x
                              new-y))))))



(defmethod apply-transform! ((at 2d-affine)  item)
  
    ;;; applies the 2D-AFFINE to ITEM, placing results in ITEM arguments 
  
  (let ((top-right (apply-transform at (top-right-of item)))
        (bottom-left (apply-transform at (bottom-left-of item))))
    (setf (bounds-of item)
          (list (2d-position-x bottom-left)
                (2d-position-x top-right)
                (2d-position-y bottom-left)
                (2d-position-y top-right)))
    item))

(defmethod compose-transforms ((at1 affine-transform)
                               (at2 affine-transform))
  
  ;;; composes AFFINE-TRANSFORMs  AT1 with AT2 , returning the resulting
  ;;; AFFINE-TRANSFORM
  
  (if (not (eql (dimension-of at1)
                (dimension-of at2)))
    (error-mismatched-dimensions at1 at2)
    
    ;; else
    (let* ((size  (dimension-of at1))
           (result (make-affine-transform size))
           (mat1 (matrix-of at1))
           (mat2 (matrix-of at2))
           (mat3 (matrix-of result ))
           )

      ; (loop for i from 0 to size do 
      (do ((i 0 (incf i)))
          ((> i size))
        ; (loop for j from 0 to size do 
        (do ((j 0 (incf j)))
            ((> j size))
          (setf (aref mat3 i j) 
                ; (loop for k from 0 to size sum
                (do ((sum-mat1 0)
                     (k 0 (incf k)))
                    ((> k size) sum-mat1)
                  (incf sum-mat1 (* (aref mat1 i k) (aref mat2 k j)))))))
      result)))
  
(defmethod error-mismatched-dimensions ((at1 affine-transform)
                                          (at2 affine-transform))
  (quail-error "The transforms ~S and ~S do not have matching dimensions" at1 
           at2))
  
(defmethod premultiply-transform! ((at affine-transform)
                                   (multiplier affine-transform))
  
  
  ;;; premultiply AFFINE-TRANSFORM AT by another AFFINE-TRANSFORM, MULTIPLIER,
  ;;; storing result in AT
  
  (if (not (eql (dimension-of at)
                (dimension-of multiplier)))
    (error-mismatched-dimensions at multiplier)
    
    ;; else
    (let* ((size (1+ (dimension-of at)))
           (mat1 (matrix-of multiplier))
           (mat2 (matrix-of at) )
           (mat3 (make-array (list size size) :initial-element 0.0)))
      (dotimes (i size) (dotimes (j size) 
                          (dotimes (k size) 
                            (incf (aref mat3 i j) 
                                  (* (aref mat1 i k) (aref mat2 k j))))))
      (setf (matrix-of at) mat3)))
  at)
  
(defmethod rotate-transform! ((self affine-transform)
                              (angle number)
                              &optional
                              (radians? nil))
  
  ;; rotates the AFFINE-TRANSFORM SELF by ANGLE
  (quail-cerror "Method has been implemented " 
          "This method does nothing-please implement"))
  
(defmethod rotate-transform! ((self 2d-affine)
                              (angle number)
                              &optional
                              (radians? nil))
  
  ;; rotates the 2D-AFFINE SELF by ANGLE
  (unless radians?
    (setq angle (/ angle 180.0)))
  (let* ((c (cos angle))
         (s (sin angle))
         (mat (matrix-of self))
         (a00 (aref mat 0 0))
         (a01 (aref mat 0 1))
         (a10 (aref mat 1 0))
         (a11 (aref mat 1 1)))
    (setf (aref mat 0 0) (- (* c a00) (* s a10)))
    (setf (aref mat 0 1) (- (* c a01) (* s a11)))
    (setf (aref mat 1 0) (+ (* c a00) (* s a10)))
    (setf (aref mat 1 1) (+ (* c a01) (* s a11)))))
  
(defmethod set-to-identity ((self affine-transform))
  
  ;; the AFFINE-TRANSFORM SELF becomes the identity
  (let* ((matrix (matrix-of self))
         (dim (array-dimension matrix 0)))
    (dotimes (i dim)
      (dotimes (j dim)
        (setf (aref matrix i j)  0.0))
      (setf (aref matrix i i)  1.0))))
  
(defmethod scale-transform! ((self affine-transform)
                             (scale number)
                             &optional &rest other-scales)
  
  ;; scales the AFFINE-TRANSFORM SELF by SCALES in the x-direction and by
  ;; OTHER-SCALES in the remaining   directions. If OTHER-SCALES is  not
  ;; present,SCALE is applied in all to  directions
  (let ((scale-list (if other-scales
                      (cons scale
                              other-scales)
                      (make-list (dimension-of self)
                                 :initial-element scale)))
        (mat (matrix-of self)))
    (loop for scale-amt in scale-list 
          for i upfrom 0 do
          ; (loop for j from 0 to (dimension-of self) do
          (let ((dos (dimension-of self))
                (do ((j 0 (incf j)))
                    ((> j dos))
                  (setf (aref mat i j) (* (aref mat i j)  scale-amt)))))))
  
(defmethod translate-transform! ((self affine-transform)
                                 (delta number)
                                 &optional &rest other-deltas)
  
  ;; translates  the AFFINE-TRANSFORM SELF by DELTA  in the x-direction
  ;; and by OTHER-DELTAS  in the other directions. If OTHER-DELTAS are
  ;; not present,DELTA  is applied to all directions
  (loop with dim = (dimension-of self)
        with mat = (matrix-of self)
        with delta-list = (if other-deltas
                            (cons delta
                                    other-deltas)
                            (make-list dim :initial-element delta))
        for delta-amt in delta-list for i upfrom 0 do
        (incf (aref mat i dim)
              delta-amt)))

(defmethod invert-transform ((at 2d-affine )
  ;; inverts the transform at
  (let* ((size (dimension-of at))
         (new-at (make-affine-transform size))
         (mat (matrix-of at))
         (new-mat (matrix-of new-at))
         (d (- (* (aref mat 0 0 ) (aref mat 1 1 ))
               (* (aref mat 0 1 ) (aref mat 1 0 )))))
    (if (zerop d) 
      (quail-error "Cannot invert transform  ~S " at)
      (let ((ox (aref mat 0 2)) (oy (aref mat 1 2))
            (sx  (aref mat 0 0)) (sy (aref mat 1 1))
            (rxy (aref mat 0 1)) (ryx (aref mat 1 0)))
      (setf (aref new-mat 0 0)  (/ sy d)
            (aref new-mat 1 1)  (/ sx d)
            (aref new-mat 0 1)  (/ (- rxy) d)
            (aref new-mat 1 0)  (/ (- ryx) d)
            (aref new-mat 0 2)  (/ (- (* rxy oy) (* sy ox)) d)
            (aref new-mat 1 2)  (/ (- (* ryx ox) (* sx oy)) d))))
    new-at)) 
  
(defmethod make-affine-transform ((dimension integer))
  (let ((array  (make-array (list (1+ dimension) (1+ dimension)) 
                            :initial-element 0.0)))
    (dotimes (i (1+ dimension))  (setf (aref array i i) 1.0))
    (make-instance 'affine-transform 
                   :dimension dimension :affine array)))
  
  
(defmethod make-affine-transform ((dimension (eql 3)))
  (change-class (call-next-method)
                '3d-affine))
  
(defmethod make-affine-transform ((dimension (eql 2)))
  (change-class (call-next-method)
                '2d-affine))

(defmethod x-shift ((affine affine-transform )) 
  (aref (matrix-of affine) 0 2))

(defmethod y-shift ((affine affine-transform )) 
  (aref (matrix-of affine) 1 2))

(defmethod x-scale ((affine affine-transform )) 
  (aref (matrix-of affine) 0 0))

(defmethod y-scale ((affine affine-transform )) 
  (aref (matrix-of affine) 1 1))

(eval-when (eval load)
  (defvar *identity-2d-transform* (make-affine-transform 2)))
