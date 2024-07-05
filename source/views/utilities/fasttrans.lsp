;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               fasttrans.lisp
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

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(make-affine-transform affine-trans-p dimension-of matrix-of 
           apply-transform apply-transform! compose-transforms error-mismatched-dimensions
           premultiply-transform! rotate-transform! set-to-identity scale-transform!
           translate-transform! invert-transform
           *identity-2d-transform*)))


(defun make-affine-transform (dimension)
  (let ((array  (make-array (list (1+ dimension) (1+ dimension))
                            :element-type 'real
                            :initial-element 0.0)))
    (dotimes (i (1+ dimension))  (setf (aref array i i) 1.0))
    array
    ))

(defun affine-trans-p (affine) 
  (and (typep affine 'array)
       (= (array-rank affine) 2)))

(defun dimension-of (affine) (- (array-dimension affine 0) 1))

(defun matrix-of (affine) affine)

(defun affine-copy (affine)
  (let* ((size (dimension-of affine))
         (new (make-affine-transform size)))
    (dotimes (i (1+ size))
      (dotimes (j (1+ size))
        (setf (aref new i j) (aref affine i j))))
    new))
        
(defun x-shift (affine) (aref affine 0 2))
(defun y-shift (affine) (aref affine 1 2))

(defun x-scale (affine) (aref affine 0 0))
(defun y-scale (affine) (aref affine 1 1))


(eval-when (:execute :load-toplevel) ;(eval load) 09MAR2022 gwb
  (defvar x-scale(make-affine-transform 2)))


(defun apply-transform (self item)
  ;;; applies the TRANSFORM self to ITEM  , returning the resulting
  ;;; 2d-POSITION /REGION/LIST
  (if (region-p item)
    (let* ((top-right (apply-transform self (top-right-of item)))
          (bottom-left (apply-transform self (bottom-left-of item)))
          (l (2d-position-x bottom-left))
          (r (2d-position-x top-right))
          (b (2d-position-y bottom-left))
          (tp (2d-position-y top-right)))
      (if (> l r) (rotatef l r))
      (if (> b tp) (rotatef b tp))
      (make-region  l r b tp))
    ;;else
    (multiple-value-bind 
      (x y)
      (if (2d-position-p item)
        (values (2d-position-x item) (2d-position-y item))
        (values (first item) (second item)))
      (let ((mat (matrix-of self)) new-x new-y)
        (setq new-x (+ (* x (aref mat 0 0)) (* y (aref mat 0 1))
                       (aref mat 0 2)))
        (setq new-y (+ (* x (aref mat 1 0)) (* y (aref mat 1 1))
                       (aref mat 1 2)))
        (if (listp item) (list new-x new-y)
            (make-2d-position new-x new-y))))))



(defun apply-transform! (at  item)
  
    ;;; applies the 2D-AFFINE at to ITEM, placing results in ITEM arguments 
  
  (let ((top-right (apply-transform at (top-right-of item)))
        (bottom-left (apply-transform at (bottom-left-of item))))
    (setf (bounds-of item)
          (list (2d-position-x bottom-left)
                (2d-position-x top-right)
                (2d-position-y bottom-left)
                (2d-position-y top-right)))
    item))

(defun compose-transforms (at1 at2)
  
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
           (mat3 (matrix-of result)))
      ; (loop for i from 0 to size do 
      (do ((i 0 (incf i)))
          ((> i size))
        ; (loop for j from 0 to size do 
        (do ((j 0 (incf j)))
            ((> j size))
          (setf (aref mat3 i j) 
                ; (loop for k from 0 to size sum
                (do ((sum-mat3 0)
                     (k 0 (incf k)))
                    ((> k size) sum-mat3)
                  (incf sum-mat3 (* (aref mat1 i k) (aref mat2 k j)))))))
      result)))
  
(defun error-mismatched-dimensions (at1 at2)
  (quail-error "The transforms ~S and ~S do not have matching dimensions" at1 
           at2))
  
(defun premultiply-transform! (at multiplier)
  
  ;;; premultiply AFFINE-TRANSFORM AT by another AFFINE-TRANSFORM, MULTIPLIER,
  ;;; storing result in AT
  
  (if (not (eql (dimension-of at)
                (dimension-of multiplier)))
    (error-mismatched-dimensions at multiplier)
    
    ;; else
    (let* ((size (1+ (dimension-of at)))
           (at-copy (affine-copy at))
           (mat3 (matrix-of at-copy))
           (mat1 (matrix-of at)) (mat2 (matrix-of multiplier)))
      (dotimes (i size) 
        (dotimes (j size) 
          (setf (aref mat1 i j) 0.0)
          (dotimes (k size) 
            (incf (aref mat1 i j) 
                  (* (aref mat2 i k) (aref mat3 k j))))))
      ))
  at)
  

  
(defun rotate-transform! (self angle &optional (radians? nil))
  
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
  
(defun set-to-identity (self)
  
  ;; the AFFINE-TRANSFORM SELF becomes the identity
  (let* ((matrix self)
         (dim (array-dimension matrix 0)))
    (dotimes (i dim)
      (dotimes (j dim)
        (setf (aref matrix i j)  0.0))
      (setf (aref matrix i i)  1.0))))
  
(defun scale-transform! (self scale &rest other-scales)
  
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
          (let ((dos (dimension-of self)))
            (do ((j 0 (incf j)))
                ((> j dos))
              (setf (aref mat i j) (* (aref mat i j)  scale-amt)))))))
  
(defun translate-transform! (self delta  &rest other-deltas)
  
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
        (incf (aref mat i dim) delta-amt)))
  
(defun invert-transform (at )
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


(eval-when (:execute :load-toplevel) ;(eval load) 09MAR2022 gwb
  (defvar *identity-2d-transform* (make-affine-transform 2)))
  

