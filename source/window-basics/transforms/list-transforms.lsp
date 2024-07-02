;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                              list-transforms.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;
;;;  This file is an addition to Window-basics for moving pointclouds
;;;  
;;;
;;;  Authors:
;;;     H.A. Chipman 1991
;;;     C.B. Hurley 1991 George Washington University
;;;     
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :wb)


;;;----------------------------------------------------------------------------------
;;; A modified version of affinetrans.lisp. It includes more specific subclasses,
;;; such as:
;;;
;;; affine-transform---- 2d-affine----- 2d-shift
;;;                  \
;;;                   \_ 3d-affine----- 3d-rotate------ 3d-x-rotate
;;;                              |                   |
;;;                              |----- 3d-scale     |- 3d-y-rotate
;;;                              |                   |
;;;                              |_____ 3d-shift     |_ 3d-z-rotate
;;;                                                  |
;;;                                                  |_ 3d-axis-rotate
;;;
;;; The modifications include fixnum math, apply-transform methods for lists of lists,
;;; and the extended classes described above.

(in-package :wb)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(mapply-transform mapply-transform!)))
 
(defclass affine-transform ()
  ((dimension
    :initarg :dimension
    :accessor dimension-of 
    :documentation "The true dimension of the transform, (n * n)")
   (affine 
    :initarg :affine
    :accessor matrix-of 
    :documentation "an (n+1) *(n+1) array of reals, first n rows and colums represent the transformation, the last column represents the location shift")
   )
  (:documentation 
   "Affine transformations for viewing 2 and 3 dimensional objects. See Foley and Van Dam for reference. Note the transforms here operate on columns whereas those in F+VD operate on row vectors"
                  ))


(defclass 2d-affine (affine-transform)
  ((affine 
    :documentation "a 3 by 3 array of reals, first 2 rows and colums represent the transformation, the last column represents the location shift")
   
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
    :documentation "a 4 by 4 array of reals, first 3 rows and colums represent the transformation, the last column represents the location shift")
   
   (x-function 
    :accessor x-function 
    :documentation "function applied to object to retrieve its x location")
   
   (y-function 
    :accessor y-function 
    :documentation "function applied to object to retrieve its y location")
   
   (z-function 
    :accessor z-function 
    :documentation "function applied to object to retrieve its z location")
   
   (dimension 
    :initform 3 
    :documentation " the true dimension of the transform, 3 by 3")))

(defclass 2d-shift (2d-affine)
  ((x-shift :initarg :x-shift
            :accessor x-shift-of
            :documentation "The translation applied to the x-axis")
   (y-shift :initarg :y-shift
            :accessor y-shift-of
            :documentation "The translation applied to the y-axis")))

(defclass 3d-rotate (3d-affine)
  ((angle :initarg :angle
          :accessor angle-of
          :documentation "The angle of rotation")))

(defclass 3d-axis-rotate (3d-rotate)
  ((x-component :initarg :x-component
                :accessor x-component-of
                :documentation "The x-component of the arbitrary axis (length=1)")
   (y-component :initarg :y-component
                :accessor y-component-of
                :documentation "The y-component of the arbitrary axis (length=1)")
   (z-component :initarg :z-component
                :accessor z-component-of
                :documentation "The z-component of the arbitrary axis (length=1)")))

(defclass 3d-x-rotate (3d-axis-rotate) ())

(defclass 3d-y-rotate (3d-axis-rotate) ())

(defclass 3d-z-rotate (3d-axis-rotate) ())

(defclass 3d-shift (3d-affine)
  ((x-shift :initarg :x-shift
            :accessor x-shift-of
            :documentation "The translation applied to the x-axis")
   (y-shift :initarg :y-shift
            :accessor y-shift-of
            :documentation "The translation applied to the y-axis")
   (z-shift :initarg :z-shift
            :accessor z-shift-of
            :documentation "The translation applied to the z-axis")))

(defclass 3d-scale (3d-affine)
  ((x-scale :initarg :x-scale
            :accessor x-scale-of
            :documentation "The scaling applied to the x-axis")
   (y-scale :initarg :x-scale
            :accessor y-scale-of
            :documentation "The scaling applied to the y-axis")
   (z-scale :initarg :z-scale
            :accessor z-scale-of
            :documentation "The scaling applied to the z-axis")))

(defmethod mapply-transform ((self 2d-shift) (a list))
  "Apply a shift transform to each sublist of the list.  ~
   A sublist has fixnums  in the order x,y,z. ~
   This routine returns a *new* list of fixnums. "
  #-:sbcl(declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0)))
         (declare (inline + list))

  (loop with xs fixnum = (x-shift-of self)
        with ys fixnum = (y-shift-of self)
        for (x y) fixnum in a
        collect (list (+ x xs) (+ y ys))))
                      
                                
;;; Note that apply-transform is not optimized for speed like the other apply-transforms
;;; that rotate three-d data.

(defmethod mapply-transform ((self 3d-axis-rotate) (a list))
  "Rotate each sublist of the list about an arbitrary axis  ~
   through a given angle, specified by the transformation. ~
   A sublist has fixnums  in the order x,y,z. ~
   This routine returns a *new* list of fixnums. "
    #-:sbcl(declare (optimize (speed 3) (safety 0)
                       (space 0) (compilation-speed 0)))
             (declare (inline + * - list round))
  (let* ((c (cos (angle-of self)))
         (s (sin (angle-of self)))
         (u (- 1 c))
         (xc (x-component-of self))
         (yc (y-component-of self))
         (zc (z-component-of self))
         )
   
    (loop for (x y z) fixnum in a
          collect 
          (list (round (+ (* x (+ (* u xc xc) c))
                          (* y (- (* u xc yc) (* s zc)))
                          (* z (+ (* u xc zc) (* s yc)))))
                (round (+ (* x (+ (* u xc yc) (* s zc)))
                          (* y (+ (* u yc yc) c))
                          (* z (- (* u yc zc) (* s xc)))))
                (round (+ (* x (- (* u xc zc) (* s yc)))
                          (* y (+ (* u yc zc) (* s xc)))
                          (* z (+ (* u zc zc) c))))))))


#|
(defmethod mapply-transform ((self 3d-x-rotate) (a list))
  "Rotate each sublist of the list about the x-axis ~
  through a given angle, specified by the transformation. ~
  A sublist has fixnums in the order x,y,z. ~
  This routine returns a *new*list of fixnums. "
  #-:sbcl(declare
    (optimize (speed 3) (safety 0)
      (space 0) (compilation-speed 0)))
  (declare (inline + - * ash list) (fixnum c s))
  
  (let ((c (round (* 1024 (cos (angle-of self)))))
        (s (round (* 1024 (sin (angle-of self))))))
  (loop for (x y z) fixnum in a
    collect (list x
                  (ash (- (* y c) (* z s)) -10)
                  (ash (+ (* y s) (* z c)) -10)))))
|#

(defmethod mapply-transform ((self 3d-x-rotate) (a list))
  "Rotate each sublist of the list about the y-axis  ~
   through a given angle, specified by the transformation. ~
   A sublist has fixnums  in the order x,y,z. ~
   This routine returns a *new* list of fixnums."
    #-:sbcl(declare 
             (optimize (speed 3) (safety 0)
                       (space 0) (compilation-speed 0)))
             (declare (inline + - * ash list))
  
  (let ((c (round (* 1024 (cos (angle-of self)))))
        (s (round (* 1024 (sin (angle-of self))))))
    (loop for (x y z) fixnum in a
          collect (list (ash (- (* y c) (* z s)) -10)
                        y
                        (ash (+ (* y s) (* z c)) -10)))))


(defmethod mapply-transform ((self 3d-y-rotate) (a list))
  "Rotate each sublist of the list about the y-axis  ~
   through a given angle, specified by the transformation. ~
   A sublist has fixnums  in the order x,y,z. ~
   This routine returns a *new* list of fixnums."
    #-:sbcl(declare 
             (optimize (speed 3) (safety 0)
                       (space 0) (compilation-speed 0)))
             (declare (inline + - * ash list))
  
  (let ((c (round (* 1024 (cos (angle-of self)))))
        (s (round (* 1024 (sin (angle-of self))))))
    (loop for (x y z) fixnum in a
          collect (list (ash (+ (* x c) (* z s)) -10)
                        y
                        (ash (- (* z c) (* x s)) -10)))))


(defmethod mapply-transform ((self 3d-z-rotate) (a list))
  "Rotate each sublist of the list about the z-axis  ~
   through a given angle, specified by the transformation. ~
   A sublist has fixnums  in the order x,y,z. ~
   This routine returns a *new* list of fixnums."
    #-:sbcl(declare 
             (optimize (speed 3) (safety 0)
                       (space 0) (compilation-speed 0)))
             (declare (inline + - * ash list))
  (let ((c (round (* 1024 (cos (angle-of self)))))
        (s (round (* 1024 (sin (angle-of self))))))
    (loop for (x y z) fixnum in a
          collect (list (ash (- (* x c) (* y s)) -10)
                        (ash (+ (* x s) (* y c)) -10)
                        z))))

(defmethod mapply-transform ((self 3d-shift) (a list))
  "Shift each sublist of the list along all three axes. ~
   A sublist has fixnums  in the order x,y,z. ~
   This routine returns a *new* list of fixnums. "
  #-:sbcl(declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0)))
           (declare (inline + list))
  (loop with xs fixnum  = (x-shift-of self)
        with ys fixnum = (y-shift-of self)
        with zs fixnum = (z-shift-of self)
        for (x y z) fixnum in a
        collect (list (+ x xs) (+ y ys) (+ z zs))))


(defmethod mapply-transform ((self 3d-scale) (a list))
  "Scale each sublist of the list along all three axes. ~
   A sublist has fixnums  in the order x,y,z. ~
   This routine returns a *new* list of fixnums."
  #-:sbcl(declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0)))
           (declare (inline * list))
  (loop with xs fixnum  = (x-scale-of self)
        with ys fixnum = (y-scale-of self)
        with zs fixnum = (z-scale-of self)
        for (x y z) fixnum in a
        collect (list (* x xs) (* y ys) (* z zs))))
                      
;---------------------------------------------------------------------------
;
; the same routines as above, but ones that destructively change the contents
;  of the lists
;
;---------------------------------------------------------------------------

(defmethod mapply-transform! ((self 2d-shift) (a list))
  "apply a shift transform to a list of lists. Observations correspond to lists~
   fixnum calculations are used, and ~
   an fixnum LIST MUST BE GIVEN TO THIS ROUTINE!!!"
  #-:sbcl(declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0)))
           (declare (inline first second))
           ;inline first second incf)) ;16 November 2019
  (loop with xs fixnum = (x-shift-of self)
        with ys fixnum = (y-shift-of self)
        for ai in a do 
        (incf (first ai) xs) 
        (incf (second ai) ys)))

                                
;;; Note that apply-transform is not optimized for speed like the other apply-transforms
;;; that rotate three-d data.

(defmethod mapply-transform! ((self 3d-axis-rotate) (a list))
  "apply a shift transform to each sublist of the list ~
   A sublist has fixnums  in the order x,y,z. 
   This routine modifies the fixnum list "
    #-:sbcl(declare (optimize (speed 3) (safety 0)
                       (space 0) (compilation-speed 0)))
             (declare (inline first second third round + * -))
  (let* ((c (cos (angle-of self)))
         (s (sin (angle-of self)))
         (u (- 1 c))
         (xc (x-component-of self))
         (yc (y-component-of self))
         (zc (z-component-of self))
         )
    (loop for (x y z) fixnum in a 
          for ai in a do
          (setf (first ai)
                (round (+ (* x (+ (* u xc xc) c))
                          (* y (- (* u xc yc) (* s zc)))
                          (* z (+ (* u xc zc) (* s yc))))))
          (setf (second ai)
                (round (+ (* x (+ (* u xc yc) (* s zc)))
                          (* y (+ (* u yc yc) c))
                          (* z (- (* u yc zc) (* s xc))))))
          (setf (third ai)
                (round (+ (* x (- (* u xc zc) (* s yc)))
                          (* y (+ (* u yc zc) (* s xc)))
                          (* z (+ (* u zc zc) c))))))))

(defmethod mapply-transform! ((self 3d-x-rotate) (a list))
  "Rotates each sublist of the list about the x-axis.~
   A sublist has fixnums  in the order x,y,z. 
   This routine modifies the fixnum list "
    #-:sbcl(declare 
             (optimize (speed 3) (safety 0)
                       (space 0) (compilation-speed 0)))
             (declare (inline ash second third + * -))  
  (let ((c (round (* 1024 (cos (angle-of self)))))
        (s (round (* 1024 (sin (angle-of self))))))
    (loop for (x y z) fixnum in a 
          for ai in a do
          (setf (second ai) (ash (- (* y c) (* z s)) -10))
          (setf (third ai) (ash (+ (* y s) (* z c)) -10)))))



(defmethod mapply-transform! ((self 3d-y-rotate) (a list))
  "Rotates each sublist of the list about the y-axis.~
   A sublist has fixnums  in the order x,y,z. 
   This routine modifies the fixnum list "
    #-:sbcl(declare 
             (optimize (speed 3) (safety 0)
                       (space 0) (compilation-speed 0)))
             (declare (inline ash first third + * -))
  (let ((c (round (* 1024 (cos (angle-of self)))))
        (s (round (* 1024 (sin (angle-of self))))))
    (loop for (x y z) fixnum in a
          for ai in a  do
          (setf (first ai) (ash (+ (* x c) (* z s)) -10)
                (third ai) (ash (- (* z c) (* x s)) -10)))))



(defmethod mapply-transform! ((self 3d-z-rotate) (a list))
  "Rotates each sublist of the list about the z-axis.~
   A sublist has fixnums  in the order x,y,z. 
   This routine modifies the fixnum list "
    #-:sbcl(declare 
             (optimize (speed 3) (safety 0)
                       (space 0) (compilation-speed 0)))
             (declare (inline ash second first + * -))
  (let ((c (round (* 1024 (cos (angle-of self)))))
        (s (round (* 1024 (sin (angle-of self))))))
    (loop for (x y z) fixnum in a
          for ai in a  do
          (setf (first ai) (ash (- (* x c) (* y s)) -10)
                (second ai) (ash (+ (* x s) (* y c)) -10)))))
  
(defmethod mapply-transform! ((self 3d-shift) (a list))
  "Shifts each sublist of the list along all axes.~
   A sublist has fixnums  in the order x,y,z. 
   This routine modifies the fixnum list "
  #-:sbcl(declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0)))
          (declare (inline first second third))
          ;(inline incf first second third)) ;16 November 2019
  (loop with xs fixnum = (x-shift-of self)
        with ys fixnum = (y-shift-of self)
        with zs fixnum = (z-shift-of self)
        for ai in a do
        (incf (first ai) xs)
        (incf (second ai) ys)
        (incf (third ai) zs)))



(defmethod mapply-transform! ((self 3d-scale) (a list))
  "Scales each sublist of the list along all axes.~
   A sublist has fixnums  in the order x,y,z. 
   This routine modifies the fixnum list "
  #-:sbcl(declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0)))
           (declare (inline * first second third))
  (loop with xs fixnum = (x-scale-of self)
        with ys fixnum = (y-scale-of self)
        with zs fixnum = (z-scale-of self)
        for (x y z) fixnum in a 
        for ai in  a do
        (setf (first ai) (* x xs) 
              (second ai) (* y ys)
              (third ai) (* z zs))))


(defclass 3d-x-rotate&2d-shift (3d-x-rotate 2d-shift)
  ())


(defclass 3d-y-rotate&2d-shift (3d-y-rotate 2d-shift)
  ())

(defclass 3d-z-rotate&2d-shift (3d-z-rotate 2d-shift)
  ())

(defmethod mapply-transform-store ((self 3d-x-rotate&2d-shift) (a list) store)
  "Rotates each sublist around x-axis, and shifts along x and y~
   A sublist has fixnums  in the order x,y,z. 
   This results are placed in store "
    #-:sbcl(declare 
             (optimize (speed 3) (safety 0)
                       (space 0) (compilation-speed 0)))
             (declare (inline * + - first second third))
  (let ((c (round (* 1024 (cos (angle-of self)))))
        (s (round (* 1024 (sin (angle-of self))))))
    (loop with xs fixnum = (x-shift-of self)
          with ys fixnum = (y-shift-of self)
          for (x y z) fixnum in a 
          for ai in store do
          (setf (first ai) (+ xs x)
                (second ai) (+ ys (ash (- (* y c) (* z s)) -10))
                (third ai) (ash (+ (* y s) (* z c)) -10)))))
        
(defmethod mapply-transform-store ((self 3d-y-rotate&2d-shift) (a list) store)
  "Rotates each sublist around y axis, and shifts along x and y~
   A sublist has fixnums  in the order x,y,z. 
   This results are placed in store "
    #-:sbcl(declare 
             (optimize (speed 3) (safety 0)
                       (space 0) (compilation-speed 0)))
             (declare (inline * + - first second third ash))
  (let ((c (round (* 1024 (cos (angle-of self)))))
                    (s (round (* 1024 (sin (angle-of self))))))
    (loop with xs fixnum = (x-shift-of self)
          with ys  fixnum = (y-shift-of self)
          for (x y z) fixnum in a
          for ai in  store do
          (setf (first ai) (+ xs (ash (+ (* x c) (* z s)) -10))
                (second ai) (+ y ys)
                (third ai) (ash (- (* z c) (* x s)) -10)))))
              


(defmethod mapply-transform-store ((self 3d-z-rotate&2d-shift) (a list) store)
  "Rotates each sublist around z-axis, and shifts along x and y~
   A sublist has fixnums  in the order x,y,z. 
   This results are placed in store "
    #-:sbcl(declare 
             (optimize (speed 3) (safety 0)
                       (space 0) (compilation-speed 0)))
             (declare (inline * + - first second third ash))
  (let ((c (round (* 1024 (cos (angle-of self)))))
        (s (round (* 1024 (sin (angle-of self))))))
    (loop with xs fixnum = (x-shift-of self)
          with ys fixnum = (y-shift-of self)
          for (x y z) fixnum in a
          for ai in  store do
          (setf (first ai) (+ xs (ash (- (* x c) (* y s)) -10))
                (second ai) (+ ys (ash (+ (* x s) (* y c)) -10))
                (third ai) z))))


