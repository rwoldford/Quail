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

(in-package :wb)

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

(defclass 3d-x-rotate (3d-rotate) ())

(defclass 3d-y-rotate (3d-rotate) ())

(defclass 3d-z-rotate (3d-rotate) ())

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

(defmethod apply-transform ((self 2d-shift) (a array))
  "apply a shift transform to a data array. Observations correspond to rows~
   This routine returns a *new* array"
  (make-array (list (first (array-dimensions a)) 2)
              :initial-contents
              (loop for i from 0 to (- (first (array-dimensions a)) 1)
                      collect (list (+ (aref a i 0) (x-shift-of self))
                                    (+ (aref a i 1) (y-shift-of self))))))
                                
(defmethod apply-transform ((self 3d-axis-rotate) (a array))
  "rotate an array about an arbitrary axis through a given angle, specified~
   by the transformation. Observations in the array correspons to rows~
   in the order x,y,z. This routine returns a *new* array."
  (make-array (list (first (array-dimensions a)) 3)
              :initial-contents
              (let* ((c (cos (angle-of self)))
                     (s (sin (angle-of self)))
                     (u (- 1 c))
                     (xc (x-component-of self))
                     (yc (y-component-of self))
                     (zc (z-component-of self))
                     )
                (loop for i from 0 to (- (first (array-dimensions a)) 1)
                      collect (let ((x (aref a i 0))
                                    (y (aref a i 1))
                                    (z (aref a i 2)))
                                (list (+ (* x (+ (* u xc xc) c))
                                         (* y (- (* u xc yc) (* s zc)))
                                         (* z (+ (* u xc zc) (* s yc))))
                                      (+ (* x (+ (* u xc yc) (* s zc)))
                                         (* y (+ (* u yc yc) c))
                                         (* z (- (* u yc zc) (* s xc))))
                                      (+ (* x (- (* u xc zc) (* s yc)))
                                         (* y (+ (* u yc zc) (* s xc)))
                                         (* z (+ (* u zc zc) c)))))))))

(defmethod apply-transform ((self 3d-x-rotate) (a array))
  "rotate an array about the x axis through a given angle, specified~
   by the transformation. Observations in the array correspons to rows~
   in the order x,y,z. This routine returns a *new* array."
  (make-array (list (first (array-dimensions a)) 3)
              :initial-contents
              (let ((c (cos (angle-of self)))
                    (s (sin (angle-of self))))
                (loop for i from 0 to (- (first (array-dimensions a)) 1)
                      collect (let ((y (aref a i 1))
                                    (z (aref a i 2)))
                                (list (aref a i 0)
                                      (round (- (* y c) (* z s)))
                                      (round (+ (* y s) (* z c)))))))))

(defmethod apply-transform ((self 3d-y-rotate) (a array))
  "rotate an array about the y axis through a given angle, specified~
   by the transformation. Observations in the array correspons to rows~
   in the order x,y,z. This returns a *new* array."
  (make-array (list (first (array-dimensions a)) 3)
              :initial-contents
              (let ((c (cos (angle-of self)))
                    (s (sin (angle-of self))))
                (loop for i from 0 to (- (first (array-dimensions a)) 1)
                      collect (let ((x (aref a i 0))
                                    (z (aref a i 2)))
                                (list (round (+ (* x c) (* z s)))
                                      (aref a i 1)
                                      (round (- (* z c) (* x s)))))))))

(defmethod apply-transform ((self 3d-z-rotate) (a array))
  "rotate an array about the z axis through a given angle, specified~
   by the transformation. Observations in the array correspons to rows~
   in the order x,y,z. This returns a *new* array."
  (make-array (list (first (array-dimensions a)) 3)
              :initial-contents
              (let ((c (cos (angle-of self)))
                    (s (sin (angle-of self))))
                (loop for i from 0 to (- (first (array-dimensions a)) 1)
                      collect (let ((x (aref a i 0))
                                    (y (aref a i 1)))
                                (list (round (- (* x c) (* y s)))
                                      (round (+ (* x s) (* y c)))
                                      (aref a i 2)))))))

(defmethod apply-transform ((self 3d-shift) (a array))
  "shift an array along all three axes specified~
   by the transformation. Observations in the array correspons to rows~
   in the order x,y,z. This returns a *new* array."
  (make-array (list (first (array-dimensions a)) 3)
              :initial-contents
              (loop for i from 0 to (- (first (array-dimensions a)) 1)
                      collect (list (+ (aref a i 0) (x-shift-of self))
                                    (+ (aref a i 1) (y-shift-of self))
                                    (+ (aref a i 2) (z-shift-of self))))))

(defmethod apply-transform ((self 3d-scale) (a array))
  "Scale an array along all three axes specified~
   by the transformation. Observations in the array correspons to rows~
   in the order x,y,z. This returns a *new* array."
  (make-array (list (first (array-dimensions a)) 3)
              :initial-contents
              (loop for i from 0 to (- (first (array-dimensions a)) 1)
                      collect (list (round (* (aref a i 0) (x-scale-of self)))
                                    (round (* (aref a i 1) (y-scale-of self)))
                                    (round (* (aref a i 2) (z-scale-of self)))))))
;---------------------------------------------------------------------------
;
; the same routines as above, but ones that destructively change the contents
;  of the arrays
;
;---------------------------------------------------------------------------

(defmethod apply-transform! ((self 2d-shift) (a array))
  "apply a shift transform to a data array. Observations correspond to rows~
   This routine destructively modifies the given array. Floating point~
   calculations are used."
  (loop for i from 0 to (- (first (array-dimensions a)) 1)
        do (progn
             (incf (aref a i 0) (x-shift-of self))
             (incf (aref a i 1) (y-shift-of self)))))
                                
(defmethod apply-transform! ((self 3d-x-rotate) (a array))
  "rotate an array about the x axis through a given angle, specified~
   by the transformation. Observations in the array correspons to rows~
   in the order x,y,z. This routine destructively modifies the given~
   array. Floating point calculations are used."
  (let ((c (cos (angle-of self)))
        (s (sin (angle-of self))))
    (loop for i from 0 to (- (first (array-dimensions a)) 1)
          do (let ((y (aref a i 1))
                   (z (aref a i 2)))
               (setf (aref a i 1) (- (* y c) (* z s)))
               (setf (aref a i 2) (+ (* y s) (* z c)))))))

(defmethod apply-transform! ((self 3d-y-rotate) (a array))
  "rotate an array about the y axis through a given angle, specified~
   by the transformation. Observations in the array correspons to rows~
   in the order x,y,z. This routine destructively modifies the given array.~
   Floating point calculations are used."
  (let ((c (cos (angle-of self)))
        (s (sin (angle-of self))))
    (loop for i from 0 to (- (first (array-dimensions a)) 1)
          do (let ((x (aref a i 0))
                   (z (aref a i 2)))
               (setf (aref a i 0) (+ (* x c) (* z s)))
               (setf (aref a i 2) (- (* z c) (* x s)))))))

(defmethod apply-transform! ((self 3d-z-rotate) (a array))
  "rotate an array about the z axis through a given angle, specified~
   by the transformation. Observations in the array correspons to rows~
   in the order x,y,z. This routine destructively modifies the given array.~
   Floating point calculations are used."
  (let ((c (cos (angle-of self)))
        (s (sin (angle-of self))))
    (loop for i from 0 to (- (first (array-dimensions a)) 1)
          do (let ((x (aref a i 0))
                   (y (aref a i 1)))
               (setf (aref a i 0) (- (* x c) (* y s)))
               (setf (aref a i 1) (+ (* x s) (* y c)))))))

(defmethod apply-transform! ((self 3d-shift) (a array))
  "shift an array along all three axes specified~
   by the transformation. Observations in the array correspons to rows~
   in the order x,y,z. This routine destructively modifies the given array.~
   Floating point calculations are used."
  (loop for i from 0 to (- (first (array-dimensions a)) 1)
        do (progn
             (incf (aref a i 0) (x-shift-of self))
             (incf (aref a i 1) (y-shift-of self))
             (incf (aref a i 2) (z-shift-of self)))))

(defmethod apply-transform! ((self 3d-scale) (a array))
  "Scale an array along all three axes specified~
   by the transformation. Observations in the array correspons to rows~
   in the order x,y,z. This routine destructively modifies the given array.~
   Floating point calculations are used."
  (loop for i from 0 to (- (first (array-dimensions a)) 1)
        do (progn
             (setf (aref a i 0) (round (* (aref a i 0) (x-scale-of self))))
             (setf (aref a i 1) (round (* (aref a i 1) (y-scale-of self))))
             (setf (aref a i 2) (round (* (aref a i 2) (z-scale-of self)))))))
