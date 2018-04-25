;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                            matrix-multiply.lisp                              
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1990, 1994
;;; Statistical Computing Laboratory
;;; University of Waterloo
;;;
;;;
;;;  Authors:
;;;     Greg Anglin 1989, 1990.
;;;     R.W. Oldford 1994.
;;;
;;;--------------------------------------------------------------------------------
;;;
;;;  Includes:
;;;          matmult
;;;          dot-times-object
;;;          .*
;;;

(in-package :quail-kernel)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(.* dot-times-object)))

;----------------------------------------------------------------------------------

;;; matrix/vector/scalar by matrix/vector/scalar matrix multiplication

(defun matmult (a b)
  "Matrix multiplication of two objects having dimensions."
  (declare (optimize speed)
           (inline ext_+ ext_*))
  (let* ((dim-a (dimensions-of a))
         (dim-b (dimensions-of b))
         (len-a (length dim-a))
         (len-b (length dim-b)))
    (case len-a
      (2
       (case len-b
         (2 
          (let ((rows-in-a (first dim-a))
                (cols-in-a (second dim-a))
                (rows-in-b (first dim-b))
                (cols-in-b (second dim-b)))
            (if (eq cols-in-a rows-in-b)
              (let ((result
                     (make-dimensioned-result
                      (list rows-in-a cols-in-b) a b)))
                (loop
                  for i from 0 to (- rows-in-a 1)
                  do
                  (loop
                    for j from 0 to (- cols-in-b 1)
                    do
                    (setf
                     (eref result i j)
                     (let ((accum 0))
                       (loop
                         for k from 0 to (- cols-in-a 1)
                         do
                         (setf accum
                               (ext_+ accum (ext_* (eref a i k) (eref b k j)))))
                       accum))))
                result)
              (quail-error "MATMULT: Cannot multiply matrix ~S by matrix ~S.~
                            ~&They are not conformable. ~
                            ~&Dimensions are ~s and ~s respectively."
                           a b dim-a dim-b))))
         (1 
          (let ((rows-in-a (first dim-a))
                (cols-in-a (second dim-a))
                (rows-in-b (first dim-b)))
            (if (= cols-in-a rows-in-b)
              (let ((result (make-dimensioned-result (list rows-in-a) a b)))
                (loop
                  for j from 0 to (- rows-in-a 1)
                  do
                  (setf
                   (eref result j)
                   (let ((accum 0))
                     (loop
                       for i from 0 to (- cols-in-a 1)
                       do
                       (setf accum (ext_+ accum (ext_* (eref a j i) (eref b i)))))
                     accum)))
                (if (= 1 rows-in-a )
                  (eref result 0 0)
                  result))
              (quail-error "MATMULT: Cannot multiply matrix ~S by column vector ~S.~
                            ~&They are not conformable. ~
                            ~&Dimensions are ~s and ~s respectively."
                           a b
                           (matrix-dimensions-of a)
                           (matrix-dimensions-of b)))))
         (0 (if (= (second dim-a) 1)
              (if (= (first dim-a) 1)
                (ext_* (eref a) (eref b))
                (map-element #'ext_* nil a b))
              (quail-error
               "MATMULT: Dimensions ~s and ~s do not conform. ~&~
                Perhaps you meant to use the element-wise multiplication *"
               dim-a
               dim-b))
          )))
      (1
       (case len-b
         (2 
          (let ((rows-in-a (first dim-a))
                (rows-in-b (first dim-b))
                (cols-in-b (second dim-b)))
            (if (= 1 rows-in-b)
              (if (= 1 rows-in-a)
                (if (= cols-in-b 1)
                  (ext_* (eref a) (eref b))
                  (map-element #'ext_* nil a b))
                (if (= cols-in-b 1)
                  (map-element #'ext_* nil a b)
                  (let ((result
                         (make-dimensioned-result (list rows-in-a cols-in-b) a b)))
                    (loop
                      for j from 0 to (- cols-in-b 1)
                      do
                      (loop
                        for i from 0 to (- rows-in-a 1)
                        do
                        (setf (eref result i j) (ext_* (eref a i) (eref b 0 j)))))
                    result)))
              (quail-error "MATMULT: Cannot multiply column vector ~S by matrix ~S.~
                            ~&They are not conformable. ~
                            ~&Dimensions are ~s and ~s respectively."
                           a b
                           (matrix-dimensions-of a)
                           (matrix-dimensions-of b)))))
         (1 (if (= (first dim-b) 1)
              (if (= (first dim-a) 1)
                (ext_* (eref a) (eref b))
                (map-element #'ext_* nil a b))
              (quail-error
               "MATMULT: Dimensions ~s and ~s do not conform. ~&~
                Perhaps you meant to use the element-wise multiplication *"
               (matrix-dimensions-of a)
               (matrix-dimensions-of b)))
          )
         (0 (if (= (first dim-a) 1)
              (ext_* (eref a) (eref b))
              (map-element #'ext_* nil a b)))))
      (0 (case len-b
           (2 (if (= (first dim-b) 1)
                (if (= (second dim-b) 1)
                  (ext_* (eref a) (eref b))
                  (map-element #'ext_* nil a b))
                (quail-error
                 "MATMULT: Dimensions ~s and ~s do not conform. ~&~
                  Perhaps you meant to use the element-wise multiplication *"
                 dim-a
                 dim-b)))
           (1 (quail-error
               "MATMULT: Dimensions ~s and ~s do not conform. ~&~
                Perhaps you meant to use the element-wise multiplication *"
               (matrix-dimensions-of a)
               (matrix-dimensions-of b)))
           (0 (ext_* (eref a) (eref b))))
       )
      )))

;;;
;;;  defun for .*
;;;

(defun .* (&rest args)
  "The matrix multiplication operator or \"dot-product\".~
   Takes arbitrary number of arguments whose dimensions must conform.  ~
   Implemented recursively, calling dot-times-object at each level.  ~
   (:see-also (* :function) (dot-times-object :generic-function))"
  (case (length args)
    (0 1)
    (1 (dot-times-object :identity (first args)))
    (2 (dot-times-object (first args) (second args)))
    (t (apply #'.*
              (dot-times-object (first args) (second args))
              (rest (rest args))))))

;;;                                                  
;  DEFMETHODs of dot-times-object for basic classes
;

(defgeneric dot-times-object (a b)
  (:documentation
   "The generic binary operator for dot product.  (:see-also .*)"
   ))

(defmethod dot-times-object ((a t) (b t))
  (missing-method 'dot-times-object a b))

(defmethod dot-times-object ((a (eql :identity)) (b number))
  b)

(defmethod dot-times-object ((a (eql :identity)) (b symbol))
  (ext_* 1 b 'dot-times-object))

(defmethod-multi dot-times-object ((a (eql :identity))
                                   (b (sequence array dimensioned-ref-object)))
  (let ((result (make-dimensioned-result (dimensions-of b) b)))
    (setf (ref result) b)
    result))

(defmethod dot-times-object ((x1 number) (x2 number))
  (declare (inline *))
  (* x1 x2))

(defmethod-multi dot-times-object ((x1 (number symbol))
                                   (x2 symbol))
  (ext_* x1 x2 'dot-times-object))

(defmethod dot-times-object ((x1 symbol) (x2 number))
  (ext_* x1 x2 'dot-times-object))



(defmethod-multi dot-times-object ((a (symbol number sequence array
                                              dimensioned-ref-object)) 
                                   (b (symbol number sequence array
                                              dimensioned-ref-object)))
  (matmult a b))








