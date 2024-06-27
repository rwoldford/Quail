;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               ops-low.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1990 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     Greg Anglin 1989, 1990.
;;;
;;;
;;;--------------------------------------------------------------------------------
;;;
;;;  Includes:
;;;          v-op-s
;;;          m-op-s
;;;          v-op-v
;;;          m-op-mv
;;;          op-error
;;;
;;;  Has routines for (op a b) when a and b have <= 2 dimensions each.
;;;  This allows extra speed and special behavior for matrices.
;;;

(in-package :quail-kernel)

;----------------------------------------------------------------------------------

;;; vector with scalar

(defun v-op-s (op op-name v s &optional (dim-v (dimensions-of v)))
  (declare (optimize (speed 3)))
  (declare (ignore op-name))
  (let* ((result (make-dimensioned-result dim-v v s))
         (s-eref (eref s)))
    (loop for i
          from 0
          to (- (first dim-v) 1)
          do (setf (eref result i) (funcall op (eref v i) s-eref)))
    result))

;;; matrix with scalar

(defun m-op-s (op op-name m s &optional (dim-m (dimensions-of m)))
  (declare (optimize (speed 3)))
  (declare (ignore op-name))
  (let* ((result (make-dimensioned-result dim-m m s))
         (s-eref (eref s)))
    (loop for i
          from 0
          to (- (first dim-m) 1)
          do (loop for j
                   from 0
                   to (- (second dim-m) 1)
                   do (setf (eref result i j) (funcall op (eref m i j) s-eref))))
    result))

;;; vector with vector

(defun v-op-v (op op-name a b &optional (dim-a (dimensions-of a))
                                        (dim-b (dimensions-of b)))
  (declare (optimize (speed 3)))
  (let* ((len (first dim-a)))
    (if (eq len (first dim-b))
      (let ((result (make-dimensioned-result dim-a a b)))
        (loop for i
              from 0
              to (- len 1)
              do (setf (eref result i) (funcall op (eref a i) (eref b i))))
        result)
      (op-error op-name a b "The vectors are not the same length.~%"))))

;;; matrix with matrix/vector (including subcases: n x p with n x 1, etc)

(defun m-op-mv (op op-name a b &optional (dim-a (dimensions-of a))
                                         (dim-b (dimensions-of b)))
  (declare (optimize (speed 3)))
  (let* ((num-dim-b (length dim-b))
         (rows-in-a (first dim-a))
         (cols-in-a (second dim-a))
         (rows-in-b (first dim-b))
         (cols-in-b (ecase num-dim-b
                           (1 1)
                           (2 (second dim-b))))
         result)
    (cond ((eq rows-in-a rows-in-b)
              (cond ((eq cols-in-b 1)
                        ;; then b is a 1-dim or 2-dim column-vector
                        (setf result (cglue-replicates cols-in-a b a)))
                    ((eq cols-in-a cols-in-b)
                        ;; then a and b have same shape, with cols-in-b > 1
                        (setf result (sel b :shape t)))
                    (t (op-error op-name a b))))
          ((eq cols-in-a cols-in-b)         
              (cond ((eq rows-in-b 1)
                        ;; then b is a 2-dim row-vector
                        (setf result (rglue-replicates rows-in-a b a)))
                    (t (op-error op-name a b))))
          ((eq cols-in-a rows-in-b)          
              (cond ((eq cols-in-b 1)
                        ;; then b is a 1-dim row-vector
                        (setf result (rglue-replicates rows-in-a b a)))
                    (t (op-error op-name a b))))
          (t (op-error op-name a b)))
    (loop for i
          from 0
          to (- rows-in-a 1)
          do (loop for j
                   from 0
                   to (- cols-in-a 1)
                   do (setf (eref result i j) (funcall op (eref a i j)
                                                          (eref result i j)))))
    result))

#|
;;;  num-array with scalar

(defun n-op-s (op op-name a b &optional (dim-a (dimensions-of a))
                                        (dim-b (dimensions-of b)))
  )

;;;  num-array with num-array

(defun n-op-n (op op-name a b &optional (dim-a (dimensions-of a))
                                        (dim-b (dimensions-of b)))
  )
|#

;;;
;  The op-error generic function
;

(defgeneric op-error (op-name a b &optional more-comments))

;;  This provides a default method for most symbols ...
;;  usually do (defmethod ((op-name (eql 'op)) a b) ...) etc.

(defmethod op-error ((op-name symbol) a b &optional more-comments)
  (if more-comments
    (quail-error "Error attempting to perform ~S with operands ~S and ~S.  ~A"
           op-name
           a
           b
           (format nil more-comments))
    (quail-error "Error attempting to perform ~S with operands ~S and ~S."
           op-name
           a
           b)))



