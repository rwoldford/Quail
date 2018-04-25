;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               matrix.lisp                               
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

(in-package :quail-kernel)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(matrix)))

;------------------------------------------------------------------------------

(defclass matrix (num-array)
  ())

(push-extension-class 'matrix)

;; This is a tad tricky since slots don't have their values yet !!

(defmethod initialize-instance :before ((self matrix) &rest initargs)
  (let* ((p (position :dimensions initargs)))
    (if (and p (< (+ 1 p) (length initargs)))
      (let ((dimensions (elt initargs (+ 1 p))))
        (if (> (length dimensions) 2)
          (quail-error "A matrix must have 2 or fewer dimensions, hence ~
                  ~&~S is an invalid dimensions specifier."
                 dimensions))))))

;;  This is a bit tricky as well ... notice that similar things are required
;;  to be defined for similar functionality in similar situations (for
;;  example, for foreign-array) ... this is NOT inherited !!

(defmethod make-instance ((class-name (eql 'num-array)) &rest initargs)
  (let* ((class (find-class 'num-array))
         (p (position :dimensions initargs)))
    (if (and p (< (+ 1 p) (length initargs)))
      (let ((dimensions (elt initargs (+ 1 p))))
        (if (<= (length dimensions) 2)
          (setf class (find-class 'matrix)))))
    (apply #'make-instance class initargs)))

;;
;;  The following is tempting, because it also captures
;;
;;   (make-instance (find-class 'num-array) ...)
;;
;;  However, it should NOT be done, for two reasons:
;;
;;   1.  change-class is slow
;;
;;  but more critically
;;
;;   2.  'matrix is hard-coded here.  Since this is an :after method,
;;       there is no clean way of over-riding this !!
;;
;;  (defmethod initialize-instance :after ((self num-array) &rest initargs)
;;    (with-slots (dimensions) self
;;      (if (and (not (eq dimensions :none))     ; This is
;;               (<= (length dimensions) 2))     ;  undesirable
;;        (change-class self 'matrix))))         ;   code (see above)
;;
;;  Code should use the (make-instance some-class-name ...) form rather
;;  than (make-instance some-class ...) whenever possible.  See
;;  file Z:quail-object.lisp for more info on how properly to do 
;;  make-instances in Z.

;------------------------------------------------------------------------------

;;;
;  return-class structure (see return-class.lisp)
;

(put-return-class 'ref-array
                  'ref-array
                  '(ref-array num-array matrix))

(put-return-class 'num-array
                  'num-array
                  '(num-array matrix))

(put-return-class 'matrix
                  'matrix
                  '(matrix array vector cons  number integer fixnum float
                    rational complex symbol))

;------------------------------------------------------------------------------

;;;
;  DEFMETHODs for matrix
;

(defmethod print-object ((m matrix) stream)
  (print-dim-ref-object m 'M stream)
  m)

#|
(defmethod describe ((m matrix))
  (format t "~&#<matrix ~S> is an instance of class ~S with slots: ~
                  ~{~&~A~3,8@T~S~}"
                  (system-get-pointer m)
                  (class-of m)
                  (list
                  "CONTENTS" (contents-of m)
                  "DIMENSIONS" (dimensions-of m)))
  (values))
|#

         
                                                   
         





