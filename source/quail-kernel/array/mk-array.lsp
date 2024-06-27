;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               mk-array.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1990, 1991, 1992 Statistical Computing Laboratory, University of Waterloo
;;;
;;; 
;;;  Authors:
;;;     Greg Anglin 1989, 1990, 1991.
;;;     M.E. LEWIS 1991.
;;;     R.W. Oldford 1992.
;;;     Greg Anglin 1993, 1994.
;;;
;;;--------------------------------------------------------------------------------

(in-package :quail-kernel)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(mk-array
          make-ref-array
          shape-as-array)))

;------------------------------------------------------------------------------

#|
(defun mk-array (dimensions &rest rest)
  (multiple-value-bind (class extra)
                       (interpret-keys-only rest '((:class num-array)) 'mk-array t)
    (apply #'make-ref-array class dimensions extra)))
|#

;;  The implementation of make-ref-array is a bit bizarre.  It accepts as its
;;  argument one of three things:
;;
;;        1.  the name of a class (as a symbol)
;;        2.  the class itself (ie. an instance of standard class)
;;        3.  an instance of the class, which will be modified and returned
;;
;;  In cases 1 and 2, case 3 is eventually invoked, although
;;  in case 1, the class of interest may change in the meantime
;;  [ see (defmethod make-ref-array ((class-name (eql 'num-array)) ...) ...) ]

(defgeneric make-ref-array (thing dimensions &rest rest))

(defmethod make-ref-array ((self T) dimensions &rest rest)
   (declare (ignorable self dimensions rest)) ;(declare (ignore self dimensions rest))  25JUL2023
   T)  ;; added May29 98 gwb

(defmethod make-ref-array :around ((self T) dimensions &rest rest)
  (declare (ignorable dimensions rest)) ;(declare (ignore dimensions rest))  25JUL2023
  (error "Called the Quail function MAKE-REF-ARRAY, which is supposed to be ~
          deprecated.  Contact quail-bugs@setosa.uwaterloo.ca."))

#|
(defmethod make-ref-array ((thing t) dimensions &rest rest)
  (declare (ignore dimensions rest))
  (missing-method 'make-ref-array thing))

(defmethod make-ref-array ((class-name symbol) dimensions &rest rest)
  (apply #'make-ref-array (make-instance class-name
                                       :proto :ignore
                                       :dimensions dimensions)
                        dimensions
                        rest))

(defmethod make-ref-array ((class standard-class) dimensions &rest rest)
  (apply #'make-ref-array (make-instance class
                                       :proto :ignore
                                       :dimensions dimensions)
                        dimensions
                        rest))

(defmethod make-ref-array ((self ref-array) dimensions &rest rest)
  (multiple-value-bind (initial-element initial-contents extra)
                       (interpret-keys-only rest
                                            '(:initial-element
                                              :initial-contents)
                                            'make-ref-array
                                            t)
    (cond ((and initial-contents initial-element)
           (quail-error "Must specify only one of :initial-element ~
                   or :initial-contents"))
          (initial-contents
#|
           (if (and (typep initial-contents 'sequence)
                      (not (= (length initial-contents)
                              (first dimensions))))
             (setf initial-contents
                   (shape-as-array initial-contents dimensions)))
|#
           (progn
             (reinitialize-instance self
                                    :dimensions dimensions
                                    :proto initial-contents)
             (apply #'initialize-contents self
                                          initial-contents
                                          extra)))
           (initial-element
            (progn
              (reinitialize-instance self
                                     :dimensions dimensions)
              (apply #'initialize-contents self
                                           nil
                                           :initial-element initial-element
                                           extra)))
            (t
               (progn
                 (reinitialize-instance self
                                        :dimensions dimensions)
                 (apply #'initialize-contents self
                                             :empty
                                             extra)))))
  self)
|#

(defun shape-as-array (sequence dimensions &optional seq-type)
  "Returns a sequence having the same elements as sequence but shaped ~
   according to the list dimensions."
  (unless seq-type
    (setf seq-type (type-of sequence)))
  (cond
   ((eq 'cons seq-type)
    (setf seq-type 'list))
   ((listp (type-of sequence))
    (setf seq-type (first (type-of sequence))))
   )
  
  
  (let* ((first-dim (first dimensions))
         (rest-dim (rest dimensions))
         (new-seq (make-sequence seq-type first-dim)))
    (if rest-dim
      (replace new-seq
               (loop
                 for i from 0 to (- first-dim 1)
                 with length = (apply #'* rest-dim)
                 collect
                 (shape-as-array
                  (subseq sequence (* i length) (* (+ 1 i) length))
                  rest-dim
                  seq-type)))
      (replace new-seq sequence))
    new-seq))
