;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               make-result.lisp                               
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
;;;          make-dimensioned-result
;;;

(in-package :quail-kernel)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(make-dimensioned-result)))

(defun make-dimensioned-result (dimensions &rest args)
  ;(declare (special NaN))
  (multiple-value-bind (proto-list minimal-class)
                       (interpret args '(:class))
    (let* ((len-dim (length dimensions))
           (ret-class (find-return-class len-dim minimal-class proto-list))
           (result
            (if (eq (class-name (class-of (find-class ret-class))) 'built-in-class) 
              ;;  this is the best we can do for now ... need better subtypep
              (cond ((or (eq ret-class 'array)
                         (eq ret-class 'vector))
                     (make-array dimensions))
                    ((and (or (eq ret-class 'cons)
                              (eq ret-class 'list))
                          (eq len-dim 1))
                     (make-sequence 'list (first dimensions)))
                    (t
                     (array nan :dimensions dimensions)))
              ;;  We do (make-instance ret-class ...) rather than
              ;;  (make-instance (find-class ret-class) ...) because
              ;;  then we catch occurrences of, for example,
              ;;  (defmethod make-instance ((class (eql 'num-array)) ...) ...)
              (make-instance ret-class :dimensions dimensions
                                       :contents :empty
                                       :proto proto-list))))
      result)))
