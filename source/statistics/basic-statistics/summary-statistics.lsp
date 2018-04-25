;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                         summary-statistics.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1990 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1991, 1994.
;;;     M.E. Lewis 1991.
;;;
;;;
;;;----------------------------------------------------------------------------
;;;

(in-package :quail)
(eval-when (:compile-toplevel :load-toplevel :execute)
 (export '(sum mean sd var cov ;;trim 
          median quartile iqr five-num-sum range)))

(defun sum (ref-thing)
  "Returns the sum of all elements of the argument."
  (reduce-slices
         #'+ 
         ref-thing))

(defgeneric mean (x)
  (:documentation "Returns the mean of the argument."))

(defmethod-multi mean ((x (symbol number list array ref-array)))
  (/ (sum x) (number-of-elements x)))

#|
(defun trim (x &optional (fraction 0.0))
  "Returns two values: x trimmed on each end ~
   by half the value of the optional argument fraction, ~
   and the leftover part of x.  ~
   The optional argument should be a non-negative fraction < 1.0 ."
  (cond
   ((= fraction 0.0) x)
   ((and (> fraction 0.0)
         (< fraction 1.0))
    (let* ((s-x (sort x #'<= :copy? T))    ;<---- wasteful. need a function that pushes
           (n (number-of-elements x))      ; the highest and lowest numbers to either end
           (lower-indexes
            (loop for i from 0 to (- (floor (* n fraction)) 1)
                  collect i))
           (upper-indexes
            (loop for i from (- (ceiling (* n (- 1 fraction))) 1) to n
                  collect i))
           (index-list (append lower-indexes upper-indexes)))
      (values
       (ref s-x (concatenate 'list '(:c) index-list))
       (ref s-x index-list))))
   ((= fraction 1.0) NaN)
   ((or (< fraction 0.0) (> fraction 1.0))
    (quail-error "TRIM: The fraction argument must be between 0.0 ~
                  and 1.0 inclusively.  ~
                  ~s is not"
                 fraction))))
           
|#


(defgeneric sd (x)
  (:documentation "Returns the standard-deviation of the argument."))

(defmethod-multi sd ((x (symbol number list array ref-array)))
  (let ((n-1 (- (number-of-elements x) 1)))
    (sqrt (/ (sum (expt (- x (mean x)) 2)) n-1))))

(defmethod sd ((x t))
  (sqrt (var x)))
            


(defgeneric var (x)
  (:documentation "Returns the variance of the argument."))

(defmethod-multi var ((x (symbol number list array ref-array)))
  (let ((n-1 (- (number-of-elements x) 1)))
    (/ (sum (expt (- x (mean x)) 2)) n-1)))



(defgeneric cov (x)
  (:documentation "Returns the covariance of the argument."))

(defmethod cov ((x matrix))
  (let* ((diff (sweep #'mean x :slices 1 :copy? T)))
    (/ (.* (tp diff) diff)
       (- (nrows x) 1))))

;;;
;;;
;;;  Other functions / methods:
;;; 
;;;  median (x)
;;;  quartiles (x)
;;;  five-num  (x)   i.e. (min q1 med q3 max)  or some more bizarre Tukeyism
;;;  
;;;  cov (x)   x - a matrix  ... ideally returns a symmetric-matrix or a number
;;;  and then cor (x) for correlation
;;;  

(defgeneric quartile (thing &optional which)
  (:documentation 
   "Returns the quartile of the argument.  Which quartile ~
    depends on the value of the optional argument which.~
    (:optional (:arg which 1 Specifies which quartile 1 2 or 3.))~
    (:see-also quantile-at median)~
    (:elaboration Default behaviour evaluates the quantile at .25 .5 or .75.)"))

(defmethod quartile ((thing t) &optional (which 1))
  "Returns the quartile of the argument.  Which quartile ~
   depends on the value of the optional argument which.~
   (:optional (:arg which 1 Specifies which quartile 1 2 or 3.))~
   (:see-also quantile-at median)~
   (:elaboration Default behaviour evaluates the quantile at .25 .5 or .75.)"
  (if (and (numberp which)
           (member which '(1 2 3) :test #'=))
    (quantile-at thing (float (/ which 4.0)))
    (quail-error "QUARTILE: Which must be one of 1, 2, or 3, not ~s" which)))

(defgeneric median (thing)
  (:documentation "Returns the median of the argument.~
                   (:required (:arg thing Any of a variety of things.)) ~
                   (:see-also quantile-at quartile) ~
                   (:elaboration Default behaviour evaluates the quantile at .5.)")
  )

(defmethod median ((thing t))
  "Returns the median of the argument.~
   (:required (:arg thing Any of a variety of things.)) ~
   (:see-also quantile-at quartile) ~
   (:elaboration Evaluates the quantile at .5.)"
  (quantile-at thing 0.5))

(defgeneric iqr (thing)
  (:documentation
   "Calculates the inter-quartile range of the argument, that is the ~
    absolute difference of the third and fourth quartiles. ~
    (:see-also quartile range)"))

(defmethod iqr ((thing t))
  "Calculates the inter-quartile range of the argument, that is the ~
   absolute difference of the third and fourth quartiles. ~
   (:see-also quartile range)"
  (- (quantile-at thing 0.75)
     (quantile-at thing 0.25)))

(defgeneric five-num-sum (thing)
  (:documentation
   "Calculates the five number summary min quartiles 1 2 and 3 and the max ~
    as appropriate for the argument. ~
    (:see-also quantile-at) ~
    (:elaboration Default calculation is by quantile-at.)"
   ))

(defmethod five-num-sum ((thing t))
  "Calculates the five number summary min quartiles 1 2 and 3 and the max ~
   as appropriate for the argument. ~
   (:see-also quantile-at) ~
   (:elaboration Default calculation is by quantile-at.)"
  (quantile-at thing '(0.0 0.25 0.5 0.75 1.0)))


(defgeneric range (thing)
  (:documentation
   "Calculates the range of thing, that is the ~
    absolute difference maximum and minimum values. ~
    (:see-also iqr)"))

(defmethod-multi range ((thing (sequence array dimensioned-ref-object)))
  "Calculates the range of thing, that is the ~
    absolute difference maximum and minimum values. ~
    (:see-also iqr)"
  (- (max thing) (min thing)))

(defmethod-multi range ((thing prob-measure))
  "Calculates the range of thing, that is the ~
    absolute difference maximum and minimum values. ~
    (:see-also iqr)"
  (- (upper-bound-of thing) (lower-bound-of thing)))
