;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               model-frame.lisp                              
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1992 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     Greg Anglin 1992.
;;;     R.W. Oldford 1995
;;;
;;;
;;;--------------------------------------------------------------------------------
;;;

(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(model-frame special-formula-operator-handler)))

;;; see the comments in additive-formula-semantics about VREF1, etc.

(defclass model-frame (data-frame)
  ((aux-frame :accessor aux-frame-of :initarg :aux-frame)))

(defun empty-model-frame (&optional (aux t))
  (make-instance 'model-frame
                 :data nil
                 :data-names nil
                 :data-types nil
                 :aux-frame (if aux (empty-model-frame nil))))

(defmethod model-frame ((formula additive-formula) 
                        (raw-data data-frame)
                        &optional model-frame)
  (let ((model-frame (or model-frame (empty-model-frame))))
    (with-slots (reduced-variables) formula
      (loop for v in reduced-variables
            do (ensure-variable v model-frame raw-data
                                formula)))
    (dataset model-frame :identifiers (list-identifiers raw-data))
    model-frame))

(defmethod aux-eref ((r model-frame) &rest args)
  (with-slots (aux-frame) r
    (let ((item (apply #'eref r args)))
      (if (and aux-frame (eq :none item))
        (apply #'eref aux-frame args)
        item))))

(defmethod ref-kernel ((ref-r model-frame) r args)
  (with-slots ((r-aux-frame aux-frame)) r
    (with-slots ((ref-r-aux-frame aux-frame)) ref-r
      (setf ref-r-aux-frame r-aux-frame)))
  (call-next-method ref-r r args))

;;  I'll cheat here and use the internals of data frames

(defun ensure-variable (v new-frame old-frame formula)
  (with-slots (variable-table) formula
    (multiple-value-bind (vcalc vnames)
                         (vfind v variable-table)
      (with-slots (data-names) new-frame
        (let ((pos (position (first (last vnames))
                             data-names
                             :key #'(lambda (x) (first (last x)))
                             :test #'equal)))
          (if pos
            (if (> (length vnames) (length (elt data-names pos)))
              (setf (elt data-names pos) vnames))
            (multiple-value-bind (new-data new-type)
                                 (calculate-variable vnames
                                                     vcalc
                                                     new-frame
                                                     old-frame
                                                     formula)
              (add-data new-frame new-data (first vnames)
                        :type new-type
                        :nicknames (rest vnames))))))
      vnames)))

(defun calculate-variable (vnames vcalc new-frame old-frame 
                                  formula)
  (let ((aux-frame (or (aux-frame-of new-frame) new-frame))
        (canonical-name (first (last vnames))))
    (multiple-value-bind (datum type)
                         (eref aux-frame canonical-name)
      (if (eq datum :none)
          (apply (symbol-function (vget vcalc))
                 old-frame
                 new-frame
                 formula
                 (vargs vcalc))
        (values datum type)))))

(defun ensure-args (args old-frame new-frame formula)
  (loop for arg in args
        collect (cond ((not (stringp arg))
                       arg)
                      ;; cond returns the value of a non-nil predicate
                      ;; when there are no forms ... ie. foo if it's not :none
                      ((let ((foo (aux-eref new-frame arg)))
                         (if (eq foo :none) nil foo)))
                      (t
                       (let ((aux-frame (or (aux-frame-of new-frame) new-frame)))
                         (ensure-variable arg aux-frame old-frame
                                          formula)
                         (eref aux-frame arg))))))

;;;  Here is the implementation of VINTERCEPT, VREF1, VREF2, VREF3, and VREF4

;;;  I suppose these really should be generic functions, specializing on
;;;  the class of formula and maybe even old-frame.

#|
"Each of the VREFs has a different meaning when it comes to building a
model frame from a dataset; the intent is

VINTERCEPT    just provide a column of 1's
VREF1         a variable, say \"x\"
VREF2         an indexed variable, say \"z[t 3]\"
VREF3         a function of variable(s), say \"foo(x, z[t 3])\"
VREF4         a lisp operation on the variable(s) \"{(/ z[t 2] z[t 1])}\"

Sometimes the function for VREF3 will be special, and refer to a process
rather than a fixed function.  For example, for generalized additive
models, \"s(x)\" will mean an adaptive smoothing spline fit of the
partial residuals of the response and other fitted terms to x.
(See Hastie and Tibshirani, Generalized Additive Models,
Chapman and Hall, 1990).

Each of these is implemented as a function which returns at most 2 values:

0:  the variable to be put in the model frame
1:  the type of the variable (the class of return value 0).
"
|#

(defun vintercept (old-frame new-frame formula)
  (declare (ignore old-frame formula))
  ;; since the response is already in new-frame, we can cheat and
  ;; use data-size on it to get the dimensions right.
  (make-instance 'ones-array :dimensions (list (data-size new-frame))))

(defun vref1 (old-frame new-frame formula variable)
  (declare (ignore new-frame formula))
  (eref old-frame variable))

(defun vref2 (old-frame new-frame formula variable &rest ref-args)
  (declare (ignore new-frame formula))
  (multiple-value-bind (data type) 
                       (eref old-frame variable)
    (values (apply #'ref data ref-args)
            type)))

(defun vref3 (old-frame new-frame formula operator-name &rest args)
  (let ((ensured-args (ensure-args args old-frame
               new-frame formula))
        (operator-symbol (intern (string-upcase operator-name) :keyword)))
      (apply #'special-formula-operator-handler
             ;; use the operator-symbol so we can write (eql ...) methods
             operator-symbol
             formula
             operator-name
             ensured-args)))

(defun vref-function-or-macro-dispatch (function-symbol ensured-args)
  (let ((func (symbol-function function-symbol)))
    (if (functionp func)
      (apply func
             ensured-args)
      (eval `(,function-symbol ,@ensured-args)))))

#|
(defun vref4 (old-frame new-frame formula function-symbol &rest args)
  (let ((the-value (apply (symbol-function function-symbol)
                          (ensure-args args old-frame
                                       new-frame formula))))
   (values the-value (class-of the-value))))
|#

(defun vref4 (old-frame new-frame formula function-symbol &rest args)
  (let ((the-value 
         (vref-function-or-macro-dispatch function-symbol
                                          (ensure-args args old-frame
                                                       new-frame formula))))
    (values the-value (class-of the-value))))

(defgeneric special-formula-operator-handler 
  (operator-symbol formula operator-name &rest ensured-args)
  (:documentation 
   "Handles special operators in model formulae. Usually specializes on ~
    (operator-symbol (eql ...)) and (formula ...), and manipulates ~
    ensured-args. For example, for GAMs ~
    we have a method for (operator-symbol (eql :s)) and ~
    (formula additive-formula) to handle s(...), meaning spline smooth to ~
    the fit method.  If there is no (eql ...) method, the function given by ~
    (read-from-string operator-name) is called on ensured-args."
  ))

#|
(defmethod special-formula-operator-handler 
           ((operator-symbol t) formula operator-name &rest ensured-args)
  "This is the default case ... treat the operator-name as a lisp function ~
   on the ensured-args ie. no special behavior"
  (declare (ignore formula))
  (let ((the-value (apply (symbol-function (read-from-string operator-name))
                          ensured-args)))
   (values the-value (class-of the-value))))
|#
  
(defmethod special-formula-operator-handler 
           ((operator-symbol t) formula operator-name &rest ensured-args)
  "This is the default case ... treat the operator-name as a lisp function ~
   on the ensured-args ie. no special behavior"
  (declare (ignore formula))
  (let ((the-value 
         (vref-function-or-macro-dispatch (read-from-string operator-name)
                                          ensured-args)))
    (values the-value (class-of the-value))))
                   
                   
                   
      
