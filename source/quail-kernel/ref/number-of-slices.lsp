;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               number-of-slices.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (C) 1994 Statistical Computing Laboratory, University Of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1994
;;;
;;;
;;;--------------------------------------------------------------------------------

(in-package :quail-kernel)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(number-of-slices)))

(defgeneric number-of-slices (object slices)
  (:documentation
   "The number of slices in object. ~
    (:required ~
    (:arg object The object whose number of slices are to be determined.) ~
    (:arg slices The list of fixed dimensions that identify each slice.~
    This should be an index or list of indices that identifies ~
    where in the object the positions are to be determined.  For example if slices ~
    is 0 then a slice is defined to be the set of all elements having a common ~
    value of the zeroth index. ~
    Slices may also be the keyword :elements in which case number-of-slices ~
    is identical to number-of-elements,)~
    ) ~
    (:see-also number-of-elements number-of-dimensions)~
    "))

(defmethod number-of-slices (object slices)
  (missing-method 'number-of-slices object slices))

(defmethod number-of-slices ((object symbol) slices)
  (declare (ignore object slices))
  1)

(defmethod number-of-slices ((object number) slices)
  (declare (ignore object slices))
  1)

(defmethod number-of-slices ((object T) (slices null))
  (declare (ignore object slices))
  1)

(defmethod number-of-slices :around ((object T) (slices list))
  (if slices
    (if (or
         (>= (apply #'max slices)
            (number-of-dimensions object))
         (< (apply #'min slices) 0))
      (quail-error "~&Slices ~s are invalid ~%~
                    for ~s having dimensions: ~s."
                   slices object (dimensions-of object))
      (call-next-method))
    (call-next-method)))

(defmethod-multi number-of-slices ((object (sequence dimensioned-ref-object array))
                                   (slices list))
  (let ((dimensions (dimensions-of object)))
    (cond ((or (null dimensions) (null slices))
           1)
          (t (let ((product 1))
               (loop for index in slices do 
                     (setf product (* product (elt dimensions index))))
               product)))))

(defmethod-multi number-of-slices ((object (T sequence dimensioned-ref-object array))
                                   (slices (eql :elements)))
  (number-of-elements object))

(defmethod-multi number-of-slices ((object (sequence dimensioned-ref-object array))
                                   (slices number))
  (number-of-slices object (list slices)))
