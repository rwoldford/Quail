;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                            deviance.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   (c) Copyright Statistical Computing Laboratory
;;;       University of Waterloo
;;;       1995
;;;
;;;  Authors:
;;;     D.G. Anglin 1992
;;;     R.W. Oldford 1995
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(deviance)))

(defgeneric deviance (family-or-fit &rest initargs &key &allow-other-keys))

(defmethod-multi deviance ((family (string symbol)) &rest initargs)
  (apply #'deviance (find-family family) initargs))

(defmethod deviance ((family gaussian-family) &rest initargs &key mu y w)
  (declare (ignore initargs))
  (with-slots (deviances-fn) family
    (.* (tp w) (expt (fn-call deviances-fn mu y) 2))))

(defmethod-multi deviance ((family (binomial-family
                                    inverse-gaussian-family))
                           &rest initargs &key mu y w)
  (declare (ignore initargs))
  (with-slots (deviances-fn) family
    (.* (tp w) (fn-call deviances-fn mu y))))

(defmethod-multi deviance ((family (poisson-family
                                    gamma-family)) &rest initargs &key mu y w)
  (declare (ignore initargs))
  (with-slots (deviances-fn) family
    (* 2 (.* (tp w) (fn-call deviances-fn mu y)))))


