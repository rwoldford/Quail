;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                            weight-fn.lisp
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

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(def-weight-fn find-weight-fn)))

(defvar *weight-fn-table* (make-hash-table :test #'equal))

(defun def-weight-fn (family link weight-fn)
  (declare (special *weight-fn-table*))
  (setf (gethash (list family link) *weight-fn-table*) weight-fn))

(defun find-weight-fn (family link)
  (declare (special *weight-fn-table*))
  (let ((weight-fn
         (gethash (list family link) *weight-fn-table*)))
    (or weight-fn
        (let* ((link-deriv (find-deriv (link-of link)))
               (family-variance (variance-fn-of family))
               (result 
                (eval `(fn (mu w)
                           (/ w (* (^ (fn-call ,link-deriv mu) 2)
                                   (fn-call ,family-variance mu))))))
               )
          (def-weight-fn family link result)))))

;;; some basic ones below ... most others are complicated and just as easily
;;; handled in the general code above.

(def-weight-fn gaussian-family identity-link (fn (mu w) w))

(def-weight-fn binomial-family logit-link (fn (mu w) (* w mu (- 1 mu))))

(def-weight-fn poisson-family log-link (fn (mu w) (* w mu)))

(def-weight-fn gamma-family reciprocal-link (fn (mu w) (* w mu mu)))
(def-weight-fn gamma-family log-link (fn (mu w) w))
  
