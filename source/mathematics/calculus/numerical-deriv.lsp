;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                            numerical-deriv.lisp                              
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1992 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     Ba Pham 1991
;;;     R.W. Oldford 1992.
;;;
;;;--------------------------------------------------------------------------------

(in-package :quail-kernel)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(numerical-deriv)))

(defun numerical-deriv (fun &key x ;; (n 4)
                            (eps 0.2))
  "Numerical derivative of a continuous function fun using a polynomial ~
   of degree three fitting through 4 points in the range +- eps about the ~
   desired point x. ~
   (:references Computational Methods in Elementary Numerical Analysis by ~
   J.L. Morris.) ~
   (:see-also deriv) ~
   (:examples (:files (Differentiation in Quail ~
   q:Examples;Mathematics;Calculus;deriv.lisp ~
   )))"
  (let* ((n 4)
         (low (- x eps))
         (high (+ x eps))
         (h (/ (- high low) n))
         (nn (+ n 1))
         (m (floor n 2))
         (prod 1)
         (isum 0)
         (osum 0))
    (if (not (= (+ low (* m h)) x)) 
      (error "Differentiate: use an even number for n"))
    (dotimes (j nn)
      (dotimes (k nn)
        (if (not (= j k))
          (block one
            (dotimes (i nn)
              (if (and (not (= i j)) (not (= i k)))
                (setf prod (* prod (/ (- m i) (- j i))))))
            (setf isum (+ isum (/ prod (- j k))))
            (setf prod 1))))        
      (setf osum (+ osum (* isum (funcall fun (+ low (* j h))))))
      (setf isum 0))
    (float (/ osum h))))
