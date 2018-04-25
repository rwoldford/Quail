;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               simpsons.lisp                              
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1992 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     Stat 440/840: Rupa and Rita

(in-package :quail-kernel)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(extended-simpsons)))

(defun extended-simpsons
       (integrand a b &key (epsilon 1.0D-7) (max-iterations 10))
  "Numerically evaluates the function integrand from a to b.  ~
   Uses an automatically extended simpson's rule."
  (let* ((h (- b a))
         (n 1)
         (iold)
         (inew (/ (* h (+ (funcall integrand a) (funcall integrand b))) 2))
         (sold)
         (snew 0)
         (f-total)
         (tee)
         (diff))
    (loop for i from 1 do
          (setf tee (+ a (/ h 2)))
          (setf f-total 0)
          (loop for j from 1 to n do
                (setf f-total (+ f-total (funcall integrand (+ tee (* (- j 1) h))))))
          (setf iold inew)
          (setf inew (/ (+ iold (* h f-total)) 2))
          (setf sold snew)
          (setf snew (/ (- (* 4 inew) iold) 3))
          (setf h (/ h 2))
          (setf n (* n 2))
          (setf diff (- snew sold))
          
          (if (< (abs diff) (* epsilon (abs sold)))
            (return snew))
          (if (> i max-iterations)
            (quail-error "Extended-simpsons rule faled to converge. ~%~
                          Number of iterations = ~s ~%~
                          Tolerance = ~s . ~%" i epsilon)))))

