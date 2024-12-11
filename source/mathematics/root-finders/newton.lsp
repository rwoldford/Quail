;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               newton.lisp                              
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

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(newton)))


(proclaim '(sb-ext:maybe-inline newton)) ;24NOV2024
(defun newton (f  &key (start 0.0) (tolerance .0000001) (max-iterations 150) (deriv NIL))
  "Finds the root of the equation f(x) = 0 where f is a real-valued ~
   continuous function of a single numerical argument.  Uses Newton's method.~
   (:required ~
   (:arg f The function whose root we are to find.) ~
   ) ~
   (:key ~
   (:arg start 0.0 A first guess at the root.) ~
   (:arg tolerance .0000001 The relative convergence criterion. ~
   If the absolute difference between the current and previous values is smaller than ~
   the tolerance times the previous value, then the current value ~
   is declared the root.) ~
   (:arg max-iterations 150 The maximum number of iterations to perform before giving ~
   up. If this is reached the most recent value is returned as the first value and ~
   NIL as the second value.) ~
   (:arg deriv NIL The function that is the derivative of f.  If it is not specified ~
   it will be computed within the newton procedure.)) ~
   (:returns Returns multiple values.  The first is the root or the most recent ~
   value depending on whether convergence was reached or not.  The second value ~
   is T if the procedure converged and NIL otherwise.)"
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0)))
  (let ((xold) (xnew start) (converged? NIL))
    ;; Check if derivative is given.  If not, compute it.
    (unless (functionp deriv) (setf deriv (quail::deriv f)))
    (loop
      for i from 1 do
      (setf xold xnew)
      
      ;; Checking for zero divide
      (when (= (funcall deriv xold) 0)
        (format *quail-terminal-io*
                "~&Warning!: Newton hit a zero derivative at the point x = ~s.
                 ~%Continuing in the neighbourhood ...~%"
               xold)
        ;; Restart at a random place nearby.
        (loop for j from 1 to 10
              until (/= (funcall deriv xold) 0)
              do
              (setf xold
                    (* xold (+ .95 (/ (random 100) 1000.0))))))
      
      ;; The Newton step
      (setf xnew (- xold (/ (funcall f xold) (funcall deriv xold))))
      
      ;; Converged?
      (when (< (abs (- xnew xold)) (* tolerance (abs xold)))
        (setf converged? T)
        (return xnew))
      
      ;;Max iterations exceeded?
      (when (>= i max-iterations)
        (return xnew)))
    (values xnew converged?)
    )
)

