;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                           continued-fraction.lisp                              
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1992 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1992.
;;;
;;;
;;;-------------------------------------------------------------------------------
;;;
;;;


(in-package :quail-kernel)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(continued-fraction-eval)))

(defun continued-fraction-eval (numerator-fun denominator-fun)
  "Given two functions numerator-fun denominator-fun this function ~
   returns a function which will calculated the continued fraction ~
   expansion whose succesive numeratores are defined by numerator-fun ~
   and whose successive denominators are defined by denominator-fun.  ~
   The continued fraction is d_0(x) + [n_1(x) / [d_1(x) + [n_2(x) / [d_2(x) + ~
   ... ]]]] ~
   where d_i(x) is the i'th denominator evaluated at x and n_i is the ~
   i'th numerator evaluated at x.
   (:required ~
   (:arg numerator-fun A function of two arguments which when applied ~
   to two numbers x and i returns the value of the i'th (i = 1, 2, ~
   3, ... ) numerator ~
   in the continued fraction evaluated at x.)
    (:arg denominator-fun A function of two arguments which when applied ~
   to two numbers x and i returns the value of the i'th (i = 0, 1, ~
   2, ... ) denominator ~
   in the continued fraction evaluated at x.))~
   (:returns A function which will calculate the continued fraction ~
   expansion d_0(x) + [n_1(x) / [d_1(x) + [n_2(x) / [d_2(x) + ~
   ... ]]]] ~
   at arbitrary argument x to a tolerance determined ~
   by the keyword epsilon, and to a maximum number of iterations ~
   given by the keyword max-iterations.  The method of calculation ~
   employs the well-known forward recurrence relations.)"
    
    (flet
      ;; Set up a convergence test function.
      ((close-enough-p (old new eps)
          (if (and old new)
            (< (abs (- old new)) (* eps (abs old)))))
         )
      
      (function
       (lambda (x  &key (epsilon 1.0D-7) (max-iterations 100))
         "This function calculates a continued fraction expansion at the ~
          value of its argument by approximating the continued fraction ~
          by a rational function.  It uses forward recurrence relations ~
          for this calculation and convergence control is given by use of ~
          keyword arguments.  ~
          (:required
            (:arg x The value at which the fraction is to be evaluated.)) ~
          (:key ~
           (:arg epsilon 1.0D-7 The minimal relative difference between successive ~
             iterations to declare convergence.) ~
           (:arg max-iterations 100 The maximum number of iterations to ~
             perform before divergence is declared.)) ~
          (:returns Returns multiple-values: the approximation, the number ~
          of iterations j, the partial numerator at j-1, the partial numerator ~
          at j, the partial denominator at j-1, and the partial denominator ~
          at j.)"
         (let
           ;; Set up of values for the case j = 0
           ((partial-numerator-j-2 NIL)
            (partial-numerator-j-1 1)
            (partial-numerator-j NIL)
            (partial-denominator-j-2 NIL)
            (partial-denominator-j-1 0)
            (partial-denominator-j 1)
            (approximant-j-1 NIL)
            (approximant-j NIL))
           
           ;; Set up starting values.
           (setf partial-numerator-j (funcall denominator-fun x 0))
           (setf approximant-j partial-numerator-j)
           
           ;; Begin iterative evaluation
           (loop for j from 1 do
                 
                 ;; Set the partial numerators' and denominators'
                 ;; indices back 1.
                 (setf partial-numerator-j-2 partial-numerator-j-1)
                 (setf partial-numerator-j-1 partial-numerator-j)
                 (setf partial-denominator-j-2 partial-denominator-j-1)
                 (setf partial-denominator-j-1 partial-denominator-j)
                 
                 ;; Get the new partial numerator
                 (setf partial-numerator-j
                       (+ (* partial-numerator-j-1
                             (funcall denominator-fun x j))
                          (* partial-numerator-j-2
                             (funcall numerator-fun x j))))
                 
                 ;; Get the new partial denominator
                 (setf partial-denominator-j
                       (+ (* partial-denominator-j-1
                             (funcall denominator-fun x j))
                          (* partial-denominator-j-2
                             (funcall numerator-fun x j))))
                 
                 ;; Set the approximant's index back 1
                 (setf approximant-j-1 approximant-j)
                 
                 (unless (zerop partial-denominator-j)
                   ;; Normalize to prevent overflow
                   (setf partial-numerator-j
                         (/ partial-numerator-j partial-denominator-j))
                   (setf partial-numerator-j-1
                         (/ partial-numerator-j-1 partial-denominator-j))
                   (setf partial-denominator-j-1
                         (/ partial-denominator-j-1 partial-denominator-j))
                   (setf partial-denominator-j 1.0)
                   
                   ;; Approximant is now simply partial-numerator-j
                   (setf approximant-j partial-numerator-j)
                   
                   ;; Check for convergence
                   (cond
                    ((> j max-iterations) (return approximant-j)
                     (error "Failure to converge! ~%~
                             Increase max-iterations (now ~s) or ~
                             increase epsilon (the convergence tolerance, now ~s)."
                            max-iterations
                            epsilon))
                    ((close-enough-p approximant-j-1 approximant-j epsilon)
                     (return (values approximant-j
                                     j
                                     partial-numerator-j-1
                                     partial-numerator-j
                                     partial-numerator-j-1
                                     partial-denominator-j))))
                   )
                 )
           )
       )
      )
    )
  )
