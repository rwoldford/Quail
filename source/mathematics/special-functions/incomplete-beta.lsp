;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               incomplete-beta.lisp                              
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1992 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     Dan 
;;;     R.W. Oldford 1992.
;;;
;;;
;;;-------------------------------------------------------------------------------
;;;
;;; This file contains the CL implementation of the special function:
;;; incomplete beta.
;;;
;;; Code is a hand-coded translation of fortran code from Numerical Recipes
;;; Chapter 6, by Press, Flannery, Teukolsky, and Vetterling (1986).
;;;
;;;-------------------------------------------------------------------------------


(in-package :quail-kernel)

(eval-when (:compile-toplevel :load-toplevel :execute) 
   (export '(incomplete-beta)))


;;;----------------------------------------------------------------------------------
;;;
;;;  Incomplete beta function I(a,b,x) is the value of the cumulative distribution
;;;  function of a beta(a,b) random variable evaluated at the point x.
;;;
;;;                          x
;;;                  1      /   a-1      b-1
;;;  I(a,b,x) =  --------  /   t    (1-t)    dt           (a,b > 0)
;;;               B(a,b)  /
;;;                       0
;;;
;;;-----------------------------------------------------------------------------------

(proclaim '(sb-ext:maybe-inline incomplete-beta)) ;24NOV2024
(defun incomplete-beta (a b x &key (max-iterations 100)
                                   (epsilon 3.0D-7))
  "Returns the value of the cumulative distribution ~
   function of a beta(a,b) random variable evaluated at the point x.  ~
   That is the value of the incomplete beta function Ix(a,b)."
  
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           (inline log-gamma incomplete-beta-cf))
  (cond ((= x 0) 0.0)
        ((= x 1) 1.0)
        ((or (< x 0) (> x 1))
         (error "Argument X = ~s is out of range = [0.0 1.0]" x))
        ((or (<= a 0.0) (<= b 0.0))
         (error "Arguments a and b must be greater than 0.0.  Here (a, b) = (~s, ~s)."
                a b))
        (T
         (let ((factor
                ;; Factor in front of the continued fraction.
                (exp (- (+ (* a (log x))
                           (* b (log (- 1.0 x)))
                           (log-gamma (+ a b)))
                        (log-gamma a)
                        (log-gamma b)))))
           (if (< x (/ (+ a 1)
                       (+ a b 2)))
             
             ;; then use continued fraction directly
             (/ (* factor (incomplete-beta-cf a b x
                                              :epsilon epsilon
                                              :max-iterations max-iterations))
                a)
             
             ;; Else use the continued fraction after 
             ;; making the symmetry transformation.
             (- 1
                (/ (* factor
                      (incomplete-beta-cf b a (- 1.0 x)
                                          :epsilon epsilon
                                          :max-iterations max-iterations))
                   b)))))))

(proclaim '(sb-ext:maybe-inline incomplete-beta-cf)) ;24NOV2024
(defun incomplete-beta-cf (a b x &key (max-iterations 100)
                                      (epsilon 3.0D-7))
  "Continued fraction for incomplete-beta function."

  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           (inline incomplete-beta))
  (let*
    ((am 1.0)
     (bm 1.0)
     (az 1.0)
     ;; These q's will be used in factors which occur in the
     ;; coefficients.
     (qab (+ a b))
     (qap (+ a 1.0))
     (qam (- a 1.0))
     (bz (- 1.0 (/  (* qab x) qap))))
    
    ;; Now the continued fraction evaluation by the recurrence method.
    (let (2m d ap bp app bpp a-old )
      (loop for m from 1 by 1
          do
          (setf 2m (+ m m))
          (setf d (/ (* m (- b m) x)
                     (* (+ qam 2m) (+ a 2m))))
          ;; One step (the even one) of the recurrence
          
          (setf ap (+ az (* d am)))
          (setf bp (+ bz (* d bm)))
          (setf d
                (- (/ (* (+ a m) (+ qab m) x)
                      (* (+ a 2m) (+ qap 2m)))))
          
          ;; Next step of the recurrence (the odd one).
          (setf app (+ ap (* d az)))
          (setf bpp (+ bp (* d bz)))
          
          ;; Save the old answer.
          (setf a-old az)
          
          ;; Renormalize to prevent overflows.
          (setf am (/ ap bpp))
          (setf bm (/ bp bpp))
          (setf az (/ app bpp))
          (setf bz 1.0)
          
          #|(print (format NIL
                  "M = ~S ~% 2M = ~S ~% D = ~S ~% Ap = ~S ~%~
                   bp = ~S ~% app = ~S ~% bpp = ~S ~%~
                   a-old = ~S ~%~
                   approximant (az) = ~s ~%"
                  m 2m d ap bp app bpp a-old az)) |#

          ;; Determine exit criteria
          ;; First has it converged?
          (if (< (abs (- az a-old))
                 (* epsilon (abs az)))
            ;; Then return the final result, az.
            (return az))
          
          ;; Second have we reached the max-iterations?
          (if (> m max-iterations) (return az)
            #|(error "Failed to converge: One of beta parameters a or b ~
                    is too large (a = ~s, b = ~s), or the max-iterations ~
                    (= ~s) is too small."
                   a b max-iterations)|#
            )
          )
      )
    )
  )

#|

(defun incomplete-beta-continued-fraction
       (a b x &key (max-iterations 100) (epsilon 1.0D-7))
  "Continued fraction for incomplete-beta function (a b x) <=> Ix(a,b)."
  
  (flet
    ((den-fun (y j)
        (declare (ignore y) (type integer j))
        (cond
         ((= j 0) 0.0)
         (T 1.0)))
     
     (num-fun (y j)
        (declare (type number y) (type integer j))
        (cond
         ((= j 1) 1.0)
         ((oddp j)
          ;; Corresponds to 2m = (j-1)
          (let* ((2m (- j 1))
                 (m (/ 2m 2)))
            (/ (* m (- b m) y)
               (* (+ a 2m -1.0) (+ a 2m)))))
         ((evenp j)
          ;; Corresponds to 2m+1 = (j-1)
          (let* ((2m (- j 2))
                 (m (/ 2m 2)))
            (-
             (/ (* (+ a m) (+ a b m) y)
                (* (+ a 2m) (+ a 2m 1.0)))))))))
    
    (funcall
     (continued-fraction-eval #'num-fun #'den-fun)
     x :max-iterations max-iterations :epsilon epsilon)
    ))
 

(defun test-incomplete-beta (a b x &key (max-iterations 100)
                                   (epsilon 3.0D-7))
  "Returns the value of the cumulative distribution ~
   function of a beta(a,b) random variable evaluated at the point x.  ~
   That is the value of the incomplete beta function Ix(a,b)."
  
  ;;(cerror "a = ~s ~% b = ~s ~% x = ~s ~%" "Inside incomplete-beta" a b x)
  (cond ((= x 0) 0.0)
        ((= x 1) 1.0)
        ((or (< x 0) (> x 1))
         (error "Argument X = ~s is out of range = [0.0 1.0]" x))
        ((or (<= a 0.0) (<= b 0.0))
         (error "Arguments a and b must be greater than 0.0.  Here (a, b) = (~s, ~s)."
                a b))
        (T
         (let ((factor
                ;; Factor in front of the continued fraction.
                (exp (- (+ (* a (log x))
                           (* b (log (- 1.0 x)))
                           (log-gamma (+ a b)))
                        (log-gamma a)
                        (log-gamma b)))))
           (if (< x (/ (+ a 1)
                       (+ a b 2)))
             
             ;; then use continued fraction directly
             (/ (* factor (incomplete-beta-continued-fraction a b x
                                              :epsilon epsilon
                                              :max-iterations max-iterations))
                a)
             
             ;; Else use the continued fraction after 
             ;; making the symmetry transformation.
             (- 1
                (/ (* factor
                      (incomplete-beta-continued-fraction b a (- 1.0 x)
                                          :epsilon epsilon
                                          :max-iterations max-iterations))
                   b)))))))
|#
