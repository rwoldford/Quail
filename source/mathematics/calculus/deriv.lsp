;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               deriv.lisp                              
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1992 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1992, 1994.
;;;
;;;--------------------------------------------------------------------------------

(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(deriv deriv-wrt)))

(defgeneric deriv (function &key wrt)
  (:documentation "Takes the derivative of its first argument with ~
                   respect to the value of the keyword parameter wrt.  ~
                   (:required ~
                   (:arg function The thing to be differentiated. ~
                   Current methods allow handle the cases where function ~
                   is a list, a lambda expression, a function, ~
                   or an fn.) ~
                   ) ~
                   (:key ~
                   (:arg wrt NIL This is the variable that the derivative is ~
                   to be taken with respect to. It should be a symbol or NIL. ~
                   If wrt is not supplied or is NIL, then some attempt is made to ~
                   determine what wrt should be.  This is done by calling the ~
                   function deriv-wrt on the required argument.  Typically, ~
                   this will return the first argument of function.)) ~
                   (:see-also deriv-wrt simplify) ~
                   (:examples (:files (Differentiation ~
                   q:Examples;Mathematics;Calculus;deriv.lisp ~
                   )))
                   "
                  ))

(defgeneric deriv-wrt (function)
  (:documentation "Determines the default parameter name which is to ~
                   be used for purposes of differentiation.  For functions ~
                   and the like this will be the first required parameter in ~
                   the functions lambda-list.  ~
                   Returns NIL if no default parameter could be determined."))



;;;------------------------------------------------------------------------
;;;
;;;  deriv-wrt methods
;;;
;;;------------------------------------------------------------------------

(defun required-parameter-only (symbol)
  "Returns the symbol if its first character is not an ampersand (&), ~
   NIL otherwise."
  (if (not (string-equal (string symbol) "&" :start1 0 :end1 1))
      symbol))

(defmethod deriv-wrt ((function T))
  NIL)


(defmethod deriv-wrt ((function list))
  (if (eq (car function) 'lambda)
    (required-parameter-only (first (second function)))))


(defmethod deriv-wrt ((function function))
  (required-parameter-only (first (qk::get-lambda-list function))))


(defmethod deriv-wrt ((function fn))
  (required-parameter-only (first (arglist-of function))))



;;;------------------------------------------------------------------------
;;;
;;;  deriv methods
;;;
;;;------------------------------------------------------------------------

(defmethod deriv ((function function) &key wrt)
  (let ((f-wrt (deriv-wrt function)))
    (unless wrt (setf wrt (deriv-wrt function)))
    (if (eq wrt f-wrt)
      (eval `(function
              (lambda ,(list wrt)
                (numerical-deriv ,function :x ,wrt)))))))
  

(defmethod deriv ((function fn) &key wrt)
  (unless wrt (setf wrt (deriv-wrt wrt)))
  (let* ((deriv-id (position wrt (arglist-of function) :test #'eql))
         (result (find-deriv function (if deriv-id
                                 (setf deriv-id (list deriv-id))))))
    (unless result 
      (setf result
            (deriv (list* 'lambda (arglist-of function) (body-of function))
                   :wrt wrt))
      (setf result (eval `(fn ,(second result) ,@(rest (rest result)))))
      (add-deriv function result))
    result))

(defmethod-multi deriv ((function (symbol number list)) &key wrt)
  (setf function (simplify function))
  (labels
    (
     ;;
     ;; selectors
     ;;
     
     (first-arg (binary-expr) (second binary-expr))
     (second-arg (binary-expr) (third binary-expr))
     (args (expression) (rest expression))
     (num-args (expression) (length (rest expression)))
     (base (p) (cadr p))
     (power (p) (caddr p))
     (exponent (x) (cadr x))
     (log-arg (x) (cadr x))
     ;(log-base (x) (third x))
     (lambda-arglist (x) (cadr x))
     (lambda-contents (x) (caddr x))
     
     ;;
     ;; predicates
     ;;
     
     (constant-p (x) (qk::extnump x))
     (variable-p (x) (symbolp x))
     (same-variable-p (x y)
       (and (variable-p x) (variable-p y) (eq x y)))
     (sum-p (x) (and (listp x) (eq '+ (car x))))
     (difference-p (x) (and (listp x) (eq '- (car x))))
     (product-p (x) (and (listp x) (eq '* (car x))))
     (quotient-p (x) (and (listp x) (eq '/ (car x))))
     (power-p (x) (and (listp x) (eq 'expt (car x))))
     (exp-p (x) (and (listp x) (eq 'exp (car x))))
     (log-p (x) (and (listp x) (eq 'log (car x))))
     (cos-p (x) (and (listp x) (eq 'cos (car x))))
     (sin-p (x) (and (listp x) (eq 'sin (car x))))
     (tan-p (x) (and (listp x) (eq 'tan (car x))))
     (cosh-p (x) (and (listp x) (eq 'cosh (car x))))
     (sinh-p (x) (and (listp x) (eq 'sinh (car x))))
     (tanh-p (x) (and (listp x) (eq 'tanh (car x))))
     (lambda-p (x)   (and (listp x) (eq 'lambda (car x))))
     (fn-list-p (x)   (and (listp x) (eq 'fn (car x))))
     ;;(funcall-p (x)  (and (listp x) (eq 'funcall (car x))))
     ;;(fn-call-p (x)  (and (listp x) (eq 'fn-call (car x))))
     ;;(apply-p (x)    (and (listp x) (eq 'apply (car x))))
     ;;(arbitrary-proc-call-p (x)  (and x (listp x)))
     
    
     ;;
     ;; constructors
     ;;
     
     (make-sum (a b)
               (cond ((and (qk::extnump a) (qk::extnump b)) (+ a b))
                     ((numberp a) (if (= 0 a)
                                    b
                                    (list '+ a b)))
                     ((numberp b) (if (= 0 b)
                                    a
                                    (list '+ a b)))
                     (T (list '+ a b))))
     
     (make-difference (a b)
               (cond ((and (qk::extnump a) (qk::extnump b)) (- a b))
                     ((numberp a) (if (= 0 a)
                                    (list '- b)
                                    (list '- a b)))
                     ((numberp b) (if (= 0 b)
                                    a
                                    (list '- a b)))
                     (T (list '- a b))))
     
     (make-product (a b)
       (cond ((and (qk::extnump a) (qk::extnump b)) (* a b))
             ((numberp a) (if (= 1 a)
                            b
                            (if (= 0 a)
                              0
                              (list '* a b))))
             ((numberp b) (if (= 1 b)
                            a
                            (if (= 0 b)
                              0
                              (list '* a b))))
             (T (list '* a b))))
     
     
     (make-quotient (a b)
                   (cond ((and (qk::extnump a) (qk::extnump b)) (/ a b))
                         ((numberp a) (if (= 0 a)
                                        0
                                        (list '/ a b)))
                          ((numberp b) (if (= 1 b)
                                         a
                                         (list '/ a b)))
                           (T (list '/ a b))))

     (make-power (a b)
       (cond ((and (qk::extnump a) (qk::extnump b)) (expt a b))
             ((numberp b) (if (= 1 b)
                            a
                            (if (= 0 b)
                              1
                              (list 'expt a b))))
             ((numberp a) (if (= 1 a)
                            1
                            (if (= 0 a)
                              0
                              (list 'expt a b))))
             (T (list 'expt a b))))
     
     (make-exp (a)
       (cond ((numberp a) (if (= a 0)
                            1
                            (list 'exp a)))
             ((log-p a) (log-arg a))
             ((sum-p a) (make-product (make-exp (first-arg a))
                                      (make-exp (second-arg a))))
             (T (list 'exp a))))
     
     (make-log
         (a &optional base)
       (if base
         
         ;; then
         (cond 
          ((qk::extnump a)
           (if (qk::extnump base)
             (log a base)
             (list 'log a base)))
          ((exp-p a)
           (make-product (make-log (exp 1) base) (exponent a))
           )
          ((product-p a) (make-sum (make-log (first-arg a))
                                   (make-log (second-arg a))))
          ((power-p a) 
           (if (equal base (base a))
             (power a)
             (make-product (make-log (base a) base) (power a))))
          (T (list 'log a base)))
         
         ;; else
         (cond
          ((qk::extnump a) (log a))
          ((exp-p a) (exponent a))
          ((product-p a) (make-sum (make-log (first-arg a))
                                   (make-log (second-arg a))))
          ((power-p a) (make-product (make-log (base a)) (power a)))
          (T (list 'log a)))))
     

     (make-cos (x) (if (qk::extnump x) (cos x) (list 'cos x)))
     (make-sin (x) (if (qk::extnump x) (sin x) (list 'sin x)))
     ;(make-tan (x) (if (qk::extnump x) (tan x) (list 'tan x)))
     (make-cosh (x) (if (qk::extnump x) (cosh x) (list 'cosh x)))
     (make-sinh (x) (if (qk::extnump x) (sinh x) (list 'sinh x)))
     ;(make-tanh (x) (if (qk::extnump x) (tanh x) (list 'tanh x)))
     
     (make-lambda (var-list alg-expr) (list 'lambda var-list alg-expr))
     
     (make-fn (var-list alg-expr) (list 'fn var-list alg-expr))
     )
    
    (unless wrt (setf wrt (deriv-wrt function)))
    (simplify
     (cond
      ((or (constant-p function)
           (null wrt))
       0)
      
      ((variable-p function)
       (if (same-variable-p function wrt) 1 0))
      
      ((sum-p function) 
       (let ((nargs (num-args function)))
         (cond
          ((> nargs 2)
           (let ((result NIL)
                 (args (args function)))
             (do* ((i 0 (+ i 1))
                   arg
                   derivative
                   )
                  ((>= i nargs) result)
               (setf arg (elt args i))
               (setf derivative (deriv arg :wrt wrt))
               (when (not (and (numberp derivative) (zerop derivative)))
                 (push derivative result))
               )
             (setf result
                   (simplify (cons '+ (reverse result))))
             ))
          ((= nargs 2)
           (make-sum (deriv (first-arg function) :wrt wrt)
                     (deriv (second-arg function) :wrt wrt)))
          ((= nargs 1)
           (deriv (first-arg function) :wrt wrt))
          ((= nargs 0)
           0
           )
          )
         )
       )
      
      ((difference-p function)
       (let ((nargs (num-args function)))
         (cond
          ((> nargs 2)
           (simplify
            (list '- 
                  (loop for arg in (args function)
                        collect (deriv arg :wrt wrt)))))
          ((= nargs 2)
           (simplify
            (make-difference
             (deriv  (first-arg function) :wrt wrt)
             (deriv  (second-arg function) :wrt wrt))))
          ((= nargs 1)
           (let ((result (deriv (first-arg function) :wrt wrt)))
             (if (constantp result)
               (- result)
               (list '- result))))
          ((= nargs 0)
           0
           )
          )
         )
       )
      
      ((product-p function)
       (let ((nargs (num-args function)))
         (cond
          ((> nargs 2)
           (let ((arg2 (cons '* (rest (args function)))))
             (simplify
              (list '+ 
                    (make-product (deriv (first-arg function) :wrt wrt)
                                  arg2)
                    (make-product (first-arg function)
                                  (deriv arg2 :wrt wrt)))))
           )
          ((= nargs 2)
           (simplify
            (make-sum
             (make-product (deriv (first-arg function) :wrt wrt)
                           (second-arg function))
             (make-product (first-arg function)
                           (deriv (second-arg function) :wrt wrt)))))
          ((= nargs 1)
           (deriv (first-arg function) :wrt wrt))
          ((= nargs 0)
           0
           )
          )
         )
       )
      
      ((quotient-p function)
       (let ((nargs (num-args function)))
         (cond
          ((> nargs 2)
           (let ((arg1 (first-arg function))
                 (arg2 (cons '* (rest (args function)))))
             (simplify
              (make-quotient
               (make-difference
                (make-product arg2 (deriv arg1 :wrt wrt))
                (make-product arg1 (deriv arg2 :wrt wrt)))
               (make-power arg2 2))))
           )
          ((= nargs 2)
           (make-quotient
            (make-difference
             (make-product
              (second-arg function) (deriv (first-arg function) :wrt wrt))
             (make-product
              (first-arg function) (deriv (second-arg function) :wrt wrt)))
            (make-power (second-arg function) 2)))
          ((= nargs 1)
           (deriv (list '/ 1 (first-arg function)) :wrt wrt))
          ((= nargs 0)
           0
           )
          )
         )
       )
      
      ((power-p function)
       (make-product
        (power function)
        (make-product
         (make-power (base function) 
                     (make-difference (power function) 1))
         (deriv (base function) :wrt wrt))))
      
      ((exp-p function) (make-product function (deriv (exponent function) :wrt wrt)))
      
      ((log-p function)
       (make-quotient (deriv (log-arg function) :wrt wrt) 
                      (log-arg function)))
      
      
      ((cos-p function)
       (make-product
        (make-product -1 (make-sin (first-arg function)))
        (deriv (first-arg function) :wrt wrt)))
      
      ((sin-p function)
       (make-product
        (make-cos (first-arg function))
        (deriv (first-arg function) :wrt wrt)))
      
      ((tan-p function)
       (make-product
        (make-power (make-cos (first-arg function)) -2)
        (deriv (first-arg function) :wrt wrt)))
      
      
      ((cosh-p function)
       (make-product
        (make-sinh (first-arg function))
        (deriv (first-arg function) :wrt wrt)))
      
      ((sinh-p function)
       (make-product
        (make-cosh (first-arg function))
        (deriv (first-arg function) :wrt wrt)))
      
      ((tanh-p function)
       (make-product
        (make-power (make-cosh (first-arg function)) -2)
        (deriv (first-arg function) :wrt wrt)))
      
      ((lambda-p function) 
       (make-lambda (lambda-arglist function)
                    (deriv (lambda-contents function) :wrt wrt)))
      
      ((functionp function)
       (list 'function (deriv (second function) :wrt wrt)))
      
      ((fn-list-p function) 
       (make-fn (lambda-arglist function)
                (deriv (lambda-contents function) :wrt wrt)))
      
      (T (quail-error "deriv: can't differentiate the expression ~
                       ~s with respect to ~s" function wrt))
      )
     )
    ))

