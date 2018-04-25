;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               extended-arithmetic                              
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (C) 1994 Statistical Computing Laboratory, University Of Waterloo
;;;
;;; 
;;;  Authors:
;;;     R.W. Oldford 1994.
;;;
;;;  In Quail we have chosen to introduce extended arithmetic a la IEEE
;;;  and so have +infinity, -infinity and NaN (not a number).
;;;
;;;  There are two major points which the examples illustrate:
;;;  
;;;  1.  This is done in software, so we pay some penalty for the facility.
;;;      To allow the user to avoid this penalty, all common lisp numerical
;;;      functions (+ - / * log exp expt ... ) have been shadowed.
;;;      This means that they are still available to the user in Quail by using
;;;      the package prefix CL:   as in (CL:+ 2 3) to add two numbers.
;;;
;;;      We have also provide a macro "with-CL-functions" that does the substitution
;;;      for you and so leaves you with more readable code.
;;;
;;;  2.  The IEEE standard need not be followed.  Sometimes it is convenient
;;;      for example to return something other than NaN for a zero over zero
;;;      divide.
;;;      There are some macros which allow you to return what you like.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Try these


(+ 2 3 4 5 6)
(/ 2 0)
(/ 0 0)

(+ 2 NaN)
(+ 3 infinity)
(/ infinity -infinity)
(/ 3 infinity)
(/ 3 -infinity)
(/ -3 0)
(+ infinity 3 infinity)
(- infinity infinity)
(+ -infinity infinity)
(expt infinity 0)
(log 0)

(>= infinity 3)
(= infinity +infinity)
(< NaN 0)
(> NaN 0)
(= NaN 0)
(> NaN NaN)
(= NaN NaN)
;;; Because NaN is special and may result from +infinity/-infinity or
;;; 0/0 or -0/0 , etc. it is dangerous to say that two NaNs are =
(eq NaN NaN)
;;; returns T though.
(<= 0 -infinity)

;;; etc. 
;;;
;;; Compare to the common lisp versions
#|
(CL:+ 2 3 4 5 6)
(CL:/ 2 0)
(CL:/ 0 0) 
|#
;;; An error should result here:  (CL:+ 2 NaN)
;;; An error should result here:  (CL:+ 3 infinity)
;;; An error should result here:  (CL:log 0)

;;; 
;;;
;;;  The price?
;;;

(time (loop for i from 1 to 1000 do (+ 2 3 4)))
(time (loop for i from 1 to 1000 do (CL:+ 2 3 4)))

;;; Part of the reason it's so expensive is that the numerical functions
;;; have been extended to operate on arrays and other things.
;;;

(<- x (array '(1 2 3 4 5 6 7 8 9 0) :dimensions '(5 2)))
(<- y (array (seq 31 40) :dimensions '(5 2)))
(<- z '(10 20 30 40 50))

(+ x y)
(log x)
(* x y)
(+ x z)
(sqrt (.* (tp x) y))

;;; So if you know you've got numbers make the most use of it!
;;;
;;; For example

(defun foo (x y)
  "Stupid function that doesn't know what arguments it will get."
  (- (* x x) (* y y)))

(foo 2 3)
(foo infinity 0)

(defun bar (x y)
  "Function that assumes its  arguments are numbers."
  (with-CL-functions (- *) 

    ;; this replaces the operators
    ;; - * with CL:- and CL:+
    ;; but leaves readable source.
    ;; The list of operators can be as long as you like.

    (- (* x x) (* y y))))

(bar 2 3)
;;; An error should result here:  (bar infinity 0)


;;; The improvement can be substantial

(time (loop for i from 1 to 1000 do (foo 2 3)))
(time (loop for i from 1 to 1000 do (bar 2 3))) 

;;;
;;; This sometimes comes up in generic functions
;;;

(defgeneric baz (x y)
  (:documentation "A generic function that calculates the point x y ~
                   on a saddle."))

;;;
;;; The completely unrestricted case
;;;

(defmethod baz (x y)
  (- (* x x) (* y y)))

;;;
;;; Both numbers
;;;

(defmethod baz ((x number)  (y number))
  (with-CL-functions (- *) 
    (- (* x x) (* y y))))

;;;
;;; Just x is a number
;;;

(defmethod baz ((x number) y)
  (-
   (with-CL-functions (*)  (* x x))
   (* y y)))

(baz 2 3)
(baz (complex 1 1) 3/4)

(<- x (seq 1 3 .5))
(<- y (random-gaussian :n 5))

(baz x y)
(baz 7 x)
(baz x (complex 1 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;;  Extended arith without IEEE
;;;

(defun ratio-of-funs (fun1 fun2 &key (start 1) (end 5) (step 1))
  ;; The following string is just extended documentation for 
  ;; the help function.
  "Returns a list of evaluations of the ratio of fun1 to fun2. ~
   (:required ~
     (:arg fun1 A function of one argument) ~
     (:arg fun2 A function of one argument) ) ~
   (:key ~
     (:arg start 1 Where to begin evaluating the functions.) ~
     (:arg end 5   The point to end the arguments.) ~
     (:arg step 1 The step size for the iterations.)) ~
   (:returns A list of the ratio of the evaluated functions.)"

  (loop for x from start to end by step
        collect
        (/ (funcall fun1 x)
           (funcall fun2 x))))

;;;
;;;  Now suppose we wanted to look at sin(x)/x 
;;;
;;;

(ratio-of-funs #'sin #'identity)
(ratio-of-funs #'sin #'identity :start -5)

;;; The second expression would result in a NaN in the middle
;;; because sin(0)/0 is 0/0 or NaN.
;;; but l'hopital's rule applies and says that the limit as 
;;; x -> 0 of sin(x)/x is just cos(x)/1 or 1
;;; so we could program this up as follows
;;;


(with-zero-over-zero 0.0 
      (ratio-of-funs #'sin #'identity :start -5))

;;;
;;;  Of course one way to do this ratio problem in general
;;;  is as follows:

(defun new-ratio-of-funs (fun1 fun2 &key (start 1) (end 5) (step 1))
  "Returns a list of evaluations of the ratio of fun1 to fun2. ~
   (:required ~
     (:arg fun1 A function of one argument) ~
     (:arg fun2 A function of one argument) ) ~
   (:key ~
     (:arg start 1 Where to begin evaluating the functions.) ~
     (:arg end 5   The point to end the arguments.) ~
     (:arg step 1 The step size for the iterations.)) ~
   (:returns A list of the ratio of the evaluated functions.)"

  (loop for x from start to end by step
        collect
        (let ((num (funcall fun1 x))
              (den (funcall fun2 x)))
          (if (= num den 0)
            (first
             ;; because new-ratio-of-funs always returns a list
             (new-ratio-of-funs (deriv fun1)
                                (deriv fun2)
                                :start x :end x))
            (/ num den)))))

(new-ratio-of-funs #'sin #'identity :start -5)

;;; and of course you could do better than that and actually implement
;;; L'Hopital's rule.
;;;  And if the funcalls were replaced by fn-calls and the initial fun1
;;;  and fun2 were defined using fn instead of defun as in
;;; (<- f1 (fn my-sin (x) (sin x)))
;;; (inspect f1)
;;; (<- f2 (fn just-x (x) x))
;;; Then the source would be available to the deriv function. 
;;; And we would get the best values yet.
;;; (<- f1-prime (deriv f1)) 

(defun newest-ratio-of-funs (fun1 fun2 &key (start 1) (end 5) (step 1))
  "Returns a list of evaluations of the ratio of fun1 to fun2. ~
   (:required ~
   (:arg fun1 A function of one argument) ~
   (:arg fun2 A function of one argument) ) ~
   (:key ~
   (:arg start 1 Where to begin evaluating the functions.) ~
   (:arg end 5   The point to end the arguments.) ~
   (:arg step 1 The step size for the iterations.)) ~
   (:returns A list of the ratio of the evaluated functions.)"
  
  (loop for x from start to end by step
        collect
        (let ((num (fn-call fun1 x))
              (den (fn-call fun2 x)))
          (if (= num den 0)
            (first
             ;; because new-ratio-of-funs always returns a list
             (newest-ratio-of-funs
              (deriv fun1)
              (deriv fun2)
              :start x :end x))
            (/ num den)))))

(<- f1 (fn my-sin (x) (sin x)))
(<- f2 (fn just-x (x) x))
(newest-ratio-of-funs f1 f2 :start -5)

;;; 


;;; Similarly there are macros
;;;
;;;  (with-plus-inf-over-zero value body)
;;;  (with-minus-inf-over-zero value body)
;;;

(with-plus-inf-over-zero 9999999 (/ +infinity 0))
(with-minus-inf-over-zero 9999999 (/ -infinity 0))

;;;  And because there is more than one possibility
;;;
;;;  (with-inf-over-inf fun body)
;;;  (with-plus-over-zero fun body)
;;;  (with-minus-over-zero fun body)
;;; 
;;;  Where each of the functions "fun" are functions of the numerator
;;;  and the denominator in the division
;;;
;;;  Here are some examples:
;;;

(defun my-fun (n d) (* (signum n) (signum d)))
(defun my-fun-2 (n d) 1.0)   

#| or to avoid compiler warnings
(defun my-fun-2 (n d)
  (declare (ignore n d))
  1.0)
|#

(defun my-fun-3 (n d) (signum n))

#| or to avoid compiler warnings
(defun my-fun-2 (n d)
  (declare (ignore d))
  (signum n))
|#

(with-inf-over-inf #'my-fun (/ +infinity -infinity))
(with-inf-over-inf #'my-fun (/ -infinity -infinity))
(with-plus-over-zero #'my-fun-2 (/ 3 0))
(with-minus-over-zero #'my-fun-3 (/ -3 0))








