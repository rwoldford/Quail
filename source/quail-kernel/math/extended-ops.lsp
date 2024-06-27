;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                              extended-ops.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (C) 1990 Statistical Computing Laboratory, University Of Waterloo
;;;
;;; 
;;;  Authors:
;;;     R.W. Oldford 1989-1994.
;;;     D.G. Anglin 1989, 1990.
;;;     M.E. Lewis 1991.
;;;
;;;--------------------------------------------------------------------------------

(in-package :quail-kernel)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(+infinity infinity -infinity NaN 
          with-zero-over-zero with-plus-inf-over-zero with-minus-inf-over-zero
          with-plus-over-zero with-minus-over-zero with-inf-over-inf)))

;;;--------------------------------------------------------------------------------
;;;  Extended Operators
;;;
;;;  These are *extended real number* operations.  An error is signalled
;;;  if the result cannot be determined using the extended
;;;  real numbers i.e. reals with +infinity, -infinity, and nan (not a number).
;;;
;;;  Note that the predicate operators return nil whenever one of the arguments is
;;;  nan; hence, for example, ext_< treats nan as not strictly less than any other
;;;  extended real number. Consequently, the lisp sort and stable-sort routines, when 
;;;  used with the new < (which calls ext_< if necessary), treat nan as equal to any other
;;;  number. Query: what then happens?
;;;
;;;  Each binary operation takes as an optional parameter the name of a calling method.
;;;  When this is provided, the error routine ext-binop-err for these operations will 
;;;  call missing-method if either num1 or num2 has a class other than t.
;;;
;------------------------------------------------------------------------------


(defconstant  +infinity   '+infinity  "Represents positive infinity.")
(defconstant   infinity   '+infinity  "Represents positive infinity.")
(defconstant  -infinity   '-infinity  "Represents negative infinity.")

(defconstant NaN 'NaN "Not a number.")

(defvar *zero-over-zero*  NaN
  "The value used when zero is divided by zero.~
   Default returns value of NaN. ~
   (elaboration Sometimes useful to replace this value temporarily using ~
   the macro with-zero-over-zero) ~
   (:see-also (with-zero-over-zero :macro))")

(defvar *plus-inf-over-zero* NaN
  "The value used when +infinity is divided by zero.~
   Default returns NaN ~
   (elaboration Sometimes useful to replace this value temporarily using ~
   the macro with-plus-inf-over-zero to avoid the NaN.)")

(defvar *minus-inf-over-zero* NaN
  "The value used when -infinity is divided by zero.~
   Default returns NaN ~
   (elaboration Sometimes useful to replace this value temporarily using ~
   the macro with-minus-inf-over-zero to avoid the NaN.) ~
   (:see-also (with-minus-inf-over-zero :macro))")

(defun *plus-over-zero* (num den)
  "The function called when a positive number is divided by zero.~
   Default returns +infinity. ~
   (elaboration Sometimes useful to replace this function temporarily   using ~
   the macro with-plus-over-zero to avoid the +infinity.) ~
   (:required (:arg num The numerator in the division.) ~
              (:arg den The denominator in the division.))"
  (declare (ignore num den)
           (special +infinity))
  +infinity)

(defun *minus-over-zero* (num den)
  "The function called when a negative number is divided by zero..~
   Default returns -infinity. ~
   (elaboration Sometimes useful to replace this function temporarily  using ~
   the macro with-minus-over-zero to avoid the -infinity.) ~
   (:required (:arg num The numerator in the division.) ~
              (:arg den The denominator in the division.))"
  (declare (ignore num den)
           (special -infinity))
  -infinity)

(defun *inf-over-inf* (num den)
  "The function when an infinite number is divided by another infinite number.~
   Default returns value of NaN. ~
   (elaboration Sometimes useful to replace this function temporarily   using ~
   the macro with-inf-over-inf instead of always getting NaN.) ~
   (:required (:arg num The numerator in the division.) ~
              (:arg den The denominator in the division.))"
  (declare (ignore num den)
           (special NaN))
  NaN)

(defmacro with-zero-over-zero (value &body body)
  "Performs the body using value of the first argument as ~
   the value returned when zero is divided by zero."
  `(let ((temp *zero-over-zero*)
         result)
     (setf *zero-over-zero* ,value)
     (unwind-protect
       (setf result ,@body)
       (setf *zero-over-zero* temp))
     result))

(defmacro with-plus-inf-over-zero (value &body body)
  "Performs the body using value of the first argument as ~
   the value returned when +infinity is divided by zero."
  `(let ((temp *zero-over-zero*)
         result)
     (setf *plus-inf-over-zero* ,value)
     (unwind-protect
       (setf result ,@body)
       (setf *plus-inf-over-zero* temp))
     result))

(defmacro with-minus-inf-over-zero (value &body body)
  "Performs the body using value of the first argument as ~
   the value returned when -infinity is divided by zero."
  `(let ((temp *zero-over-zero*)
         result)
     (setf *minus-inf-over-zero* ,value)
     (unwind-protect
       (setf result ,@body)
       (setf *minus-inf-over-zero* temp))
     result))

(defmacro with-plus-over-zero (fun &body body)
  "Performs the body using the first argument as ~
   the function to be applied to the numerator and denominator ~
   when a positive extended number is divided by zero. ~
   (:required ~
   (:arg fun A function of two arguments, num and den, ~
             which returns the result of dividing num by den.  ~
             It will be called only when num is positive and den ~
             is zero.))"
  `(let ((temp (symbol-function '*plus-over-zero*))
         result)
     (setf (symbol-function '*plus-over-zero*) ,fun)
     (unwind-protect
       (setf result ,@body)
       (setf (symbol-function '*plus-over-zero*) temp))
     result))

(defmacro with-minus-over-zero (fun &body body)
  "Performs the body using the first argument as ~
   the function to be applied to the numerator and denominator ~
   when a negative extended number is divided by zero. ~
   (:required ~
   (:arg fun A function of two arguments, num and den, ~
             which returns the result of dividing num by den.  ~
             It will be called only when num is negative and den ~
             is zero.))"
  `(let ((temp (symbol-function '*minus-over-zero*))
         result)
     (setf (symbol-function '*minus-over-zero*) ,fun)
     (unwind-protect
       (setf result ,@body)
       (setf (symbol-function '*minus-over-zero*) temp))
     result))

(defmacro with-inf-over-inf (fun &body body)
  "Performs the body using the first argument as ~
   the function to be applied to the numerator and denominator ~
   when one infinite number is divided by another. ~
   (:required ~
   (:arg fun A function of two arguments, num and den, ~
             which returns the result of dividing num by den.  ~
             It will be called only when both num and den ~
             are plus or minus infinity.))"
  `(let ((temp (symbol-function '*inf-over-inf*))
         result)
     (setf (symbol-function '*inf-over-inf*) ,fun)
     (unwind-protect
       (setf result ,@body)
       (setf (symbol-function '*inf-over-inf*) temp))
     result))

(defmacro ext_+infp (num)
  `(eq ,num +infinity))

(defmacro ext_-infp (num)
  `(eq ,num -infinity))

(defmacro ext_nanp (num)
  `(eq ,num nan))

(defmacro extp (num)
  `(or (ext_nanp ,num) (numberp ,num) (ext_+infp ,num) (ext_-infp ,num)))

(defmacro extnump (num)
  `(or (numberp ,num) (ext_+infp ,num) (ext_-infp ,num)))

(defun ext-binop-err (ext-op method num1 num2 &optional which)
  (if (and method
           (not (equal (class-name (class-of num1)) t))
           (not (equal (class-name (class-of num2)) t)))
    (missing-method method num1 num2)
    (case which
      (1 (quail-error "~s is an invalid argument to ~s." num1 ext-op))
      (2 (quail-error "~s is an invalid argument to ~s." num2 ext-op))
      (otherwise (quail-error "invalid arguments to ~s: ~s and ~s." ext-op num1 num2)))))

(defun ext_+ (num1 num2 &optional method)
  (declare (special +infinity -infinity NaN)
           (optimize (speed 3)))
  (let (result)    
    (cond ((numberp num1)
           (if (numberp num2)
             ;; then both are numbers, do operation -- most cases end here
             (setf result (cl:+ num1 num2))
             ;; else only num2 is not a number
             (case num2
               (+infinity (setf result +infinity))
               (-infinity (setf result -infinity))
               (nan (setf result nan))
               (otherwise (ext-binop-err 'ext_+ method num1 num2 2)))))
          ((numberp num2)
           ;; then only num1 is not a number
           (case num1
             (+infinity (setf result +infinity))
             (-infinity (setf result -infinity))
             (nan (setf result nan))
             (otherwise (ext-binop-err 'ext_+ method num1 num2 1))))
          (t
           ;; else both num1 and num2 are not numbers
           (case num1          
             (nan
              (case num2
                ((+infinity -infinity nan) (setf result nan))
                (otherwise (ext-binop-err 'ext_+ method num1 num2 2))))          
             (+infinity
              (case num2
                (-infinity (setf result nan))
                (+infinity (setf result +infinity))
                (nan (setf result nan))
                (otherwise (ext-binop-err 'ext_+ method num1 num2 2))))
             (-infinity
              (case num2
                (-infinity (setf result -infinity))
                (+infinity (setf result nan))
                (nan (setf result nan))
                (otherwise (ext-binop-err 'ext_+ method num1 num2 2))))          
             (otherwise (ext-binop-err 'ext_+ method num1 num2 1)))))
    result))

(defun ext_- (num1 num2 &optional method)
  (declare (special +infinity -infinity NaN)
           (optimize (speed 3)))
  (let (result)    
    (cond ((numberp num1)
           (if (numberp num2)
             ;; then both are numbers, do operation -- most cases end here
             (setf result (cl:- num1 num2))
             ;; else only num2 is not a number
             (case num2
               (+infinity (setf result -infinity))
               (-infinity (setf result +infinity))
               (nan (setf result nan))
               (otherwise (ext-binop-err 'ext_- method num1 num2 2)))))
          ((numberp num2)
           ;; then only num1 is not a number
           (case num1
             (+infinity (setf result +infinity))
             (-infinity (setf result -infinity))
             (nan (setf result nan))
             (otherwise (ext-binop-err 'ext_- method num1 num2 1))))
          (t
           ;; else both num1 and num2 are not numbers
           (case num1          
             (nan
              (case num2
                ((+infinity -infinity nan) (setf result nan))
                (otherwise (ext-binop-err 'ext_- method num1 num2 2))))          
             (+infinity
              (case num2
                (-infinity (setf result +infinity))
                (+infinity (setf result nan))
                (nan (setf result nan))
                (otherwise (ext-binop-err 'ext_- method num1 num2 2))))          
             (-infinity
              (case num2
                (-infinity (setf result nan))
                (+infinity (setf result -infinity))
                (nan (setf result nan))
                (otherwise (ext-binop-err 'ext_- method num1 num2 2))))          
             (otherwise (ext-binop-err 'ext_- method num1 num2 1)))))
    result))

(defun ext_* (num1 num2 &optional method)
  (declare (special +infinity -infinity NaN)
           (optimize (speed 3)))
  (let (result)
    (cond ((numberp num1)
           (if (numberp num2)
             ;; then both are numbers, do operation -- most cases end here
             (setf result (cl:* num1 num2))
             ;; else only num2 is not a number
             (case num2
               (+infinity (setf result
                                (cond ((cl:< num1 0) -infinity)
                                      ((cl:= num1 0) nan)
                                      ((cl:> num1 0) +infinity))))
               (-infinity (setf result
                                (cond ((cl:< num1 0) +infinity)
                                      ((cl:= num1 0) nan)
                                      ((cl:> num1 0) -infinity))))
               (nan (setf result nan))
               (otherwise (ext-binop-err 'ext_* method num1 num2 2)))))
          ((numberp num2)        
           ;; then only num1 is not a number
           (case num1
             (+infinity (setf result
                              (cond ((cl:< num2 0) -infinity)
                                    ((cl:= num2 0) nan)
                                    ((cl:> num2 0) +infinity))))
             (-infinity (setf result
                              (cond ((cl:< num2 0) +infinity)
                                    ((cl:= num2 0) nan)
                                    ((cl:> num2 0) -infinity))))
             (nan (setf result nan))
             (otherwise (ext-binop-err 'ext_* method num1 num2 1))))
          (t
           ;; else both num1 and num2 are not numbers
           (case num1          
             (nan
              (case num2
                ((+infinity -infinity nan) (setf result nan))
                (otherwise (ext-binop-err 'ext_* method num1 num2 2))))          
             (+infinity
              (case num2
                (-infinity (setf result -infinity))
                (+infinity (setf result +infinity))
                (nan (setf result nan))
                (otherwise (ext-binop-err 'ext_* method num1 num2 2))))          
             (-infinity
              (case num2
                (-infinity (setf result +infinity))
                (+infinity (setf result -infinity))
                (nan (setf result nan))
                (otherwise (ext-binop-err 'ext_* method num1 num2 2))))          
             (otherwise (ext-binop-err 'ext_* method num1 num2 1)))))
    result))


(defun ext_/ (num1 num2 &optional method)
  (declare (special +infinity -infinity NaN
                    *zero-over-zero*
                    *plus-inf-over-zero*
                    *minus-inf-over-zero*)
           (optimize (speed 3)))
  (let (result)    
    (cond ((numberp num1)
           (if (numberp num2)
             ;; then both are numbers, do operation -- most cases end here
             (if (zerop num2)
               (setf result
                     (cond ((plusp num1) (*plus-over-zero* num1 num2))
                           ((zerop num1) *zero-over-zero*)
                           (t (*minus-over-zero* num1 num2))))
               (setf result (cl:/ num1 num2)))
             ;; else only num2 is not a number
             (case num2
               (+infinity (setf result 0))
               (-infinity (setf result 0))
               (nan (setf result nan))
               (otherwise (ext-binop-err 'ext_/ method num1 num2 2)))))
          ((numberp num2)        
           ;; then only num1 is not a number
           (case num1
             (+infinity (setf result
                              (cond ((cl:< num2 0) -infinity)
                                    ((cl:= num2 0) *plus-inf-over-zero*)
                                    ((cl:> num2 0) +infinity))))
             (-infinity (setf result
                              (cond ((cl:< num2 0) +infinity)
                                    ((cl:= num2 0) *minus-inf-over-zero*)
                                    ((cl:> num2 0) -infinity))))
             (nan (setf result nan))
             (otherwise (ext-binop-err 'ext_/ method num1 num2 1))))
          (t
           ;; else both num1 and num2 are not numbers
           (case num1
             (nan (setf result nan))
             ((+infinity -infinity)
              (setf result
                    (case num2
                      (nan (setf result nan))
                      ((+infinity -infinity) (*inf-over-inf* num1 num2))
                      (otherwise (ext-binop-err 'ext_/ method num1 num2 2)))))
             (otherwise (setf result
                              (ext-binop-err 'ext_/ method num1 num2 1))))))
    result))


(defun ext_min (num1 num2 &optional method)
  (declare (special +infinity -infinity NaN)
           (optimize (speed 3)))
  (let (result)    
    (cond ((and (numberp num1) (numberp num2))
           (setf result (cl:min num1 num2)))          
          ((eq num2 +infinity) (setf result num1))        
          ((eq num1 +infinity) (setf result num2))          
          ((eq num1 -infinity) (setf result num1))          
          ((eq num2 -infinity) (setf result num2))         
          ((or (and (eq num1 nan) (numberp num2))
               (and (numberp num1) (eq num2 nan))
               (and (eq num1 nan) (eq num2 nan)))
           (setf result nan))          
          ((not (numberp num1))
           (ext-binop-err 'ext_min method num1 num2 1))          
          ((not (numberp num2))
           (ext-binop-err 'ext_min method num1 num2 2)))    
    result))

(defun ext_max (num1 num2 &optional method)
  (declare (special +infinity -infinity NaN)
           (optimize (speed 3)))
  (let (result)    
    (cond ((and (numberp num1) (numberp num2))
           (setf result (cl:max num1 num2)))          
          ((eq num1 +infinity) (setf result num1))          
          ((eq num2 +infinity) (setf result num2))          
          ((eq num1 -infinity) (setf result num2))          
          ((eq num2 -infinity) (setf result num1))          
          ((or (and (eq num1 nan) (numberp num2))
               (and (numberp num1) (eq num2 nan))
               (and (eq num1 nan) (eq num2 nan)))
           (setf result nan))          
          ((not (numberp num1))
           (ext-binop-err 'ext_max method num1 num2 1))          
          ((not (numberp num2))
           (ext-binop-err 'ext_max method num1 num2 2)))    
    result))



(defun ext_= (num1 num2 &optional method)
  (declare (special +infinity -infinity NaN)
           (optimize (speed 3)))
  (cond ((and (numberp num1)
              (numberp num2))
         (cl:= num1 num2))
        ((and (eq num1 +infinity)
              (eq num2 +infinity))
         t)          
        ((and (eq num1 -infinity)
              (eq num2 -infinity))
         t)
        ((and (extp num1) (extp num2))
         nil)
        (t (ext-binop-err 'ext_= method num1 num2))))


(defun ext_< (num1 num2 &optional method)
  (declare (special +infinity -infinity NaN)
           (optimize (speed 3)))
  (cond ((and (numberp num1)
              (numberp num2))
         (cl:< num1 num2))
        ((eq num1 +infinity)
         nil)     
        ((eq num2 -infinity)
         nil)     
        ((and (ext_nanp num1) (extp num2))
         nil)
        ((and (extp num1) (ext_nanp num2))
         nil)
        ((eq num1 -infinity)
         t)     
        ((eq num2 +infinity)
         t)
        (t (ext-binop-err 'ext_< method num1 num2))))


(defun ext_> (num1 num2 &optional method)
  (declare (special +infinity -infinity NaN)
           (optimize (speed 3)))
  (cond ((and (numberp num1)
              (numberp num2))
         (cl:> num1 num2))
        ((eq num1 -infinity)
         nil)     
        ((eq num2 +infinity)
         nil)     
        ((and (ext_nanp num1) (extp num2))
         nil)
        ((and (extp num1) (ext_nanp num2))
         nil)
        ((eq num1 +infinity)
         t)     
        ((eq num2 -infinity)
         t)
        (t (ext-binop-err 'ext_> method num1 num2))))


(defun ext_<= (num1 num2 &optional method)
  (declare (special +infinity -infinity NaN)
           (optimize (speed 3)))
  (cond ((and (numberp num1)
              (numberp num2))
         (cl:<= num1 num2))
        ((and (ext_nanp num1) (extp num2))
         nil)
        ((and (extp num1) (ext_nanp num2))
         nil)
        ((eq num1 -infinity)
         t)     
        ((eq num2 +infinity)
         t)
        ((and (extp num1) (extp num2))
         nil)
        (t (ext-binop-err 'ext_<= method num1 num2))))


(defun ext_>= (num1 num2 &optional method)
  (declare (special +infinity -infinity NaN)
           (optimize (speed 3)))
  (cond ((and (numberp num1)
              (numberp num2))
         (cl:>= num1 num2))
        ((and (ext_nanp num1) (extp num2))  
         nil)
        ((and (extp num1) (ext_nanp num2))
         nil)
        ((eq num1 +infinity)
         t)     
        ((eq num2 -infinity)
         t)
        ((and (extp num1) (extp num2))
         nil)
        (t (ext-binop-err 'ext_>= method num1 num2))))

(defvar *most-positive-long-logarithm*
  (log most-positive-long-float))

(defun ext_exp (number)
  (declare (special +infinity -infinity NaN *most-positive-long-logarithm*)
           (optimize (speed 3)))
  (cond ((numberp number)
         (if (> number *most-positive-long-logarithm*)
           +infinity
           (cl:exp number)))
        ((ext_nanp number)  
         NaN)
        ((ext_+infp number)
         +infinity)     
        ((ext_-infp number)
         0)
        (t (quail-error "Not an extended number: ~s" number))))

(defun ext_sqrt (number)
  (declare (special +infinity -infinity NaN)
           (optimize (speed 3)))
  (cond ((numberp number)
         (cl:sqrt number))
        ((ext_nanp number)  
         NaN)
        ((ext_+infp number)
         +infinity)     
        ((ext_-infp number)
         NaN)
        (t (quail-error "Not an extended number: ~s" number))))

(defun ext_isqrt (number)
  (declare (special +infinity -infinity NaN)
           (optimize (speed 3)))
  (cond ((numberp number)
         (cl:isqrt number))
        ((ext_nanp number)  
         NaN)
        ((ext_+infp number)
         +infinity)     
        ((ext_-infp number)
         NaN)
        (t (quail-error "Not an extended number: ~s" number))))

(defun ext_abs (number)
  (declare (special +infinity -infinity NaN)
           (optimize (speed 3)))
  (cond ((numberp number)
         (cl:abs number))
        ((ext_nanp number)  
         NaN)
        ((ext_+infp number)
         +infinity)     
        ((ext_-infp number)
         +infinity)
        (t (quail-error "Not an extended number: ~s" number))))

(defun ext_phase (number)
  (declare (special +infinity -infinity NaN)
           (optimize (speed 3)))
  (cond ((numberp number)
         (cl:phase number))
        ((ext_nanp number)  
         NaN)
        ((ext_+infp number)
         0)     
        ((ext_-infp number)
         pi)
        (t (quail-error "Not an extended number: ~s" number))))


(defun ext_signum (number)
  (declare (special +infinity -infinity NaN)
           (optimize (speed 3)))
  (cond ((numberp number)
         (cl:signum number))
        ((ext_nanp number)  
         NaN)
        ((ext_+infp number)
         1)     
        ((ext_-infp number)
         -1)
        (t (quail-error "Not an extended number: ~s" number))))

(defun ext_sin (radians)
  (declare (special +infinity -infinity NaN)
           (optimize (speed 3)))
  (cond ((numberp radians)
         (cl:sin radians))
        ((ext_nanp radians)  
         NaN)
        ((ext_+infp radians)
         NaN)     
        ((ext_-infp radians)
         NaN)
        (t (quail-error "Not an extended number: ~s" radians))))

(defun ext_cos (radians)
  (declare (special +infinity -infinity NaN)
           (optimize (speed 3)))
  (cond ((numberp radians)
         (cl:cos radians))
        ((ext_nanp radians)  
         NaN)
        ((ext_+infp radians)
         NaN)     
        ((ext_-infp radians)
         NaN)
        (t (quail-error "Not an extended number: ~s" radians))))

(defun ext_tan (radians)
  (declare (special +infinity -infinity NaN)
           (optimize (speed 3)))
  (cond ((numberp radians)
         (cl:tan radians))
        ((ext_nanp radians)  
         NaN)
        ((ext_+infp radians)
         NaN)     
        ((ext_-infp radians)
         NaN)
        (t (quail-error "Not an extended number: ~s" radians))))

(defun ext_cis (radians)
  (declare (special +infinity -infinity NaN)
           (optimize (speed 3)))
  (cond ((numberp radians)
         (cl:cis radians))
        ((extnump radians)  
         NaN)
        (t (quail-error "Sorry can't calculate (cis ~s).  " radians))))

(defun ext_asin (number)
  (declare (special +infinity -infinity NaN)
           (optimize (speed 3)))
  (cond ((numberp number)
         (cl:asin number))
        ((ext_nanp number)  
         NaN)
        ((ext_+infp number)
         NaN)     
        ((ext_-infp number)
         NaN)
        (t (quail-error "Not an extended number: ~s" number))))

(defun ext_acos (number)
  (declare (special +infinity -infinity NaN)
           (optimize (speed 3)))
  (cond ((numberp number)
         (cl:acos number))
        ((ext_nanp number)  
         NaN)
        ((ext_+infp number)
         NaN)     
        ((ext_-infp number)
         NaN)
        (t (quail-error "Not an extended number: ~s" number))))

(defun ext_atan (number &optional (x NIL))
  (declare (special pi +infinity -infinity NaN)
           (optimize (speed 3)))
  (cond ((numberp number)
         (if x
           (cond
            ((numberp x) (cl:atan number x))
            ((ext_nanp x) NaN)
            ((eq x +infinity) (cl:atan 0))
            ((eq x -infinity) (cl:atan 0 (- (signum number))))
            (t (quail-error "Second argument to atan is not an extended number: ~s" x)))
           (cl:atan number)))
         ((or (ext_nanp number) (and x (ext_nanp x))) 
          NaN)
         ((ext_+infp number)
          (if x
            (cond
             ((numberp x)
              (* (signum x) (/ pi 2.0)))
             ((or (ext_nanp x) (eq x +infinity) (eq x -infinity)) NaN)
             (t (quail-error "Second argument to atan is not an extended number: ~s" x)))
            (/ pi 2.0)))    
         ((ext_-infp number)
          (if x
            (cond
             ((numberp x)
              (* -1 (signum x) (/ pi 2.0)))
             ((or (ext_nanp x) (eq x +infinity) (eq x -infinity)) NaN)
             (t (quail-error "Second argument to atan is not an extended number: ~s" x)))
            (/ pi -2.0)))
         (t (quail-error "Not an extended number: ~s" number))))

(defun ext_sinh (number)
  (declare (special +infinity -infinity NaN)
           (optimize (speed 3)))
  (cond ((numberp number)
         (cl:sinh number))
        ((ext_nanp number)  
         NaN)
        ((ext_+infp number)
         +infinity)     
        ((ext_-infp number)
         -infinity)
        (t (quail-error "Not an extended number: ~s" number))))

(defun ext_cosh (number)
  (declare (special +infinity -infinity NaN)
           (optimize (speed 3)))
  (cond ((numberp number)
         (cl:cosh number))
        ((ext_nanp number)  
         NaN)
        ((ext_+infp number)
         +infinity)     
        ((ext_-infp number)
         +infinity)
        (t (quail-error "Not an extended number: ~s" number))))

(defun ext_tanh (number)
  (declare (special +infinity -infinity NaN)
           (optimize (speed 3)))
  (cond ((numberp number)
         (cl:tanh number))
        ((ext_nanp number)  
         NaN)
        ((ext_+infp number)
         1.0)     
        ((ext_-infp number)
         -1.0)
        (t (quail-error "Not an extended number: ~s" number))))

(defun ext_asinh (number)
  (declare (special +infinity -infinity NaN)
           (optimize (speed 3)))
  (cond ((numberp number)
         (cl:asinh number))
        ((ext_nanp number)  
         NaN)
        ((ext_+infp number)
         +infinity)     
        ((ext_-infp number)
         -infinity)
        (t (quail-error "Not an extended number: ~s" number))))

(defun ext_acosh (number)
  (declare (special +infinity -infinity NaN)
           (optimize (speed 3)))
  (cond ((numberp number)
         (cl:acosh number))
        ((ext_nanp number)  
         NaN)
        ((ext_+infp number)
         +infinity)     
        ((ext_-infp number)
         NaN)
        (t (quail-error "Not an extended number: ~s" number))))

(defun ext_atanh (number)
  (declare (special pi +infinity -infinity NaN)
           (optimize (speed 3)))
  (cond ((numberp number)
         (cl:atanh number))
        ((ext_nanp number)  NaN)
        ((or (ext_+infp number) (ext_-infp number))
         (cl:/ (cl:log -1) 2))
        (t (quail-error "Not an extended number: ~s" number))))



(defun ext_float (number &optional other)
  (declare (optimize (speed 3)))
  (cond ((numberp number)
         (if other
           (if (floatp other)
             (cl:float number other)
             (quail-error "Second argument to float is not of type float: ~s" other))
           (cl:float number)))
         ((extnump number) number)
         (t (quail-error "Not an extended number: ~s" number))))



(defun ext_rational (number)
  (declare (optimize (speed 3)))
  (cond ((numberp number)
         (cl:rational number))
        ((extnump number) number)
        (t (quail-error "Not an extended number: ~s" number))))



(defun ext_rationalize (number)
  (declare (optimize (speed 3)))
  (cond ((numberp number)
         (cl:rationalize number))
        ((extnump number) number)
        (t (quail-error "Not an extended number: ~s" number))))



(defun ext_numerator (number)
  (declare (optimize (speed 3)))
  (cond ((numberp number)
         (cl:numerator number))
        ((extnump number) number)
        (t (quail-error "Not an extended number: ~s" number))))



(defun ext_denominator (number)
  (declare (optimize (speed 3)))
  (cond ((numberp number)
         (cl:denominator number))
        ((ext_nanp number)  NaN)
        ((or (ext_+infp number) (ext_-infp number))
         1)
        (t (quail-error "Not an extended number: ~s" number))))



(defun ext_realpart (number)
  (declare (optimize (speed 3)))
  (cond ((numberp number)
         (cl:realpart number))
        ((extnump number) number)
        (t (quail-error "Not an extended number: ~s" number))))

(defun ext_imagpart (number)
  (declare (optimize (speed 3)))
  (cond ((numberp number)
         (cl:imagpart number))
        ((ext_nanp number)  NaN)
        ((or (ext_+infp number) (ext_-infp number))
         0)
        (t (quail-error "Not an extended number: ~s" number))))



(defun ext_floor (number &optional (divisor NIL))
  (declare (special +infinity -infinity NaN)
           (optimize (speed 3)))
  (if (and divisor (complexp divisor))
    (quail-error "The divisor argument to floor ~
                  must not be complex! (number = ~s divisor = ~s)"
                 number divisor))
  (cond ((numberp number)
         (if divisor
           (cond
            ((numberp divisor)
             (if (not (zerop divisor))
               (cl:floor number divisor)
               (if (minusp number)
                 (values -infinity NaN)
                 (values +infinity NaN))))
            ((ext_nanp divisor) (values NaN NaN))
            ((ext_+infp divisor)
             (if (minusp number)
               (values -1 +infinity)
               (values 0 number)))
            ((ext_-infp divisor)
             (if (plusp number)
               (values -1 -infinity)
               (values 0 number)))
            (T
             (quail-error
              "Divisor, ~s, for floor is not an extended number." divisor)))
           (cl:floor number)))
        ((or (ext_nanp number) (and divisor (ext_nanp divisor)))
         (values NaN NaN))
        ((ext_+infp number)
         (if divisor
           (cond
            ((numberp divisor)
             (if (minusp divisor)
               (values -infinity NaN)
               (values +infinity NaN)))
            ((or (ext_-infp divisor) (ext_+infp divisor))
             (values NaN NaN))
            (T
             (quail-error
              "Divisor, ~s, for floor of ~s is not an extended number." divisor number)))
           (values +infinity NaN)))
        ((ext_-infp number)
         (if divisor
           (cond
            ((numberp divisor)
             (if (minusp divisor)
               (values +infinity NaN)
               (values -infinity NaN)))
            ((or (ext_-infp divisor) (ext_+infp divisor))
             (values NaN NaN))
            (T
             (quail-error
              "Divisor, ~s, for floor of ~s is not an extended number." divisor number)))
           (values +infinity NaN)))
        (t (quail-error "Not an extended number: ~s" number))))



(defun ext_ceiling (number &optional (divisor NIL))
  (declare (special +infinity -infinity NaN)
           (optimize (speed 3)))
  (if (and divisor (complexp divisor))
    (quail-error "The divisor argument to ceiling ~
                  must not be complex! (number = ~s divisor = ~s)"
                 number divisor))
  (cond ((numberp number)
         (if divisor
           (cond
            ((numberp divisor)
             (if (not (zerop divisor))
               (cl:ceiling number divisor)
               (if (minusp number)
                 (values -infinity NaN)
                 (values +infinity NaN))))
            ((ext_nanp divisor) (values NaN NaN))
            ((ext_+infp divisor)
             (if (minusp number)
               (values 0 number)
               (values 1 -infinity)))
            ((ext_-infp divisor)
             (if (plusp number)
               (values 0 number)
               (values 1 +infinity)))
            (T
             (quail-error
              "Divisor, ~s, for ceiling is not an extended number." divisor)))
           (cl:ceiling number)))
        ((or (ext_nanp number) (and divisor (ext_nanp divisor)))
         (values NaN NaN))
        ((ext_+infp number)
         (if divisor
           (cond
            ((numberp divisor)
             (if (minusp divisor)
               (values -infinity NaN)
               (values +infinity NaN)))
            ((or (ext_-infp divisor) (ext_+infp divisor))
             (values NaN NaN))
            (T
             (quail-error
              "Divisor, ~s, for ceiling of ~s is not an extended number." divisor number)))
           (values +infinity NaN)))
        ((ext_-infp number)
         (if divisor
           (cond
            ((numberp divisor)
             (if (minusp divisor)
               (values +infinity NaN)
               (values -infinity NaN)))
            ((or (ext_-infp divisor) (ext_+infp divisor))
             (values NaN NaN))
            (T
             (quail-error
              "Divisor, ~s, for ceiling of ~s is not an extended number." divisor number)))
           (values +infinity NaN)))
        (t (quail-error "Not an extended number: ~s" number))))



(defun ext_truncate (number &optional (divisor NIL))
  (declare (special +infinity -infinity NaN)
           (optimize (speed 3)))
  (if (and divisor (complexp divisor))
    (quail-error "The divisor argument to truncate ~
                  must not be complex! (number = ~s divisor = ~s)"
                 number divisor))
  (cond ((numberp number)
         (if divisor
           (cond
            ((numberp divisor)
             (if (not (zerop divisor))
               (cl:truncate number divisor)
               (if (minusp number)
                 (values -infinity NaN)
                 (values +infinity NaN))))
            ((ext_nanp divisor)    (values NaN NaN))
            ((ext_+infp divisor)   (values 0 number))
            ((ext_-infp divisor)   (values 0 number))
            (T
             (quail-error
              "Divisor, ~s, for truncate is not an extended number." divisor)))
           (cl:truncate number)))
        ((or (ext_nanp number) (and divisor (ext_nanp divisor)))
         (values NaN NaN))
        ((or (ext_+infp number) (ext_-infp number))
         (if divisor
           (cond
            ((numberp divisor) (values number NaN))
            ((or (ext_-infp divisor) (ext_+infp divisor))
             (values NaN NaN))
            (T
             (quail-error
              "Divisor, ~s, for truncate of ~s is not an extended number." divisor number)))
           (values number NaN)))
        (t (quail-error "Not an extended number: ~s" number))))



(defun ext_round (number &optional (divisor NIL))
  (declare (special +infinity -infinity NaN)
           (optimize (speed 3)))
  (if (and divisor (complexp divisor))
    (quail-error "The divisor argument to round ~
                  must not be complex! (number = ~s divisor = ~s)"
                 number divisor))
  (cond ((numberp number)
         (if divisor
           (cond
            ((numberp divisor)
             (if (not (zerop divisor))
               (cl:round number divisor)
               (if (minusp number)
                 (values -infinity NaN)
                 (values +infinity NaN))))
            ((ext_nanp divisor)    (values NaN NaN))
            ((ext_+infp divisor)   (values 0 number))
            ((ext_-infp divisor)   (values 0 number))
            (T
             (quail-error
              "Divisor, ~s, for round is not an extended number." divisor)))
           (cl:round number)))
        ((or (ext_nanp number) (and divisor (ext_nanp divisor)))
         (values NaN NaN))
        ((or (ext_+infp number) (ext_-infp number))
         (if divisor
           (cond
            ((numberp divisor) (values number NaN))
            ((or (ext_-infp divisor) (ext_+infp divisor))
             (values NaN NaN))
            (T
             (quail-error
              "Divisor, ~s, for round of ~s is not an extended number." divisor number)))
           (values number NaN)))
        (t (quail-error "Not an extended number: ~s" number))))



(defun ext_ffloor (number &optional (divisor NIL))
  (declare (special +infinity -infinity NaN)
           (optimize (speed 3)))
  (if (and divisor (complexp divisor))
    (quail-error "The divisor argument to ffloor ~
                  must not be complex! (number = ~s divisor = ~s)"
                 number divisor))
  (cond ((numberp number)
         (if divisor
           (cond
            ((numberp divisor)
             (if (not (zerop divisor))
               (cl:ffloor number divisor)
               (if (minusp number)
                 (values -infinity NaN)
                 (values +infinity NaN))))
            ((ext_nanp divisor) (values NaN NaN))
            ((ext_+infp divisor)
             (if (minusp number)
               (values -1.0 +infinity)
               (values 0.0 number)))
            ((ext_-infp divisor)
             (if (plusp number)
               (values -1.0 -infinity)
               (values 0.0 number)))
            (T
             (quail-error
              "Divisor, ~s, for ffloor is not an extended number." divisor)))
           (cl:ffloor number)))
        ((or (ext_nanp number) (and divisor (ext_nanp divisor)))
         (values NaN NaN))
        ((ext_+infp number)
         (if divisor
           (cond
            ((numberp divisor)
             (if (minusp divisor)
               (values -infinity NaN)
               (values +infinity NaN)))
            ((or (ext_-infp divisor) (ext_+infp divisor))
             (values NaN NaN))
            (T
             (quail-error
              "Divisor, ~s, for ffloor of ~s is not an extended number." divisor number)))
           (values +infinity NaN)))
        ((ext_-infp number)
         (if divisor
           (cond
            ((numberp divisor)
             (if (minusp divisor)
               (values +infinity NaN)
               (values -infinity NaN)))
            ((or (ext_-infp divisor) (ext_+infp divisor))
             (values NaN NaN))
            (T
             (quail-error
              "Divisor, ~s, for ffloor of ~s is not an extended number." divisor number)))
           (values +infinity NaN)))
        (t (quail-error "Not an extended number: ~s" number))))



(defun ext_fceiling (number &optional (divisor NIL))
  (declare (special +infinity -infinity NaN)
           (optimize (speed 3)))
  (if (and divisor (complexp divisor))
    (quail-error "The divisor argument to fceiling ~
                  must not be complex! (number = ~s divisor = ~s)"
                 number divisor))
  (cond ((numberp number)
         (if divisor
           (cond
            ((numberp divisor)
             (if (not (zerop divisor))
               (cl:fceiling number divisor)
               (if (minusp number)
                 (values -infinity NaN)
                 (values +infinity NaN))))
            ((ext_nanp divisor) (values NaN NaN))
            ((ext_+infp divisor)
             (if (minusp number)
               (values 0.0 number)
               (values 1.0 -infinity)))
            ((ext_-infp divisor)
             (if (plusp number)
               (values 0.0 number)
               (values 1.0 +infinity)))
            (T
             (quail-error
              "Divisor, ~s, for fceiling is not an extended number." divisor)))
           (cl:fceiling number)))
        ((or (ext_nanp number) (and divisor (ext_nanp divisor)))
         (values NaN NaN))
        ((ext_+infp number)
         (if divisor
           (cond
            ((numberp divisor)
             (if (minusp divisor)
               (values -infinity NaN)
               (values +infinity NaN)))
            ((or (ext_-infp divisor) (ext_+infp divisor))
             (values NaN NaN))
            (T
             (quail-error
              "Divisor, ~s, for fceiling of ~s is not an extended number." divisor number)))
           (values +infinity NaN)))
        ((ext_-infp number)
         (if divisor
           (cond
            ((numberp divisor)
             (if (minusp divisor)
               (values +infinity NaN)
               (values -infinity NaN)))
            ((or (ext_-infp divisor) (ext_+infp divisor))
             (values NaN NaN))
            (T
             (quail-error
              "Divisor, ~s, for fceiling of ~s is not an extended number." divisor number)))
           (values +infinity NaN)))
        (t (quail-error "Not an extended number: ~s" number))))



(defun ext_ftruncate (number &optional (divisor NIL))
  (declare (special +infinity -infinity NaN)
           (optimize (speed 3)))
  (if (and divisor (complexp divisor))
    (quail-error "The divisor argument to ftruncate ~
                  must not be complex! (number = ~s divisor = ~s)"
                 number divisor))
  (cond ((numberp number)
         (if divisor
           (cond
            ((numberp divisor)
             (if (not (zerop divisor))
               (cl:ftruncate number divisor)
               (if (minusp number)
                 (values -infinity NaN)
                 (values +infinity NaN))))
            ((ext_nanp divisor)    (values NaN NaN))
            ((ext_+infp divisor)   (values 0.0 number))
            ((ext_-infp divisor)   (values 0.0 number))
            (T
             (quail-error
              "Divisor, ~s, for ftruncate is not an extended number." divisor)))
           (cl:ftruncate number)))
        ((or (ext_nanp number) (and divisor (ext_nanp divisor)))
         (values NaN NaN))
        ((or (ext_+infp number) (ext_-infp number))
         (if divisor
           (cond
            ((numberp divisor) (values number NaN))
            ((or (ext_-infp divisor) (ext_+infp divisor))
             (values NaN NaN))
            (T
             (quail-error
              "Divisor, ~s, for ftruncate of ~s is not an extended number." divisor number)))
           (values number NaN)))
        (t (quail-error "Not an extended number: ~s" number))))



(defun ext_fround (number &optional (divisor NIL))
  (declare (special NaN)
           (optimize (speed 3)))
  (if (and divisor (complexp divisor))
    (quail-error "The divisor argument to fround ~
                  must not be complex! (number = ~s divisor = ~s)"
                 number divisor))
  (cond ((numberp number)
         (if divisor
           (cond
            ((numberp divisor)
             (if (not (zerop divisor))
               (cl:fround number divisor)
               (if (minusp number)
                 (values -infinity NaN)
                 (values +infinity NaN))))
            ((ext_nanp divisor)    (values NaN NaN))
            ((ext_+infp divisor)   (values 0.0 number))
            ((ext_-infp divisor)   (values 0.0 number))
            (T
             (quail-error
              "Divisor, ~s, for fround is not an extended number." divisor)))
           (cl:fround number)))
        ((or (ext_nanp number) (and divisor (ext_nanp divisor)))
         (values NaN NaN))
        ((or (ext_+infp number) (ext_-infp number))
         (if divisor
           (cond
            ((numberp divisor) (values number NaN))
            ((or (ext_-infp divisor) (ext_+infp divisor))
             (values NaN NaN))
            (T
             (quail-error
              "Divisor, ~s, for fround of ~s is not an extended number." divisor number)))
           (values number NaN)))
        (t (quail-error "Not an extended number: ~s" number))))



(defun ext_complex (realpart &optional (imagpart NIL))
  (declare (special  NaN)
           (optimize (speed 3)))
  (cond ((numberp realpart)
         (if imagpart
           (cond
            ((numberp imagpart) (cl:complex realpart imagpart))
            ((ext_nanp imagpart)    NaN)
            (T
             (quail-error
              "Sorry, cannot handle complex numbers with imaginary part: ~
               ~s." imagpart)))
           (cl:complex realpart)))
        ((or (ext_nanp realpart) (and imagpart (ext_nanp imagpart)))
         NaN)
        ((or (ext_+infp realpart) (ext_-infp realpart))
         (if imagpart
           (if (and (numberp imagpart) (zerop imagpart))
             realpart
             (quail-error
              "Sorry, cannot construct complex numbers with an imaginary part: ~ 
               ~s."
              imagpart))
           realpart))
        (t (quail-error "Arguments are not extended numbers.  ~
                         Real part = ~s.  Imaginary part = ~s."
                        realpart imagpart))))


(defun ext_expt (base-number power-number)
  (declare (special +infinity -infinity NaN)
           (optimize (speed 3)))
  (cond
   ((and (numberp base-number) (numberp power-number))
    (if (and (zerop base-number) (minusp power-number))
      +infinity
      (cl:expt base-number power-number)))
   ((or (ext_nanp base-number) (ext_nanp power-number))
    NaN)
   ((numberp base-number)
    (if (minusp base-number)
      NaN
      (cond
       ((ext_+infp power-number)
        (if(or (= base-number 1) (= base-number 0))
           base-number
           +infinity))
       ((ext_-infp power-number)
        (cond
         ((= base-number 1) base-number)
         ((= base-number 0) +infinity)
         (T 0)))
       (T (quail-error "Sorry, (expt ~s ~s) cannot be calculated."
                       base-number power-number)))))
   ((numberp power-number)
    (cond
     ((ext_+infp base-number)
      (cond
       ((minusp power-number) 0)
       ((zerop power-number) NaN)
       (T +infinity)))
     ((ext_-infp base-number)
      (cond
       ((minusp power-number) 0)
       ((zerop power-number) NaN)
       ((and (integerp power-number)
             (evenp power-number))
        +infinity)
       (T NaN)))
     (T (quail-error "Sorry, (expt ~s ~s) cannot be calculated."
                     base-number power-number))))
   ((ext_+infp base-number)
    (cond
     ((ext_+infp power-number) +infinity)
     ((ext_-infp power-number) 0)
     (T (quail-error "Sorry, (expt ~s ~s) cannot be calculated."
                     base-number power-number))))
   ((ext_-infp base-number)
    (cond
     ((or (ext_+infp power-number) (ext_-infp power-number))
      NaN)
     (T (quail-error "Sorry, (expt ~s ~s) cannot be calculated."
                     base-number power-number))))
   (T (quail-error "Sorry, (expt ~s ~s) cannot be calculated."
                   base-number power-number))))



(defun ext_log (number &optional (base NIL))
  (declare (special +infinity -infinity NaN)
           (optimize (speed 3)))
  (cond
   ((numberp number)
    ;; quick hack by dga 93 03 to allow (log 0 b), b real > 0, --> -infinity
    (if (and (zerop number)
             (or (not base)
                 (and (realp base) (plusp base))))
      -infinity
      ;; end quick hack
      (if (not base)
        (cl:log number)
        (cond
         ((numberp base) (cl:log number base))
         ((extnump base) NaN)
         (T (quail-error "Sorry, (log ~s ~s) cannot be calculated."
                         number base))))))
   ((or (ext_nanp number) (ext_nanp base))
    NaN)
   ((ext_+infp number)
    (cond
     ((ext_+infp base) NaN)
     ((ext_-infp base) NaN)
     ((numberp base)
      (cond
       ((plusp base) +infinity)
       ((zerop base) NaN)
       ((minusp base) NaN)
       (T (quail-error "Sorry, (log ~s ~s) cannot be calculated."
                       number base))))
     ((not base) +infinity)
     (T (quail-error "Sorry, (log ~s ~s) cannot be calculated."
                     number base))))
   ((ext_-infp number)
    (if (or (ext_+infp base) (ext_-infp base) (numberp base))
      NaN
      (quail-error "Sorry, (log ~s ~s) cannot be calculated."
                   number base)))
   (T (quail-error "Sorry, (log ~s ~s) cannot be calculated."
                   number base))))
   



(defun ext_rem (number divisor)
  (declare (optimize (speed 3)))
  (cond ((and (numberp number)
              (numberp divisor))
         (cl:rem number divisor))
        ((and (extnump number)
              (extnump divisor))
         (second (multiple-value-list (ext_truncate number divisor))))
        (t
         (quail-error
          "Sorry, (rem ~s ~s) cannot be calculated."
          number divisor))))
   



(defun ext_mod (number divisor)
  (declare (optimize (speed 3)))
  (cond ((and (numberp number)
              (numberp divisor))
         (cl:mod number divisor))
        ((and (extnump number)
              (extnump divisor))
         (second (multiple-value-list (ext_floor number divisor))))
        (t
         (quail-error
          "Sorry, (mod ~s ~s) cannot be calculated."
          number divisor))))
   



(defun ext_gcd (&rest integers)
  (declare (special +infinity -infinity NaN)
           (optimize (speed 3)))
  (cond ((every #'numberp integers)
         (apply #'cl:gcd integers))
        ((every #'(lambda (x) (or (ext_NaNp x)
                                  (extnump x)))
                integers) NaN)
        (t
         (quail-error
          "Sorry, (gcd ~{ ~s~}) cannot be calculated."
          integers))))

(defun ext_lcm (&rest integers)
  (declare (special +infinity -infinity NaN)
           (optimize (speed 3)))
  (cond ((every #'numberp integers)
         (apply #'cl:lcm integers))
        ((every #'(lambda (x) (or (ext_NaNp x)
                                  (extnump x)))
                integers) NaN)
        (t
         (quail-error
          "Sorry, (lcm ~{ ~s~}) cannot be calculated."
          integers))))

(defun ext_conjugate (number)
  (declare (special +infinity -infinity NaN)
           (optimize (speed 3)))
  (cond ((numberp number) (cl:conjugate number))
        ((extnump number) number)
        (t
         (quail-error
          "Sorry, (conjugate ~s) cannot be calculated."
          number))))
