;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               new-math-old.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (C) 1990 Statistical Computing Laboratory, University Of Waterloo
;;;
;;; 
;;;  Authors:
;;;     Greg Anglin 1989, 1990.
;;;     M.E. Lewis 1991.
;;;     R.W. Oldford 1989, 1990, 1994.
;;;
;;;--------------------------------------------------------------------------------

(in-package :new-math)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(*logical-numerical-ops* *numerical-type-coercion-functions*
          *extended-simple-functions* *functions-with-methods*
          test-elements)))

(defvar *functions-with-methods*
  
  '(+ - * / < <= = > >= min max)
  
  "The function +, for example, works through a generic ~
   binary function plus-object, whose behaviour may be tailored ~
   to the type of arguments it is passed.")

(defvar *logical-numerical-ops*
  
  '(logior logxor logand logeqv
    lognand lognor logandc1 logandc2 logorc1 logorc2
    lognot logtest logbitp ash logcount integer-length
    boole)
  
  "These functions act through map-element.")

(defvar *numerical-type-coercion-functions*
  
  '(float rational rationalize complex)
  
  "These functions act through map-element.")

(defvar *extended-simple-functions*
  
  ;; unary functions
  
  '(exp sqrt isqrt abs phase signum sin cos tan cis asin acos atan
    sinh cosh tanh asinh acosh atanh
    numerator denominator 
    realpart imagpart
    
    ;; unary or binary functions
    
    floor ceiling truncate round
    ffloor fceiling  ftruncate fround 
    
    ;; n-ary functions
    
    expt log rem mod
    gcd lcm conjugate
    )
  
  "These functions act through map-element and use extended real numbers.")

(defvar *redefined-functions*
  
  (append *functions-with-methods*
          *extended-simple-functions*
          *logical-numerical-ops*
          *numerical-type-coercion-functions*)
  
  "Common Lisp functions given added meaning in the Quail system.")

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(with-cl-functions with-all-cl-functions 
           with-cl-functions! with-all-cl-functions!)))

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(plus-object
          minus-object
          times-object 
          divides-object)))

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(min-object 
          max-object 
          equals-object 
          less-than-object
          greater-than-object 
          less-than-equals-object
          greater-than-equals-object)))

;;;--------------------------------------------------------------------------------------
;;; using old math
;;;

(defmacro make-name (&rest args)
  
  "Creates symbols; e.g., (make-name \"foo\" 12) creates 'FOO12."
  
  `(intern (string-upcase (format nil "~{~a~}" (list ,@args)))))

(defmacro with-cl-functions (fns . body)
  
  "Restores the original meaning of the functions listed, ~
   but only at the top level, not in a called function. ~
   Then evaluates body, returning the value of the last form."
  
  (let* ((a-list 
          (mapcar #'(lambda (f)
                      (cons f (find-symbol (string f) 
                                           (string-upcase "cl"))))
                  fns))
         (new-body (sublis a-list body)))
    `(progn ,@new-body)))

(defmacro with-cl-functions! (fns . body)
  
  "Restores the original meaning of the functions listed, ~
   whether at the top level, or in a called function. ~
   Then evaluates body, returning the value of the last form."
  
  (loop for fn in fns
        for old-name = (gensym)
        collect `(,(make-name old-name fn) (symbol-function ',fn))
        into first-list
        collect `(setf (symbol-function ',fn)
                       (symbol-function (find-symbol (string ',fn) 
                                                     (string-upcase "cl"))))
        into second-list
        collect `(setf (symbol-function ',fn) ,(make-name old-name fn))
        into third-list
        finally 
        (return
         `(unwind-protect
            (progn
              ,@first-list
              ,@second-list
              (setf result (progn ,@body))
              )
            (progn
              ,@third-list
              result)))))

(defmacro with-all-cl-functions (&body body)
  
  "Restores the original meaning of all *redefined-functions*, ~
   but only at the top level, not in a called function. ~
   Then evaluates body, returning the value of the last form."
  
  `(with-cl-functions ,*redefined-functions* ,@body))

(defmacro with-all-cl-functions! (&body body)
  
  "Restores the original meaning of all *redefined-functions*, ~
   whether at the top level, or in a called function. ~
   Then evaluates body, returning the value of the last form."
  
  `(with-cl-functions! ,*redefined-functions* ,@body))

;;;--------------------------------------------------------------------------------------
;  Plus
;

(defun + (&rest args)
  "Adds together the arguments in left to right order ~
   by applying plus-object pairwise. ~
   (:see-also (plus-object :generic-function)(cl:+ :function))~
   "
  (case (length args)
    (0 0)
    (1 (plus-object :identity (first args)))
    (2 (plus-object (first args) (second args)))
    (t (apply #'+ (plus-object (first args) (second args)) (rest (rest args))))))

(defgeneric plus-object (x1 x2)
  (:documentation "The binary addition operator.  It is specialized ~
                   to handle different classes of arguments properly.")
  )

(defmethod plus-object ((x1 t) (x2 t))
  (missing-method 'plus-object x1 x2))

(defmethod plus-object ((x1 number) (x2 number))
  (declare (inline cl:+))
  (cl:+ x1 x2))

(defmethod-multi plus-object ((x1 (number symbol)) 
                              (x2 symbol))
  (ext_+ x1 x2 'plus-object))

(defmethod plus-object ((x1 symbol) (x2 number))
  (ext_+ x1 x2 'plus-object))

(defmethod plus-object ((x1 (eql :identity)) (x2 t))
  x2)

(defmethod plus-object ((x1 (eql :identity)) (x2 symbol))
  (ext_+ 0 x2 'plus-object))

(defmethod-multi plus-object ((a (sequence array dimensioned-ref-object))
                              (b (symbol number sequence array dimensioned-ref-object)))
  (map-element #'plus-object nil a b))

(defmethod-multi plus-object ((a ((eql :identity) symbol number)) 
                              (b (sequence array dimensioned-ref-object)))
  (map-element #'plus-object nil a b))

;;;--------------------------------------------------------------------------------
;  Minus
;

(defun - (arg1 &rest args)
  "Takes successive differences of the arguments in left to right order ~
   by applying minus-object pairwise.  So (- a b c) is (- (- a b) c).  ~
   (:see-also (minus-object :generic-function)(cl:- :function))~
   "
  (case (length args)
    (0 (minus-object :identity arg1))
    (1 (minus-object arg1 (first args)))
    (t (apply #'- (minus-object arg1 (first args)) (rest args)))))

(defgeneric minus-object (x1 x2)
  (:documentation "The binary difference operator.  It is specialized ~
                   to handle different classes of arguments properly."))

(defmethod minus-object ((x1 t) (x2 t))
  (missing-method 'minus-object x1 x2))

(defmethod minus-object ((x1 number) (x2 number))
  (declare (inline cl:-))
  (cl:- x1 x2))

(defmethod-multi minus-object ((x1 (number symbol))
                               (x2 symbol))
  (ext_- x1 x2 'minus-object))

(defmethod minus-object ((x1 symbol) (x2 number))
  (ext_- x1 x2 'minus-object))

(defmethod minus-object ((x1 (eql :identity)) (x2 number))
  (declare (inline cl:-))
  (cl:- x2))

(defmethod minus-object ((x1 (eql :identity)) (x2 symbol))
  (ext_- 0 x2 'minus-object))

(defmethod-multi minus-object ((a (sequence array dimensioned-ref-object))
                               (b (symbol number sequence array dimensioned-ref-object)))
  (map-element #'minus-object nil a b))

(defmethod-multi minus-object ((a ((eql :identity) symbol number)) 
                               (b (sequence array dimensioned-ref-object)))
  (map-element #'minus-object nil a b))

;;;-------------------------------------------------------------------------------
;  Times
;

(defun * (&rest args)
  "Multiplies together the arguments in left to right order ~
   by applying times-object pairwise. ~
   (:see-also (times-object :generic-function)(cl:* :function))~
   "
  (case (length args)
    (0 1)
    (1 (times-object :identity (first args)))
    (2 (times-object (first args) (second args)))
    (t (apply #'* (times-object (first args) (second args)) (rest (rest args))))))

(defgeneric times-object (x1 x2)
  (:documentation "The binary multiplication operator.  It is specialized ~
                   to handle different classes of arguments properly."))

(defmethod times-object ((x1 t) (x2 t))
  (missing-method 'times-object x1 x2))

(defmethod times-object ((x1 number) (x2 number))
  (declare (inline cl:*))
  (cl:* x1 x2))

(defmethod-multi times-object ((x1 (number symbol))
                               (x2 symbol))
  (ext_* x1 x2 'times-object))

(defmethod times-object ((x1 symbol) (x2 number))
  (ext_* x1 x2 'times-object))

(defmethod times-object ((x1 (eql :identity)) (x2 number))
  x2)

(defmethod times-object ((x1 (eql :identity)) (x2 symbol))
  (ext_* 1 x2 'times-object))

(defmethod-multi times-object ((a (sequence array dimensioned-ref-object))
                               (b (symbol number sequence array dimensioned-ref-object)))
  (map-element #'times-object nil a b))

(defmethod-multi times-object ((a ((eql :identity) symbol number)) 
                               (b (sequence array dimensioned-ref-object)))
  (map-element #'times-object nil a b))

;;;-------------------------------------------------------------------------------
;  Divide
;

(defun / (arg1 &rest args)
  "The arithmetic division operator. It divides the arguments one by the next ~
   in left to right order by applying divides-object pairwise.  So ~
   (/ a b c) = (/ (/ a b) c).  ~
   (:see-also (divides-object :generic-function)(cl:/ :function))~
   "
  (case (length args)
    (0 (divides-object :identity arg1))
    (1 (divides-object arg1 (first args)))
    (t (apply #'/ (divides-object arg1 (first args)) (rest args)))))

(defgeneric divides-object (x1 x2)
  (:documentation "The binary division operator.  It is specialized ~
                   to handle different classes of arguments properly."))

(defmethod divides-object ((x1 t) (x2 t))
  (missing-method 'divides-object x1 x2))

(defmethod divides-object ((x1 number) (x2 number))
  (declare (inline cl:/))
  (cl:/ x1 x2))

(defmethod-multi divides-object ((x1 number) (x2 ((eql 0) (eql 0.0))))
  (ext_/ x1 x2))

(defmethod-multi divides-object ((x1 (number symbol))
                                 (x2 symbol))
  (ext_/ x1 x2 'divides-object))

(defmethod divides-object ((x1 symbol) (x2 number))
  (ext_/ x1 x2 'divides-object))

(defmethod divides-object ((x1 (eql :identity)) (x2 number))
  (declare (inline cl:/))
  (cl:/ 1 x2))

(defmethod-multi divides-object ((x1 (eql :identity)) (x2 ((eql 0) (eql 0.0))))
  (ext_/ 1 x2))

(defmethod divides-object ((x1 (eql :identity)) (x2 symbol))
  (ext_/ 1 x2 'divides-object))

(defmethod-multi divides-object ((a (sequence array dimensioned-ref-object))
                                 (b (symbol number sequence array dimensioned-ref-object)))
  (map-element #'divides-object nil a b))

(defmethod-multi divides-object ((a ((eql :identity) symbol number)) 
                                 (b (sequence array dimensioned-ref-object)))
  (map-element #'divides-object nil a b))

;;---------------------------------------------------------------------------
;;; Min
;;;

(defun min (arg1 &rest args)
  "Finds and returns the minimum of the arguments. ~
   If one or more arguments contain many elements, then the element-wise ~
   minimum is determined and returned.  ~
   If only the required argument is given but it contains more than one ~
   element, then the minimum element is returned. ~
   (:see-also (min-object :generic-function) ~
   (cl:min :function)) ~
   "
  (case (length args)
    (0 (min-object :identity arg1))
    (1 (min-object arg1 (first args)))
    (t (apply #'min (min-object arg1 (first args)) (rest args)))))

(defgeneric min-object (x1 x2)
  (:documentation
   "The binary minimum function.  Generally returns the minimum of the ~
    two arguments or their element-wise minimum"))

(defmethod min-object ((x1 number) (x2 number))
  (cl:min x1 x2))

(defmethod min-object ((x1 number) (x2 symbol))
  (ext_min x1 x2 'min-object))

(defmethod min-object ((x1 symbol) (x2 number))
  (ext_min x1 x2 'min-object))

(defmethod min-object ((x1 symbol) (x2 symbol))
  (ext_min x1 x2 'min-object))

(defmethod min-object ((x1 (eql :identity)) (x2 number))
  x2)

(defmethod min-object ((x1 (eql :identity)) (x2 symbol))
  (ext_min +infinity x2 'min-object))


(defmethod min-object ((x1 t) (x2 t))
  (missing-method 'min-object x1 x2))

(defmethod-multi min-object ((x1 (eql :identity))
                             (x2 (dimensioned-ref-object array sequence)))
  (eref (reduce-slices #'min x2)))

(defmethod-multi min-object ((x1 (symbol number)) 
                             (x2 (sequence array dimensioned-ref-object)))
  (map-element #'min nil x1 x2))

(defmethod-multi min-object ((x1 (dimensioned-ref-object array sequence))
                             (x2 (symbol number dimensioned-ref-object array sequence)))
  (map-element #'min nil x1 x2))

;;;------------------------------------------------------------------------------
;;; Max
;;;

(defun max (arg1 &rest args)
  "Finds and returns the maximum of the arguments. ~
   If one or more arguments contain many elements, then the element-wise ~
   maximum is determined and returned.  ~
   If only the required argument is given but it contains more than one ~
   element, then the maximum element is returned. ~
   (:see-also (max-object :generic-function) ~
   (cl:max :function)) ~
   "
  (case (length args)
    (0 (max-object :identity arg1))
    (1 (max-object arg1 (first args)))
    (t (apply #'max (max-object arg1 (first args)) (rest args)))))

(defgeneric max-object (x1 x2)
  (:documentation
   "The binary maximum function.  Generally returns the maximum of the ~
    two arguments or their element-wise maximum."))

(defmethod max-object ((x1 number) (x2 number))
  (cl:max x1 x2))

(defmethod max-object ((x1 number) (x2 symbol))
  (ext_max x1 x2 'max-object))

(defmethod max-object ((x1 symbol) (x2 number))
  (ext_max x1 x2 'max-object))

(defmethod max-object ((x1 symbol) (x2 symbol))
  (ext_max x1 x2 'max-object))

(defmethod max-object ((x1 (eql :identity)) (x2 number))
  x2)

(defmethod max-object ((x1 (eql :identity)) (x2 symbol))
  (ext_max -infinity x2 'max-object))

(defmethod max-object ((x1 t) (x2 t))
  (missing-method 'max-object x1 x2))

(defmethod-multi max-object ((x1 (eql :identity))
                             (x2 (dimensioned-ref-object array sequence)))
  (eref (reduce-slices #'max x2)))

(defmethod-multi max-object ((x1 (symbol number)) 
                             (x2 (sequence array dimensioned-ref-object)))
  (map-element #'max nil x1 x2))

(defmethod-multi max-object ((x1 (dimensioned-ref-object array sequence))
                             (x2 (symbol number dimensioned-ref-object array sequence)))
  (map-element #'max nil x1 x2))

;;;
;;;  The following is useful for the Boolean functions.
;;;

(defun object-to-be-mapped-p (x)
  "Returns T if it's to be mapped, NIL, otherwise."
  (and x
       (some #'(lambda (y) (typep x y))
             '(sequence array dimensioned-ref-object))))

;;;----------------------------------------------------------------------------
;;; Equals
;;;

(defun = (arg1 &rest args)
  "Returns T if all arguments are numerically equal to one another.  ~
   (:required ~
   (:arg arg1 Argument to be compared to the remaining args.) ~
   ) ~
   (:rest ~
   (:arg args NIL The remaining things to be compared to arg1. ~
   If NIL, then equals-object is called ~
   with arguments :identity and arg1.) ~
   )~
   (:returns If all arguments are numerically equal, then T is returned, ~
   otherwise NIL. ~
   If all arguments are any one of sequence, array, or dimensioned-ref-object, ~
   then the comparison is element wise via map-element and two values are ~
   returned.  The first is as before, the second is an object of dimension ~
   equal to that of the argument having the most elements and has elements ~
   T or NIL corresponding to the result of the element-wise = comparison.) ~
   (:see-also (equals-object :generic-function) (cl:= :function)) ~
   (:examples (:files eg:Arrays;num-preds.lisp~
   ))"
  (case (length args)
    (0 (equals-object :identity arg1))
    (1 (equals-object arg1 (first args)))
    (t 
     (let ((all-args (cons arg1 args)))
       (if (every #'object-to-be-mapped-p all-args)
         (let ((result1 
                (apply #'cl:=
                       (mapcar #'number-of-elements all-args)))
               result2)
           (flet ((doit (&rest args)
                    (let ((temp-result (apply #'= args)))
                      (unless temp-result (setf result1 NIL))
                      temp-result)))
             (setf result2 (apply #'map-element #'doit nil all-args)))
           (values result1 result2))
         (if (equals-object arg1 (first args))
           (apply #'= (first args) (rest args))
           nil))))
    )
  )

(defgeneric equals-object (x1 x2)
  (:documentation "The binary predicate function used to test ~
                   whether the two objects are numerically = to one another.  ~
                   Some methods may return multiple ~
                   values with more detailed information in the other values. ~
                   (:see-also (= :function)) ~
                   (:examples (:files eg:Arrays;num-preds.lisp~
                   ))
                   "
                  ))

(defmethod equals-object ((x1 T) (x2 T))
  (equal x1 x2))

(defmethod equals-object ((x1 number) (x2 number))
  (cl:= x1 x2))

(defmethod equals-object ((x1 number) (x2 matrix))
  (cond
   ((apply #'cl:= 1 (matrix-dimensions-of x2))
    (ext_= x1 (eref x2 0 0)))
   (T (call-next-method))))

(defmethod equals-object ((x1 matrix) (x2 number))
  (cond
   ((apply #'cl:= 1 (matrix-dimensions-of x1))
    (ext_= x2 (eref x1 0 0)))
   (T (call-next-method))))

(defmethod equals-object ((x1 number) (x2 symbol))
  (ext_= x1 x2 'equals-object))

(defmethod equals-object ((x1 symbol) (x2 number))
  (ext_= x1 x2 'equals-object))

(defmethod equals-object ((x1 symbol) (x2 symbol))
  (ext_= x1 x2 'equals-object))

(defmethod equals-object ((x1 (eql :identity)) (x2 number))
  (cl:= x2))

(defmethod equals-object ((x1 (eql :identity)) (x2 symbol))
  (ext_= x2 x2 'equals-object))

(defmethod equals-object ((x1 t) (x2 t))
  (missing-method 'equals-object x1 x2))

(defmethod-multi equals-object ((x1 (eql :identity))
                                (x2 (dimensioned-ref-object array sequence)))
  "Returns T if all elements of x2 satisfy the equals-object predicate ~
   pairwise."
  (if (cl:= 1 (number-of-elements x2))
    T
    (not (eq :FALSE
             (eref (reduce-slices
                    #'(lambda (logical-or-thing next-thing)
                        (cond
                         ((eq logical-or-thing :FALSE)
                          :FALSE)
                         ((= logical-or-thing next-thing)
                          next-thing)
                         (T :FALSE)))
                    x2)))
         )))



(defmethod-multi equals-object ((x1 (sequence array dimensioned-ref-object)) 
                                (x2 (symbol number)))
  "Returns two values.  The first is NIL unless the first arg contains a single ~
   element and it = the second argument.  ~
   The second value returned is an object of appropriate ~
   structure whose elements, T or NIL, record which elements ~
   = to the second argument.~
   (:see-also map-element)
   (:examples (:files eg:Arrays;num-preds.lisp~
   ))"
  (let* ((n1 (number-of-elements x1))
         (result1 (cl:= n1 1))
         result2)
    (flet ((doit (x y)
             (let ((temp-result (= x y)))
               (unless temp-result (setf result1 NIL))
               temp-result)))
      (setf result2
            (map-element
             #'doit nil x1 x2)))
    (values result1 result2)))

(defmethod-multi equals-object ((x1 (symbol number)) 
                                (x2 (sequence array dimensioned-ref-object)))
  "Returns two values.  The first is NIL unless the second arg contains a single ~
   element and it = the first argument.  ~
   The second value returned is an object of appropriate ~
   structure whose elements, T or NIL, record which elements ~
   = to the first argument.~
   (:see-also map-element)
   (:examples (:files eg:Arrays;num-preds.lisp~
   ))"
  (let* ((n2 (number-of-elements x2))
         (result1 (cl:= 1 n2))
         result2)
    (flet ((doit (x y)
             (let ((temp-result (= x y)))
               (unless temp-result (setf result1 NIL))
               temp-result)))
      (setf result2
            (map-element
             #'doit nil x1 x2)))
    (values result1 result2)))

(defmethod-multi equals-object ((x1 (dimensioned-ref-object array sequence))
                                (x2 (dimensioned-ref-object array sequence)))
  "Returns two values. The first is T if each element of the first argument ~
   = the corresponding element of the second.  If one or more comparisons ~
   fail, then NIL is returned.  The second value returned is an object of ~
   appropriate ~
   structure whose elements, T or NIL, record which elements ~
   of x1 equal the corresponding elements of x2.~
   (:see-also map-element)
   (:examples (:files eg:Arrays;num-preds.lisp~
   ))"
  (let* ((n1 (number-of-elements x1))
         (n2 (number-of-elements x2))
         (result1 (cl:= n1 n2))
         result2
         )
    (flet ((doit (x y)
             (let ((temp-result (= x y)))
               (unless temp-result (setf result1 NIL))
               temp-result)))
      (setf result2
            (map-element
             #'doit nil x1 x2)))
    (values result1 result2)))

;;;----------------------------------------------------------------------------
;;; Less Than
;;;

(defun < (arg1 &rest args)
  "Returns T if the arguments are arranged in strictly increasing order ~
   left to right.  ~
   (:required ~
   (:arg arg1 Argument to be compared to the remaining args.) ~
   ) ~
   (:rest ~
   (:arg args NIL The remaining things to be compared to arg1. ~
   If NIL, then less-than-object is called ~
   with arguments :identity and arg1.) ~
   )~
   (:returns If all arguments are strictly increasing, then T is returned, ~
   otherwise NIL. ~
   If all arguments are any one of sequence, array, or dimensioned-ref-object, ~
   then the comparison is element wise via map-element and two values are ~
   returned.  The first is as before, the second is an object of dimension ~
   equal to that of the argument having the most elements and has elements ~
   T or NIL corresponding to the result of the element-wise < comparison.) ~
   (:see-also (less-than-object :generic-function) (cl:< :function)) ~
   (:examples (:files eg:Arrays;num-preds.lisp~
   ))
   "
  (case (length args)
    (0 (less-than-object :identity arg1))
    (1 (less-than-object arg1 (first args)))
    (t
     (let ((all-args (cons arg1 args)))
       (if (every #'object-to-be-mapped-p all-args)
         (let ((result1 
                (apply #'cl:=
                       (mapcar #'number-of-elements all-args)))
               result2)
           (flet ((doit (&rest args)
                    (let ((temp-result (apply #'< args)))
                      (unless temp-result (setf result1 NIL))
                      temp-result)))
             (setf result2 (apply #'map-element #'doit nil all-args)))
           (values result1 result2))
         (if (less-than-object arg1 (first args))
           (apply #'< (first args) (rest args))
           nil)))
     ))
  )

(defgeneric less-than-object (x1 x2)
  (:documentation "The binary less than operator. ~
                   Returns T if the first argument is to be judged less than ~
                   the second and NIL otherwise.  Some methods may return multiple ~
                   values with more detailed information in the other values."))

(defmethod less-than-object ((x1 number) (x2 number))
  (cl:< x1 x2))

(defmethod less-than-object ((x1 number) (x2 matrix))
  (cond
   ((apply #'cl:= 1 (matrix-dimensions-of x2))
    (ext_< x1 (eref x2 0 0)))
   (T (call-next-method))))

(defmethod less-than-object ((x1 matrix) (x2 number))
  (cond
   ((apply #'cl:= 1 (matrix-dimensions-of x1))
    (ext_< x2 (eref x1 0 0)))
   (T (call-next-method))))


(defmethod less-than-object ((x1 number) (x2 symbol))
  (ext_< x1 x2 'less-than-object))

(defmethod less-than-object ((x1 symbol) (x2 number))
  (ext_< x1 x2 'less-than-object))

(defmethod less-than-object ((x1 symbol) (x2 symbol))
  (ext_< x1 x2 'less-than-object))

(defmethod less-than-object ((x1 (eql :identity)) (x2 number))
  (cl:< x2))

(defmethod less-than-object ((x1 (eql :identity)) (x2 symbol))
  (if (eq x2 -infinity)
    t
    (ext_< -infinity x2 'less-than-object)))

(defmethod less-than-object ((x1 t) (x2 t))
  (missing-method 'less-than-object x1 x2))


(defmethod-multi less-than-object ((x1 (eql :identity))
                                   (x2 (dimensioned-ref-object array sequence)))
  "Returns T if all elements of x2 satisfy the less-than-object predicate ~
   pairwise."
  (if (cl:= 1 (number-of-elements x2))
    T
    (not (eq :FALSE
             (eref (reduce-slices
                    #'(lambda (logical-or-thing next-thing)
                        (cond
                         ((eq logical-or-thing :FALSE)
                          :FALSE)
                         ((< logical-or-thing next-thing)
                          next-thing)
                         (T :FALSE)))
                    x2)))
         )))



(defmethod-multi less-than-object ((x1 (sequence array dimensioned-ref-object)) 
                                   (x2 (symbol number)))
  "Returns two values.  The first is NIL unless the first arg contains a single ~
   element and it < the second argument.  ~
   The second value returned is an object of appropriate ~
   structure whose elements, T or NIL, record which elements ~
   are < the second argument.~
   (:see-also map-element <)
   (:examples (:files eg:Arrays;num-preds.lisp~
   ))"
  
  (let* ((n1 (number-of-elements x1))
         (result1 (cl:= n1 1))
         result2)
    (flet ((doit (x y)
             (let ((temp-result (< x y)))
               (unless temp-result (setf result1 NIL))
               temp-result)))
      (setf result2
            (map-element
             #'doit nil x1 x2)))
    (values result1 result2)))

(defmethod-multi less-than-object ((x1 (symbol number)) 
                                   (x2 (sequence array dimensioned-ref-object)))
  "Returns two values.  The first is NIL unless the second arg contains a single ~
   element and it is < the first argument.  ~
   The second value returned is an object of appropriate ~
   structure whose elements, T or NIL, record which elements ~
   are < the first argument.~
   (:see-also map-element <)
   (:examples (:files eg:Arrays;num-preds.lisp~
   ))"
  
  (let* ((n2 (number-of-elements x2))
         (result1 (cl:= 1 n2))
         result2)
    (flet ((doit (x y)
             (let ((temp-result (< x y)))
               (unless temp-result (setf result1 NIL))
               temp-result)))
      (setf result2
            (map-element
             #'doit nil x1 x2)))
    (values result1 result2)))

(defmethod-multi less-than-object ((x1 (dimensioned-ref-object array sequence))
                                   (x2 (dimensioned-ref-object array sequence)))
  "Returns two values. The first is T if each element of the first argument ~
   is < the corresponding element of the second.  If one or more comparisons ~
   fail, then NIL is returned.  The second value returned is an object of ~
   appropriate ~
   structure whose elements, T or NIL, record which elements ~
   of x1 are less than the corresponding elements of x2.~
   (:see-also map-element <)
   (:examples (:files eg:Arrays;num-preds.lisp~
   ))"
  (let* ((n1 (number-of-elements x1))
         (n2 (number-of-elements x2))
         (result1 (cl:= n1 n2))
         result2)
    (flet ((doit (x y)
             (let ((temp-result (< x y)))
               (unless temp-result (setf result1 NIL))
               temp-result)))
      (setf result2
            (map-element
             #'doit nil x1 x2)))
    (values result1 result2)))


;;;----------------------------------------------------------------------------
;;; Greater Than
;;;

(defun > (arg1 &rest args)
  "Returns T if the arguments are arranged in strictly decreasing order ~
   left to right.  ~
   (:required ~
   (:arg arg1 Argument to be compared to the remaining args.) ~
   ) ~
   (:rest ~
   (:arg args NIL The remaining things to be compared to arg1. ~
   If NIL, then greater-than-object is called ~
   with arguments :identity and arg1.) ~
   )~
   (:returns If all arguments are strictly increasing, then T is returned, ~
   otherwise NIL. ~
   If all arguments are any one of sequence, array, or dimensioned-ref-object, ~
   then the comparison is element wise via map-element and two values are ~
   returned.  The first is as before, the second is an object of dimension ~
   equal to that of the argument having the most elements and has elements ~
   T or NIL corresponding to the result of the element-wise > comparison.) ~
   (:see-also (greater-than-object :generic-function) (cl:> :function)) ~
   (:examples (:files eg:Arrays;num-preds.lisp~
   ))
   "
  (case (length args)
    (0 (greater-than-object :identity arg1))
    (1 (greater-than-object arg1 (first args)))
    (t 
     (let ((all-args (cons arg1 args)))
       (if (every #'object-to-be-mapped-p all-args)
         (let ((result1 
                (apply #'cl:=
                       (mapcar #'number-of-elements all-args)))
               result2)
           (flet ((doit (&rest args)
                    (let ((temp-result (apply #'> args)))
                      (unless temp-result (setf result1 NIL))
                      temp-result)))
             (setf result2 (apply #'map-element #'doit nil all-args)))
           (values result1 result2))
         (if (greater-than-object arg1 (first args))
           (apply #'> (first args) (rest args))
           nil)))
     )))

(defgeneric greater-than-object (x1 x2)
  (:documentation "The binary less than operator. ~
                   Returns T if the first argument is to be judged greater than ~
                   the second and NIL otherwise.  Some methods may return multiple ~
                   values with more detailed information in the other values."))

(defmethod greater-than-object ((x1 number) (x2 number))
  (cl:> x1 x2))

(defmethod greater-than-object ((x1 number) (x2 matrix))
  (cond
   ((apply #'cl:= 1 (matrix-dimensions-of x2))
    (ext_> x1 (eref x2 0 0)))
   (T (call-next-method))))

(defmethod greater-than-object ((x1 matrix) (x2 number))
  (cond
   ((apply #'cl:= 1 (matrix-dimensions-of x1))
    (ext_> x2 (eref x1 0 0)))
   (T (call-next-method))))


(defmethod greater-than-object ((x1 number) (x2 symbol))
  (ext_> x1 x2 'greater-than-object))

(defmethod greater-than-object ((x1 symbol) (x2 number))
  (ext_> x1 x2 'greater-than-object))

(defmethod greater-than-object ((x1 symbol) (x2 symbol))
  (ext_> x1 x2 'greater-than-object))

(defmethod greater-than-object ((x1 (eql :identity)) (x2 number))
  (cl:> x2))

(defmethod greater-than-object ((x1 (eql :identity)) (x2 symbol))
  (if (eq x2 +infinity)
    t
    (ext_> +infinity x2 'greater-than-object)))

(defmethod greater-than-object ((x1 t) (x2 t))
  (ext_> x1 x2 'greater-than-object))


(defmethod-multi greater-than-object ((x1 (eql :identity))
                                (x2 (dimensioned-ref-object array sequence)))
  "Returns T if all elements of x2 satisfy the greater-than-object predicate ~
   pairwise."
  (if (cl:= 1 (number-of-elements x2))
    T
    (not (eq :FALSE
             (eref (reduce-slices
                    #'(lambda (logical-or-thing next-thing)
                        (cond
                         ((eq logical-or-thing :FALSE)
                          :FALSE)
                         ((> logical-or-thing next-thing)
                          next-thing)
                         (T :FALSE)))
                    x2)))
         )))



(defmethod-multi greater-than-object ((x1 (sequence array dimensioned-ref-object)) 
                                      (x2 (symbol number)))
  "Returns two values.  The first is NIL unless the first arg contains a single ~
   element and it > the second argument.  ~
   The second value returned is an object of appropriate ~
   structure whose elements, T or NIL, record which elements ~
   are > the second argument.~
   (:see-also map-element >)
   (:examples (:files eg:Arrays;num-preds.lisp~
   ))"
  (let* ((n1 (number-of-elements x1))
         (result1 (cl:= n1 1))
         result2)
    (flet ((doit (x y)
             (let ((temp-result (> x y)))
               (unless temp-result (setf result1 NIL))
               temp-result)))
      (setf result2
            (map-element
             #'doit nil x1 x2)))
    (values result1 result2)))

(defmethod-multi greater-than-object ((x1 (symbol number)) 
                                      (x2 (sequence array dimensioned-ref-object)))
  "Returns two values.  The first is NIL unless the second arg contains a single ~
   element and it is > the first argument.  ~
   The second value returned is an object of appropriate ~
   structure whose elements, T or NIL, record which elements ~
   are > the first argument.~
   (:see-also map-element >)
   (:examples (:files eg:Arrays;num-preds.lisp~
   ))"
  (let* ((n2 (number-of-elements x2))
         (result1 (cl:= n2 1))
         result2)
    (flet ((doit (x y)
             (let ((temp-result (> x y)))
               (unless temp-result (setf result1 NIL))
               temp-result)))
      (setf result2
            (map-element
             #'doit nil x1 x2)))
    (values result1 result2)))

(defmethod-multi greater-than-object ((x1 (dimensioned-ref-object array sequence))
                                      (x2 (dimensioned-ref-object array sequence)))
  "Returns two values. The first is T if each element of the first argument ~
   is > the corresponding element of the second.  If one or more comparisons ~
   fail, then NIL is returned.  The second value returned is an object of ~
   appropriate ~
   structure whose elements, T or NIL, record which elements ~
   of x1 are greater than the corresponding elements of x2.~
   (:see-also map-element >)
   (:examples (:files eg:Arrays;num-preds.lisp~
   ))"
  (let* ((n1 (number-of-elements x1))
         (n2 (number-of-elements x2))
         (result1 (cl:= n1 n2))
         result2)
    (flet ((doit (x y)
             (let ((temp-result (> x y)))
               (unless temp-result (setf result1 NIL))
               temp-result)))
      (setf result2
            (map-element
             #'doit nil x1 x2)))
    (values result1 result2)))


;;;----------------------------------------------------------------------------
;;; Less Than or Equals
;;;

(defun <= (arg1 &rest args)
  "Returns T if the arguments are arranged in non-decreasing order ~
   left to right.  ~
   (:required ~
   (:arg arg1 Argument to be compared to the remaining args.) ~
   ) ~
   (:rest ~
   (:arg args NIL The remaining things to be compared to arg1. ~
   If NIL, then less-than-equals-object is called ~
   with arguments :identity and arg1.) ~
   )~
   (:returns If all arguments are strictly increasing, then T is returned, ~
   otherwise NIL. ~
   If all arguments are any one of sequence, array, or dimensioned-ref-object, ~
   then the comparison is element wise via map-element and two values are ~
   returned.  The first is as before, the second is an object of dimension ~
   equal to that of the argument having the most elements and has elements ~
   T or NIL corresponding to the result of the element-wise <= comparison.) ~
   (:see-also (less-than-equals-object :generic-function) (cl:<= :function)) ~
   (:examples (:files eg:Arrays;num-preds.lisp~
   ))~
   "
  (case (length args)
    (0 (less-than-equals-object :identity arg1))
    (1 (less-than-equals-object arg1 (first args)))
    (t 
     (let ((all-args (cons arg1 args)))
       (if (every #'object-to-be-mapped-p all-args)
         (let ((result1 
                (apply #'cl:=
                       (mapcar #'number-of-elements all-args)))
               result2)
           (flet ((doit (&rest args)
                    (let ((temp-result (apply #'<= args)))
                      (unless temp-result (setf result1 NIL))
                      temp-result)))
             (setf result2 (apply #'map-element #'doit nil all-args)))
           (values result1 result2))
         (if (less-than-equals-object arg1 (first args))
           (apply #'<= (first args) (rest args))
           nil)))
     )))

(defgeneric less-than-equals-object (x1 x2)
  (:documentation "The binary less than or equals operator. ~
                   Returns T if the first argument is to be judged less than ~
                   or equal to the second and NIL otherwise.  Some methods may ~
                   return multiple ~
                   values with more detailed information in the other values.")
  )


(defmethod less-than-equals-object ((x1 number) (x2 number))
  (cl:<= x1 x2))

(defmethod less-than-equals-object ((x1 number) (x2 matrix))
  (cond
   ((apply #'cl:= 1 (matrix-dimensions-of x2))
    (ext_<= x1 (eref x2 0 0)))
   (T (call-next-method))))

(defmethod less-than-equals-object ((x1 matrix) (x2 number))
  (cond
   ((apply #'cl:= 1 (matrix-dimensions-of x1))
    (ext_<= x2 (eref x1 0 0)))
   (T (call-next-method))))


(defmethod less-than-equals-object ((x1 number) (x2 symbol))
  (ext_<= x1 x2 'less-than-equals-object))

(defmethod less-than-equals-object ((x1 symbol) (x2 number))
  (ext_<= x1 x2 'less-than-equals-object))

(defmethod less-than-equals-object ((x1 symbol) (x2 symbol))
  (ext_<= x1 x2 'less-than-equals-object))

(defmethod less-than-equals-object ((x1 (eql :identity)) (x2 number))
  (cl:<= x2))

(defmethod less-than-equals-object ((x1 (eql :identity)) (x2 symbol))
  (ext_<= -infinity x2 'less-than-equals-object))

(defmethod less-than-equals-object ((x1 t) (x2 t))
  (missing-method 'less-than-equals-object x1 x2))


(defmethod-multi less-than-equals-object
  ((x1 (eql :identity))
   (x2 (dimensioned-ref-object array sequence)))
  "Returns T if all elements of x2 satisfy the less-than-equals-object predicate ~
   pairwise."
  (if (cl:= 1 (number-of-elements x2))
    T
    (not (eq :FALSE
             (eref (reduce-slices
                    #'(lambda (logical-or-thing next-thing)
                        (cond
                         ((eq logical-or-thing :FALSE)
                          :FALSE)
                         ((<= logical-or-thing next-thing)
                          next-thing)
                         (T :FALSE)))
                    x2)))
         )))



(defmethod-multi less-than-equals-object ((x1 (sequence array dimensioned-ref-object)) 
                                          (x2 (symbol number)))
  "Returns two values.  The first is NIL unless the first arg contains a single ~
   element and it <= the second argument.  ~
   The second value returned is an object of appropriate ~
   structure whose elements, T or NIL, record which elements ~
   are <= the second argument.~
   (:see-also map-element <=)
   (:examples (:files eg:Arrays;num-preds.lisp~
   ))"
  (let* ((n1 (number-of-elements x1))
         (result1 (cl:= n1 1))
         result2)
    (flet ((doit (x y)
             (let ((temp-result (<= x y)))
               (unless temp-result (setf result1 NIL))
               temp-result)))
      (setf result2
            (map-element
             #'doit nil x1 x2)))
    (values result1 result2)))

(defmethod-multi less-than-equals-object
  ((x1 (symbol number)) 
   (x2 (sequence array dimensioned-ref-object)))
  "Returns two values.  The first is NIL unless the second arg contains a single ~
   element and it is <= the first argument.  ~
   The second value returned is an object of appropriate ~
   structure whose elements, T or NIL, record which elements ~
   are <= the first argument.~
   (:see-also map-element <=)
   (:examples (:files eg:Arrays;num-preds.lisp~
   ))"
  (let* ((n2 (number-of-elements x2))
         (result1 (cl:= 1 n2))
         result2)
    (flet ((doit (x y)
             (let ((temp-result (<= x y)))
               (unless temp-result (setf result1 NIL))
               temp-result)))
      (setf result2 (map-element #'doit nil x1 x2)))
    (values result1 result2)))

(defmethod-multi less-than-equals-object ((x1 (dimensioned-ref-object array sequence))
                                          (x2 (dimensioned-ref-object array sequence)))
  "Returns two values. The first is T if each element of the first argument ~
   is <= the corresponding element of the second.  If one or more comparisons ~
   fail, then NIL is returned.  The second value returned is an object of ~
   appropriate ~
   structure whose elements, T or NIL, record which elements ~
   of x1 are less than or equal to the corresponding elements of x2.~
   (:see-also map-element <=)
   (:examples (:files eg:Arrays;num-preds.lisp~
   ))"
  (let* ((n1 (number-of-elements x1))
         (n2 (number-of-elements x2))
         (result1 (cl:= n1 n2))
         result2)
    (flet ((doit (x y)
             (let ((temp-result (<= x y)))
               (unless temp-result (setf result1 NIL))
               temp-result)))
      (setf result2 (map-element #'doit nil x1 x2)))
    (values result1 result2)))

;;;----------------------------------------------------------------------------
;;; Greater Than or Equals
;;;

(defun >= (arg1 &rest args)
  "Returns T if the arguments are arranged in non-increasing order ~
   left to right.  ~
   (:required ~
   (:arg arg1 Argument to be compared to the remaining args.) ~
   ) ~
   (:rest ~
   (:arg args NIL The remaining things to be compared to arg1. ~
   If NIL, then greater-than-equals-object is called ~
   with arguments :identity and arg1.) ~
   )~
   (:returns If all arguments are strictly increasing, then T is returned, ~
   otherwise NIL. ~
   If all arguments are any one of sequence, array, or dimensioned-ref-object, ~
   then the comparison is element wise via map-element and two values are ~
   returned.  The first is as before, the second is an object of dimension ~
   equal to that of the argument having the most elements and has elements ~
   T or NIL corresponding to the result of the element-wise >= comparison.) ~
   (:see-also (greater-than-equals-object :generic-function) ~
   (cl:>= :function)) ~
   (:examples (:files eg:Arrays;num-preds.lisp~
   ))~
   "
  (case (length args)
    (0 (greater-than-equals-object :identity arg1))
    (1 (greater-than-equals-object arg1 (first args)))
    (t 
     (let ((all-args (cons arg1 args)))
       (if (every #'object-to-be-mapped-p all-args)
         (let ((result1 
                (apply #'cl:=
                       (mapcar #'number-of-elements all-args)))
               result2)
           (flet ((doit (&rest args)
                    (let ((temp-result (apply #'>= args)))
                      (unless temp-result (setf result1 NIL))
                      temp-result)))
             (setf result2 (apply #'map-element #'doit nil all-args)))
           (values result1 result2))
         (if (greater-than-equals-object arg1 (first args))
           (apply #'>= (first args) (rest args))
           nil)))
     )))

(defgeneric greater-than-equals-object (x1 x2)
  (:documentation "The binary greater than or equals operator. ~
                   Returns T if the first argument is to be judged greater than ~
                   or equal to the second and NIL otherwise.  Some methods may ~
                   return multiple ~
                   values with more detailed information in the other values."))

(defmethod greater-than-equals-object ((x1 number) (x2 number))
  (cl:>= x1 x2))

(defmethod greater-than-equals-object ((x1 number) (x2 matrix))
  (cond
   ((apply #'cl:= 1 (matrix-dimensions-of x2))
    (ext_>= x1 (eref x2 0 0)))
   (T (call-next-method))))

(defmethod greater-than-equals-object ((x1 matrix) (x2 number))
  (cond
   ((apply #'cl:= 1 (matrix-dimensions-of x1))
    (ext_>= x2 (eref x1 0 0)))
   (T (call-next-method))))

(defmethod greater-than-equals-object ((x1 number) (x2 symbol))
  (ext_>= x1 x2 'greater-than-equals-object))

(defmethod greater-than-equals-object ((x1 symbol) (x2 number))
  (ext_>= x1 x2 'greater-than-equals-object))

(defmethod greater-than-equals-object ((x1 symbol) (x2 symbol))
  (ext_>= x1 x2 'greater-than-equals-object))

(defmethod greater-than-equals-object ((x1 (eql :identity)) (x2 number))
  (cl:>= x2))

(defmethod greater-than-equals-object ((x1 (eql :identity)) (x2 symbol))
  (ext_>= +infinity x2 'greater-than-equals-object))

(defmethod greater-than-equals-object ((x1 t) (x2 t))
  (missing-method 'greater-than-equals-object x1 x2))

(defmethod-multi greater-than-equals-object
  ((x1 (eql :identity))
   (x2 (dimensioned-ref-object array sequence)))
  "Returns T if all elements of x2 satisfy the greater-than-equals-object predicate ~
   pairwise."
  (if (cl:= 1 (number-of-elements x2))
    T
    (not (eq :FALSE
             (eref (reduce-slices
                    #'(lambda (logical-or-thing next-thing)
                        (cond
                         ((eq logical-or-thing :FALSE)
                          :FALSE)
                         ((>= logical-or-thing next-thing)
                          next-thing)
                         (T :FALSE)))
                    x2)))
         )))



(defmethod-multi greater-than-equals-object
  ((x1 (sequence array dimensioned-ref-object)) 
   (x2 (symbol number)))
  "Returns two values.  The first is NIL unless the first arg contains a single ~
   element and it >= the second argument.  ~
   The second value returned is an object of appropriate ~
   structure whose elements, T or NIL, record which elements ~
   are >= the second argument.~
   (:see-also map-element >=)
   (:examples (:files eg:Arrays;num-preds.lisp~
   ))"
  (let* ((n1 (number-of-elements x1))
         (result1 (cl:= n1 1))
         result2)
    (flet ((doit (x y)
             (let ((temp-result (>= x y)))
               (unless temp-result (setf result1 NIL))
               temp-result)))
      (setf result2 (map-element #'doit nil x1 x2)))
    (values result1 result2)))

(defmethod-multi greater-than-equals-object
  ((x1 (symbol number)) 
   (x2 (sequence array dimensioned-ref-object)))
  "Returns two values.  The first is NIL unless the second arg contains a single ~
   element and it is >= the first argument.  ~
   The second value returned is an object of appropriate ~
   structure whose elements, T or NIL, record which elements ~
   are >= the first argument.~
   (:see-also map-element >=)
   (:examples (:files eg:Arrays;num-preds.lisp~
   ))"
  (let* ((n2 (number-of-elements x2))
         (result1 (cl:= n2 1))
         result2)
    (flet ((doit (x y)
             (let ((temp-result (>= x y)))
               (unless temp-result (setf result1 NIL))
               temp-result)))
      (setf result2 (map-element #'doit nil x1 x2)))
    (values result1 result2)))

(defmethod-multi greater-than-equals-object
  ((x1 (dimensioned-ref-object array sequence))
   (x2 (dimensioned-ref-object array sequence)))
  "Returns two values. The first is T if each element of the first argument ~
   is >= the corresponding element of the second.  If one or more comparisons ~
   fail, then NIL is returned.  The second value returned is an object of ~
   appropriate ~
   structure whose elements, T or NIL, record which elements ~
   of x1 are greater than or equal to the corresponding elements of x2.~
   (:see-also map-element >=)
   (:examples (:files eg:Arrays;num-preds.lisp~
   ))"
  (let* ((n1 (number-of-elements x1))
         (n2 (number-of-elements x2))
         (result1 (cl:= n1 n2))
         result2)
    (flet ((doit (x y)
             (let ((temp-result (>= x y)))
               (unless temp-result (setf result1 NIL))
               temp-result)))
      (setf result2 (map-element #'doit nil x1 x2)))
    (values result1 result2)))

(defun test-elements (test-fun arg1 &optional arg2)
  "Test-fun is a predicate test that takes two arguments and returns T or NIL.  ~
   If arg2 is given, then the elements of arg1 are arg2 are compared to one ~
   another using test-fun.  The order in which the elements are compared are ~
   as determined by map-element.  ~
   If arg2 is not given, then the elements of arg1 are compared to one ~
   another using the test-fun.  ~
   to an element of arg1 and an element of ~
   Returns T if all comparisons return T, NIL otherwise.  ~
   When arg2 is supplied, two values are returned.  ~
   The first as before, but the second is an object of appropriate ~
   structure whose elements are the result of applying the test-fun ~
   to the corresponding elements of the first and second arguments.  ~
   (:see-also (map-element :function)) ~
   (:examples (:files eg:Arrays;num-preds.lisp~
   )) ~
   "
  (if arg2
    (let* ((result1 (cl:= (number-of-elements arg1)
                                 (number-of-elements arg2)))
           result2)
      (flet ((doit (x y)
               (let ((temp-result (funcall test-fun x y)))
                 (unless temp-result (setf result1 NIL))
                 temp-result)))
        (setf result2 (map-element #'doit nil arg1 arg2)))
      (values result1 result2))
    (if (cl:= 1 (number-of-elements arg1))
      T
      (not (eq :FALSE
               (eref (reduce-slices
                      #'(lambda (logical-or-thing next-thing)
                          (cond
                           ((eq logical-or-thing :FALSE)
                            :FALSE)
                           ((funcall test-fun logical-or-thing next-thing)
                            next-thing)
                           (T :FALSE)))
                      arg1)))
         ))
    )
  )
