;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               init-new-math.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (C) 1990 Statistical Computing Laboratory, University Of Waterloo
;;;
;;; 
;;;  Authors:
;;;     Greg Anglin 1989, 1990.
;;;     R.W. Oldford 1989, 1990, 1994
;;;     M.E. Lewis 1991.
;;;
;;;--------------------------------------------------------------------------------

(in-package :new-math)

;;;-----------------------------------------------------------------------
;;; Gives the "Quail meaning" to all those Common Lisp functions
;;; reimplemented using map-element.  They are listed in new-math.lisp.                   

(defmacro make-extended-quail-function (function-name)
  
  "Generalizes the function named, using map-element."
  (let* ((cl-function-name (find-symbol (string function-name) 
                                               (string-upcase "cl")))
         (lambda-list (qk::get-lambda-list cl-function-name))
         (pruned-lambda-list
          (seq-difference lambda-list
                          '(&OTHER-LAMBDA-KEYS
                            &ENVIRONMENT
                            &BODY
                            &WHOLE
                            &ALLOW-OTHER-KEYS
                            &AUX
                            &OPTIONAL
                            &KEY
                            &REST
                            &REQUIRED))))
  ;(format t "~%cl-function-name is ~s of type ~s " cl-function-name (type-of cl-function-name))
    (if (intersection '(&REST &BODY) lambda-list)
      `(defun ,function-name ,lambda-list
         (apply #'map-element 
                #',(make-name "ext_" function-name)
                nil
                ,@pruned-lambda-list) )
    `(defun ,function-name ,lambda-list
       (map-element 
              #',(make-name "ext_" function-name)
              nil
              ,@pruned-lambda-list)))))


(defmacro make-simple-quail-function (function-name)
  
  "Generalizes the function named, using map-element."
  (let* ((cl-function-name (find-symbol (string function-name) 
                                               (string-upcase "cl")))
         (lambda-list (qk::get-lambda-list cl-function-name))
         (pruned-lambda-list
          (seq-difference lambda-list
                          '(&OTHER-LAMBDA-KEYS
                            &ENVIRONMENT
                            &BODY
                            &WHOLE
                            &ALLOW-OTHER-KEYS
                            &AUX
                            &OPTIONAL
                            &KEY
                            &REST
                            &REQUIRED))))
    
    (cond
     ((intersection '(&REST &BODY) lambda-list)
      `(defun ,function-name ,lambda-list
         (apply #'map-element 
                #',cl-function-name
                nil
                ,@pruned-lambda-list))
      )
     ((intersection '(&OPTIONAL) lambda-list)
      (let ((optional (second (member '&OPTIONAL lambda-list)))
            (required-args (butlast pruned-lambda-list)))
      `(defun ,function-name ,lambda-list
         (if ,optional
           (map-element 
            #',cl-function-name
            nil
            ,@pruned-lambda-list)
           (map-element 
            #',cl-function-name
            nil
            ,@required-args)))))
     (T
      `(defun ,function-name ,lambda-list
         (map-element 
          #',cl-function-name
          nil
          ,@pruned-lambda-list))))))


(eval-when (:load-toplevel :execute);(eval load) 12JAN2021
  
  "Redefines all the *logical-numerical-ops*."
  
  (dolist (fn *logical-numerical-ops*)
    
    (eval `(make-simple-quail-function ,fn))
    (setf (documentation fn 'function)
          (documentation 
           (find-symbol (symbol-name fn)
                        :cl)
           'function))
    )
  
  (dolist (fn *numerical-type-coercion-functions*)
    
    (eval `(make-simple-quail-function ,fn))
    (setf (documentation fn 'function)
          (documentation 
           (find-symbol (symbol-name fn)
                        :cl)
           'function))
    )

  (dolist (fn *extended-simple-functions*)
    
    (eval `(make-extended-quail-function ,fn))
    (setf (documentation fn 'function)
          (documentation 
           (find-symbol (symbol-name fn)
                        :cl)
           'function))
    )
  
  ;; the functions float and rational must 
  ;; preserve their meaning as data types.
  
  (deftype float ()
    `(satisfies cl:floatp))
  
  (deftype rational ()
    `(satisfies cl:rationalp)))

