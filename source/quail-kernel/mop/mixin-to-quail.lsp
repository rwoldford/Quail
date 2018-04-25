;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               mixin-to-quail.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1992 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1992
;;;
;;;----------------------------------------------------------------------------------
;;;
;;;
;;

(in-package :quail-kernel)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(add-mixin-to-quail-object
          remove-mixin-from-quail-object)))



(defun add-mixin-to-quail-object (super)
  "Add super to the front of the supers list of the ~
   class quail-object.  The value of the argument super ~
   must be a symbol naming the class to be added."
  (let (supers)
    (setf supers
          (cons super
                (loop
                  for s in (class-direct-superclasses (find-class 'quail-object))
                  collect (class-name s))
                      ))
    (eval
     `(defclass quail-object ,supers
        ()
        (:documentation
         "A class which gathers the many different mixins which together ~
          define a quail-object.")))
    )
  )
        
    
(defun remove-mixin-from-quail-object (super)
  "Removes super from the supers list of the ~
   class quail-object.  The value of the argument super ~
   must be a symbol naming the class to be removed."
  (let (supers)
    (setf supers
          (remove super
                (loop
                  for s in (class-direct-superclasses (find-class 'quail-object))
                  collect (class-name s))
                      ))
    (eval
     `(defclass quail-object ,supers
        ()
        (:documentation
         "A class which gathers the many different mixins which together ~
          define a quail-object.")))
    )
  )
