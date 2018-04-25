;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               browser-pcl.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1988-1991 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     G. Desvignes 1988,1989
;;;
;;;
;;;----------------------------------------------------------------------------------
;;;
;;;
;;; pcl corrections or complements
;;;
;;; export get-method which is defined in CLOS 
;;; specifications define an easy to use function to delete 
;;; methods
;;; export class object
;;; define readers for slots subclasses and name of a class
;;;


(use-package 'pcl )
(in-package 'pcl)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(get-class-precedence-list
          get-method
          get-object-name
          inspect-object
          inspect
          methodp
          name
         ; object
          object-sub-classes
          object-p
          rm)))


;;;
;;; function definition
;;;

(defun rm (method class)
  "This is an easy to use function to remove a method ~
   METHOD defined in a class CLASS from the generic function.  ~
   METHOD is the method name and CLASS ~
   is the class name where it is defined."

 (let ((class-object (find-class class))
       (method-object (symbol-function method))
       result)
      (dolist (x (slot-value method-object 'methods))
         (if (and x (eq (first (slot-value x 'type-specifiers))
                        class-object))
             (return (setf result x))))
      (remove-method method-object result)))







