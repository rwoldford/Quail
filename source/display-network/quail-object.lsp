;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               quail-object.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1992 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1992.
;;;
;;;
;;;----------------------------------------------------------------------------

(in-package :quail)


;;;
;;; First mix the new class definitions into Quail-object
;;;

(eval-when (load compile)
  (loop
    for c in '(prompt-mixin linked-object indexed-object
               editable-object named-object documented-object)
    do
    (add-mixin-to-quail-object c)))

#|
(defmethod init-ivs ((self ref-array))
       

;;; 
;;; Specialization
;;; 

       self)

|#

(defmethod list-subs ((self quail-object) &optional (dont-signal-error nil))
    "Return a list comprised of the sub-structures of self. Invoked by a ~
     MicroView. SubClassResponsibility to provide the definitive description."

       (if dont-signal-error
           nil
           (sub-class-responsibility self 'list-subs)))

(defmethod descriptive-label ((self quail-object))
  "Construct a descriptive string label SuClass responsibility to specialize ~
   with more elaborate description."

       (let ((my-name (get-name self))
             (my-class (class-name (class-of self))))
            (concatenate 'string (if my-name
                                     (string my-name)
                                     " ")
                   " "
                   (string my-class))))