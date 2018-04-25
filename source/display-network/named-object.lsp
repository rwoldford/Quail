;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               named-object.lisp                               
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

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(named-object)))

(defclass named-object ()
  ((name :initform nil :documentation "Name of this object."))
  (:documentation
   "Enables to give a name to an Object. The name must be updated using ~
    set-name method exclusively. Do not change directly the slot-value because ~
    an association list *quail-object-name* must be  updated in the same time ~
    which permits to find rapidly an object given its name ~
    An object can be found when we know its name using the macro ($ Name) ~
    which returns Object." ))

(defmethod get-name ((self named-object))
       

;;; 
;;; Read the name of an object SELF. 
;;; 

       (slot-value self 'name))



(defmethod set-name ((self named-object)
                     &optional name)
  "Give a name to an Object. If name is NIL, ask the user for a name ~
   the hash table *quail-object-names* is updated by this method.  ~
   The equivalence is made on the printed name only, so that the user doesn't ~
   have to care about packages. So, the name in the equivalence variable ~
   *quail-object-names* is a string."
  
  (declare (special *quail-object-names*))
  (or name (setq name (prompt-read self 
                                   "Name to be given to this object : ")))
  (let ((previous-name (get-name self)))
    (if ($! name t)
      
      ;; 
      ;; doesn't allow 2 objects with the same name. So if the name
      ;; is already used, the user is asked whether he wants to
      ;; remove the previous existing name he has given or he wants
      ;; to put another name
      ;; 
      (if (member (prompt-read self
                               (format nil 
                                       "The name ~S is already used, do you want to set this name anyway (Y or N) "
                                       name))
                  '(y \y yes |yes|))
        (setf (slot-value ($! name)
                          'name)
              nil)
        (progn (set-name self)
               (setq name nil))))
    
    ;; 
    (if name
      (progn 
        ;; Build an empty Hash-Table if it doesn't exist yet
        ;; 
        (or (boundp '*quail-object-names*)
            (setq *quail-object-names* (make-hash-table)))
        
        ;; 
        ;; Update Hash-Table with new name
        ;; 
        (if previous-name (remhash previous-name 
                                   *quail-object-names*))
        (setf (gethash name *quail-object-names*)
              self)
        
        ;; 
        ;; Update Slot-name of self
        ;; 
        (setf (slot-value self 'name)
              name)))
    self))

