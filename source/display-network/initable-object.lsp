;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               initable-object.lisp                               
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

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(initable-object spawning-expression-of
          )))

(defclass initable-object ()
  ((spawning-expression
    :initform nil
    :accessor spawning-expression-of
    :documentation "The expression used to create this object."))
  (:documentation "A useful mixin to record that permits various controls at ~
                   initialization time."))

(defmethod initialize-instance :after ((object initable-object) &key)
    "Examines a new instance making certain that all IV's which require ~
     initialization are initialized then invoke the AfterInit ~
     method."

       (init-ivs object)
       (after-init object)
       (set-anonymous-object object)
       (after-building object)
       object)



(defmethod init-ivs ((self initable-object))
  "Initialize all IVs for this new instance whose have a Required Init.  ~
   We recognize a Required init because the INITFORM has the value ~
   (REQUIRED-INIT XXX) ~
   If XXX is T then the method simply checks that an initial value was ~
   supplied to  the IV (via New-With-Values). If the value of the required ~
   Init is a form or can be applied then EVAL the form or APPLY the function ~
   to determine an initial value for the IV."
 (dolist (iv (list-required-inits (class-of self)))
     (if (eq (slot-value self (slot-value iv 'name))
             (eval (slot-initform iv)))
         (let ((initializer (second (slot-initform iv))))
              (setf (slot-value self (slot-value iv 'name))
                    (if (eq initializer t)
                        (quail-error "Required Initialization missing for ~S"
                               (slot-value iv 'name))
                        (if (listp initializer)
                            (eval initializer)
                            (if (functionp initializer)
                                (funcall initializer self (slot-value
                                                           iv
                                                           'name))
                                (quail-error "Can't initialize ~S with ~S"
                                       (slot-value iv 'name)
                                       initializer)))))))))


(defmethod after-init ((self initable-object))
  "A method which is invoked just after creation of a new instance. This ~
   method only update Spawning Expression and return self. Specializations ~
   however should call-next-method, do some work then return Self."

       (declare (special *quail-expression*))
       
       ;; 
       ;; The *quail-EXPRESSION* is updated in method CALL-METHOD-AND-LINK
       ;; if the new node is built using the menus of the browser. Otherwise,
       ;; it means that a command has been entered using the listener and the
       ;; *quail-EXPRESSION* can be obtained by consulting the last item of
       ;; the History list
       ;; 
       (unless (and (boundp '*quail-expression*)
                    (listp *quail-expression*))
           (setq *quail-expression* (get-last-typed-command))
           (or (listp *quail-expression*)
               (setq *quail-expression* (list *expression* nil))))
               
       (setf (slot-value self 'spawning-expression)
             *quail-expression*)
       
       ;; we then reset *quail-EXPRESSION* for other uses

       (makunbound '*quail-expression*)

       self)


(defmethod after-building ((self initable-object))
  "This method updates the spawning-Expression and causal-links of objects ~
   generated simultaneously with a main object."
  
  (let (link)
    (dolist (instance-variable (set-difference (class-slots (class-of
                                                             self))
                                               (list-required-inits (class-of
                                                                     self))))
      (when (typep (setq link (slot-value self (slot-value
                                                instance-variable
                                                'name)))
                   'linked-object)
        (causal-link self link)
        (setf (slot-value link 'spawning-expression)
              (slot-value self 'spawning-expression))))))

