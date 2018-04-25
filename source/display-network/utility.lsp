;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               utility.lisp                               
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
;;; this file adds some methods to objects and defines some other
;;; utility functions
;;;

;;;
;;; package definitions
;;;

(in-package :quail)


(eval-when (:compile-toplevel :load-toplevel :execute) (export '(list-slot-contents! 
          
          
          class-p
          classify-local-methods
          classify-inherited-methods
          classify-combined-methods
          should-not-implement
          slot-initarg
          slot-initform
          sub-class-responsibility
          list-required-inits
          descriptive-label
          get-name
          class-understands

          required-init

          $
          $!
          get-mc)))


(defun mode-of-list (the-list)
  "Determines the common type of the elements of a list in order to build an ~
   array of the correct type.  The different available modes are ~
   STRING, FLOAT and BOOLEAN."
  
  (let ((common-mode 'i-dont-know))
    (dolist (item the-list)
      (case common-mode
        (i-dont-know (cond ((or (null item)
                                (eq item 't))
                            (setq common-mode 'boolean))
                           ((or (integerp item)
                                (floatp item))
                            (setq common-mode 'float))
                           (t (setq common-mode 'string))))
        ((boolean) (cond ((or (null item)
                              (eq item 't))
                          nil)
                         (t (setq common-mode 'string))))
        ((float) (cond ((or (integerp item)
                            (floatp item))
                        nil)
                       (t (setq common-mode 'string))))
        ((string) (return))))
    common-mode))


(defmacro required-init (init-form)
       (declare (ignore init-form))
       nil)


(defun requires-variable (var)
    "Prompts for var."

       (let ((result (quail-prompt-read (concatenate 'string 
                                                     "Enter the value for " 
                                                     (symbol-name var)))))
          (if (symbolp result)
              (or ($! result t) 
                  (list 'quote result))
              (eval result))))

;;;
;;; method definitions related to PCL objects
;;;

(defmethod list-slot-contents! ((self standard-class)
                  slot &optional verbose-flg
                  (sub-classes nil))
       

;;; 
;;; returns the list of the contents of the slot SLOT concatenated with the
;;; contents of the slots SLOT of   
;;;    - the supers of SELF recursively if SUB-CLASSES is NIL
;;;    - the sub-classes of SELF recursively if SUB-CLASSES is not NIL
;;; Omit content of STANDARD-OBJECT and CLASS unless Verbose-flg is T
;;; 

       (let ((result nil)
             value)
            
            ;; 
            ;;  we bind Value with the Slot-value SLOT of self and transforme
            ;; it in a List
            ;; 
            (or (listp (setf value (slot-value self slot)))
                (setf value (list value)))
            
            ;; 
            ;; we then push each element of this list which isn't already in
            ;; result
            ;; 
            (dolist (element value)
                (or (member element result)
                    (push element result)))
            
            ;; 
            ;; we call this method recursively for each super of SELF or its
            ;; SUB-CLASSES depending on the value of SUB-CLASSES
            ;; 
            (dolist (super-or-subs (if sub-classes
                                       (class-direct-subclasses self)
                                       (class-direct-superclasses self)))
                (unless (or verbose-flg
                            (member super-or-subs
                                   (list (find-class 'standard-class)
                                         (find-class 'standard-object))))
                    (setf result (append result
                                         (list-slot-contents! super-or-subs 
                                                slot 
                                                verbose-flg 
                                                sub-classes)))))
            
            ;; 
            ;; Here is the result of the method
            ;; 
            result))




(defmethod classify-local-methods ((self standard-class) 
                                   &optional (stop-list nil))
       

;;; 
;;; Index the methods of this class  according to their Method-classification
;;; property. The result is a propList style list of pairs   <Classification>
;;; <List of methods so classified>
;;; The classification of methods is stored in global variable
;;; *quail-METHODS-CLASSIFICATION*
;;; 

       (let (result classification characteristics)
            (dolist (method (slot-value self 'direct-methods))
              (when (setf characteristics (get-mc method))
                    (setf classification (second characteristics))
                    (setf (getf result classification)
                          (push (list (third characteristics)
                                      (first characteristics)
                                      "")
                                (getf result classification)))))
            result))

(defmethod classify-inherited-methods ((self standard-class)
                                       &optional
                                       (stop-list nil)
                                       (allowed-classes nil))
       

;;; 
;;; Index the methods of the Supers of this Class according to their
;;; Method-Classification property. The Result is a propList style List of
;;; pairs <Classification> <list of methods so classified>
;;; When called by the user, Allowed-classes is usually NIL but it is used to
;;; save time in the recursion by the method itself
;;; 

       (declare (special *quail-methods-classification*))
       (flet ((list-understands (list-of-objects method)
                     
                     ;; 
                     ;; Returns T if method is understood by all the elements
                     ;; of LIST-OF-OBJECTS
                     ;; Returns NIL if one element doesn't understand method
                     ;; 
                     (unless (listp list-of-objects)
                         (setf list-of-objects (list list-of-objects)))
                     (dolist (object list-of-objects t)
                         (if (not (class-understands (find-class object)
                                         method))
                             (return nil))))
              

;;; 

              (list-not-understands (list-of-objects method)
                     
                     ;; 
                     ;; Returns T if method isn't understood by all elements
                     ;; of LIST-OF-OBJECTS
                     ;; Returns NIL if one element understands method
                     ;; 
                     (unless (listp list-of-objects)
                         (setf list-of-objects (list list-of-objects)))
                     (dolist (object list-of-objects t)
                         (if (class-understands (find-class 
                                                       object)
                                    method)
                             (return nil)))))
             

;;; 

             (let (result)
                  (dolist (x *quail-methods-classification*)
                    (if (and (class-understands self (first x))
                             (list-not-understands stop-list
                                    (first x))
                             (list-understands allowed-classes
                                    (first x)))
                        (setf (getf result (second x))
                              (push (list (third x)
                                          (first x)
                                          "")
                                    (getf result (second x))))))
                  result)))


(defmethod should-not-implement ((self t) selector)
       

;;; 
;;; A do nothing method used to void specified inherited methods. The method
;;; Should-not-implement produces an error if actually invoked
;;; 

       (quail-error "~S is not implemented for the class : ~S" selector
              (class-name (class-of self))))

(defmethod sub-class-responsibility ((self t) selector)
       

;;; 
;;; This method indicates that the implementation of the method given by
;;; selector depends on the subclass, even though, abstractly, it is a method
;;; of some generic class. The method Sub-Class-Responsability produces an
;;; ERROR if actually invoked
;;; 

       (quail-error "~S must be specialized for ~S" selector
              (class-name (class-of self))))

(defmethod list-required-inits ((self standard-class) &optional (stop-list NIL))
 "List those variables which require initialization."

 (let (forbidden-ivs result)
      
      ;; 
      ;; Compute undesirable IVs
      ;; 
      (dolist (class (if (listp stop-list)
                         stop-list
                         (list stop-list)))
          (dolist (iv (class-slots class))
              (when (and (not (member iv forbidden-ivs))
                         (listp (slot-initform iv))
                         (equal (first (slot-initform iv))
                                'required-init))
                    (push iv forbidden-ivs))))
      
      ;; 
      ;; Compute IV to init which are not undesirable
      ;; 
      (dolist (iv (class-slots self))
          (when (and (not (member iv forbidden-ivs))
                     (listp (slot-initform iv))
                     (equal (first (slot-initform iv))
                            'required-init))
                (push iv result)))
      result))



(defmethod descriptive-label ((self standard-class))
  ;; Construct a descriptive string label.
  (get-name self))

(defmethod get-name ((self standard-class))
  ;; Read the name of an object SELF. 

       (class-name self))


(defmethod class-understands ((self standard-class) selector)
                                               ; Returns NIL if instances of
                                               ; SELF doesn't understand the
                                               ; method SELECTOR, T otherwise
       (if (generic-function-p selector)
           (let (result)
                (dolist (super (class-precedence-list self))
                    (when (get-method (symbol-function selector)
                                 nil
                                 (list super)
                                 nil)
                        (setf result t)
                        (return)))
                result)
           nil))


(defmethod zoom ((self standard-class))

   (inspect self))


;;;
;;; other functions
;;;


(defun slot-initarg (slot)
  "Return the initarg of slot."
  (first (slot-value slot 'initargs)))


(defun slot-initform (slot)
  "Return the initform of slot."
  (slot-value slot 'initform))


(defmacro $ (name)
  "Find an object from its name."
  
  (declare (special *quail-object-names*))
  
  `(let ((result (if (boundp '*quail-object-names*)
                   (gethash ',name 
                            *quail-object-names*))))
     (or result (quail-error "No Object named ~S" ',name))))


(defun $! (name &optional no-error-flg)
  "Returns the object which name is the value of name if it is a symbol ~
   If name is a list, we look for the evaluation of this list~
   if name is an object, it is returned. ~
   This is done by consulting the Hash table *quail-object-names* which is~
   updated every time set-name is called. ~
   If no-error-flg is T and no object is named name, NIL is returned instead~
   of an error."
  (declare (special *quail-object-names*))
  (if (listp name)
    (setf name (eval name)))
  (if (typep name 'standard-object)
    name
    (let ((result (if (boundp '*quail-object-names*)
                    (gethash name *quail-object-names*))))
      (if name
        (or result (if no-error-flg
                     nil
                     (quail-error "No object named ~S" name)))))))

(defun get-mc (method)
  "MC stands for Method Classification. ~
   Returns the value of the Property classification of method by looking in~
   global variable *quail-methods-classification*."
  
  (declare (special *quail-methods-classification*))
  (if (symbolp method)
    (assoc method *quail-methods-classification*)
    (let ((name (slot-value (slot-value method
                                        'generic-function)
                            'name)))
      (if (symbolp name)
        (assoc name *quail-methods-classification*)))))


(defun add-function-in-menu (function name-in-menu classification &rest args)

;;; make a function callable by menu.
;;; name-in-menu is the name which will be printed for function in menu
;;; classification enables to create a one level hierarchy 
;;; args is the list of compulsory arguments to the function

  (declare (special *quail-methods-classification*))
  (push (list function classification name-in-menu args) 
        *quail-methods-classification*))


(defun add-function-in-menu (function name-in-menu classification &rest args)

;;; make a function callable by menu.
;;; name-in-menu is the name which will be printed for function in menu
;;; classification enables to create a one level hierarchy 
;;; args is the list of compulsory arguments to the function

  (declare (special *quail-methods-classification*))
  (push (list function classification name-in-menu args) 
        *quail-methods-classification*))


;;;
;;; How to make a function callable from a menu
;;; you just have to call the function add-function-in-menu and give 
;;; as arguments the name of the function (with package if not in Quail)
;;; the label you want to appear in the menu
;;; the classification, which enables you to create a hierarchy in menus
;;; the compulsory parameters, which the user will be asked to complete
;;; if they are unknown when the function is called

(defvar *quail-methods-classification* nil)


