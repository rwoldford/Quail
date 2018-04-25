;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                             quail-utility.lisp                              
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1991 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     G. Desvignes 1988 - 1989
;;;     R.W. Oldford 1985 - 1991.
;;;
;;;
;;;--------------------------------------------------------------------------------

(in-package :quail)
;;;
;;; this file adds some methods to PCL objects and defines some other
;;; utility functions
;;;

;;;
;;; package definitions
;;;

(import '(specific:push-in-buffer
          quail-mop:direct-subclasses
          quail-mop:direct-superclasses
          quail-mop:class-p
          quail-mop:class-slots
          quail-mop:direct-methods
          ))

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(list-slot-contents! 
          direct-subclasses
          direct-superclasses
          class-p
          class-slots
          direct-methods
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
          *quail-methods-classification*
          $
          $!
          get-mc)))


(defvar *quail-methods-classification* nil)

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
;;; Omit content of STANDARD-OBJECT and STANDARD-CLASS unless Verbose-flg is T
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
  "Index the methods of this class  according to their Method-classification ~
   property. The result is a propList style list of pairs   <Classification> ~
   <List of methods so classified>.  ~
   The classification of methods is stored in global variable ~
   *quail-methods-classification*."

       (let (result classification characteristics)
            (dolist (method (class-direct-methods self))
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

       (declare (special Quail:*quail-methods-classification*))
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
                         (if (class-understands (find-class object) method)
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
  "A do nothing method used to void specified inherited methods. The method ~
   Should-not-implement produces an error if actually invoked."

       (quail-error "~S is not implemented for the class : ~S" selector
              (class-name (class-of self))))

(defmethod sub-class-responsibility ((self t) selector)
  "This method indicates that the implementation of the method given by ~
   selector depends on the subclass, even though, abstractly, it is a method ~
   of some generic class. The method Sub-Class-Responsibility produces an ~
   error if actually invoked."

       (quail-error "~S must be specialized for ~S" selector
              (class-name (class-of self))))

(defmethod list-required-inits ((self standard-class) &optional (stop-list NIL))
  "Returns a list of those variables which require initialization."

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
  "Construct a descriptive string label."
  (get-name self))

(defmethod get-name ((self standard-class))
  "Read the name of the object." 
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
  "Find a quail object by giving its name."
  (declare (special *quail-object-names*))
  `(let ((result (if (boundp '*quail-object-names*)
                   (gethash ',name 
                            *quail-object-names*))))
     (or result (quail-error "No Object named ~S" ',name))))


(defun $! (name &optional no-error-flg)
       

;;; 
;;; Returns the object whose name is the value of NAME if it is a symbol
;;; If NAME is a list, we look for the evaluation of this list
;;; if NAME is an object, it is returned
;;; This is done by consulting the Hash table *quail-object-NAMES* which is
;;; updated every time SET-NAME is called
;;; IF NO-ERROR-FLG is T and no object is named NAME, NIL is returned instead
;;; of an error
;;;  

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
                                   (quail-error "No object named ~S" 
                                          name)))))))

(defun get-mc (method)
  "MC stands for Method Classification.  ~
   Returns the value of the Property CLASSIFICATION of method by looking in ~
   global variable *quail-methods-classification*."

       (declare (special Quail:*quail-methods-classification*))
       (if (symbolp method)
           (assoc method Quail:*quail-methods-classification*)
           (let ((name (slot-value (slot-value method
                                       'generic-function)
                           'name)))
                (if (symbolp name)
                    (assoc name Quail:*quail-methods-classification*)))))


(defun add-function-in-menu (function name-in-menu classification &rest args)
  "Make a function callable by menu.  ~
   The second argument, name-in-menu, is the name which will be printed ~
   for function in menu ~
   classification enables to create a one level hierarchy ~
   args is the list of compulsory arguments to the function."

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


(add-function-in-menu #'in-view "in view" '|Objects in this view| 'self)
(add-function-in-menu #'in-view! "in view!" '|Objects in this view| 'self)
(add-function-in-menu #'widen-view "widen view" '|Widen this view| 'self)
(add-function-in-menu #'add-forward-analysis-link "add forward analysis link"
                      'Link-operators 'self 'linkee)
(add-function-in-menu #'analysis-link "analysis link"
                      'Link-operators 'self 'linkee)
(add-function-in-menu #'analysis-unlink "analysis unlink"
                      'Link-operators 'self 'linkee)
(add-function-in-menu #'remove-back-analysis-link "remove back analysis-link"
                      'Link-operators 'self 'linkee)
(add-function-in-menu #'remove-forward-analysis-link "remove forward analysis-link"
                      'Link-operators 'self 'linkee)
(add-function-in-menu 'and "and" 'binary-operation 'arg-left 'arg-right)
(add-function-in-menu 'or "or" 'binary-operation 'arg-left 'arg-right)
(add-function-in-menu 'xor "xor" 'binary-operation 'arg-left 'arg-right)
(add-function-in-menu #'* "*" 'binary-operation 'arg-left 'arg-right)
(add-function-in-menu #'+ "+" 'binary-operation 'arg-left 'arg-right)
(add-function-in-menu #'- "-" 'binary-operation 'arg-left 'arg-right)
(add-function-in-menu #'/ "/" 'binary-operation 'arg-left 'arg-right)
(add-function-in-menu #'< "<" 'binary-comparison 'arg-left 'arg-right)
(add-function-in-menu #'= "=" 'binary-comparison 'arg-left 'arg-right)
(add-function-in-menu #'> ">" 'binary-comparison 'arg-left 'arg-right)
(add-function-in-menu #'mod "mod" 'binary-operation 'arg-left 'arg-right)
(add-function-in-menu #'rem "rem" 'binary-operation 'arg-left 'arg-right)
(add-function-in-menu '^ "^" 'binary-operation 'arg-left 'arg-right)
(add-function-in-menu #'/= "/=" 'binary-comparison 'arg-left 'arg-right)
(add-function-in-menu '! "!" 'reexpression 'self)
(add-function-in-menu #'abs "abs" 'reexpression 'self)
(add-function-in-menu #'acos "acos" 'reexpression 'self)
(add-function-in-menu #'exp "exp" 'reexpression 'self)
(add-function-in-menu #'expt "expt" 'reexpression 'self 'exponent)
(add-function-in-menu #'floor "floor" 'reexpression 'self)
;;(add-function-in-menu #'gamma "gamma" 'reexpression 'self)
;;(add-function-in-menu #'lgamma "lgamma" 'reexpression 'self)
(add-function-in-menu #'log "log" 'reexpression 'self)
(add-function-in-menu #'log "log base a" 'reexpression 'self 'a)
(add-function-in-menu #'sin "sin" 'reexpression 'self)
(add-function-in-menu #'sqrt "sqrt" 'reexpression 'self)
;;(add-function-in-menu #'trunc "trunc" 'reexpression 'self)
;;(add-function-in-menu #'unary-minus "unary minus" 'reexpression 'self)
(add-function-in-menu #'asin "asin" 'reexpression 'self)
(add-function-in-menu #'atan "atan" 'reexpression 'self)
(add-function-in-menu #'ceiling "ceiling" 'reexpression 'self)
(add-function-in-menu #'cos "cos" 'reexpression 'self)
(add-function-in-menu #'add-analysis-node "add analysis node" '|Widen this view|
                      'self '|new analysis node|)


