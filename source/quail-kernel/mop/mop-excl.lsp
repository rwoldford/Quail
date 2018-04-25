;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               mop-excl.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1988-1991 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     G. Desvignes 1988,1989
;;;     R.W. Oldford 1988-1994
;;;     Greg Anglin 1992
;;;
;;;----------------------------------------------------------------------------------
;;;
;;;
;;; Meta-Object protocol interface
;;;
;;; This file imports to quail-kernel fundamental functions which I think are
;;; X3J13-proposed for the standard MOP, and exports them from quail-kernel,
;;; along with some older (perhaps redundant, now) covers for them.
;;;
;;;                 DGA ... upon arrival of MCL 2.0 final, Oct 92.
;;;
;;; The following are implemented in Allegro CL 4.1 and are imported to quail-kernel.
;;; Others are also present, but I haven't grabbed them ...
;;;
;;;      class-direct-subclasses
;;;      class-direct-superclasses
;;;      class-precedence-list
;;;      class-prototype
;;;      class-direct-slots
;;;      class-slots
;;;      specializer-direct-methods
;;;      specializer-direct-generic-functions
;;;      generic-function-methods
;;;      method-function
;;;      method-generic-function
;;;      method-specializers
;;;      slot-definition-name
;;;

(in-package :quail-kernel)

;;  We're breaking the rules by putting import before export, so we
;;  have to be careful and use eval-when.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(clos:class-direct-subclasses
            clos:class-direct-superclasses
            clos:class-precedence-list
            clos:class-prototype
            clos:class-direct-slots
            clos:class-slots
	    clos:class-direct-methods
            clos:specializer-direct-methods
            clos:specializer-direct-generic-functions
            clos:generic-function-methods
            clos:method-function
            clos:method-generic-function
            clos:method-specializers
	    clos:slot-definition-initargs
            clos:slot-definition-initform
            clos:slot-definition-readers
            clos:slot-definition-writers 
            clos:slot-definition-type
            clos:slot-definition-name
	    )))

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(class-direct-subclasses
          class-direct-superclasses
          class-precedence-list
          class-prototype
          class-direct-slots
          class-slots
	  class-direct-methods
          specializer-direct-methods
          specializer-direct-generic-functions
          generic-function-methods
          method-function
          method-generic-function
          method-specializers
	  slot-definition-initargs
          slot-definition-initform
          slot-definition-readers
          slot-definition-writers 
          slot-definition-type
          slot-definition-name
          slot-definition
	  )))

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(class-p
          direct-slots
          generic-function-p
          reader-method-p
          writer-method-p)))

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(collect-methods
          collect-accessors
          collect-slot-definitions
          )))


(defmethod slot-definition-initargs (slot-definition)
  (declare (ignore slot-definition)))

(defmethod slot-definition-initform (slot-definition) 
  (declare (ignore slot-definition)))

(defmethod slot-definition-writers (slot-definition) 
  (declare (ignore slot-definition)))

(defmethod slot-definition-readers (slot-definition) 
  (declare (ignore slot-definition)))

(defmethod slot-definition-type (slot-definition) 
  (declare (ignore slot-definition)))


;;; -----------------------------

(defun class-p (arg)
  "Return T if argument is a class."
  (typep  arg 'standard-class))

(defun generic-function-p (arg)
  "Return T if argument is a generic-function."
  (typep  arg 'generic-function))

(defun reader-method-p (method)
  "Tests whether method is a directly defined reader method."
  (let ((specializers (method-specializers method)))
    (and (= (length specializers) 1)
         (member method
                 (slot-definition-readers (first specializers))
                 :test #'eq))))

(defun writer-method-p (method)
  "Tests whether method is a directly defined writer method."
  (let ((specializers (method-specializers method)))
    (and (= (length specializers) 2)
         (member method
                 (slot-definition-readers (first specializers))
                 :test #'eq))))

(defmethod class-slots ((self structure-class))
  "Returns the list of slots of the class."
  (append (class-class-slots self)
          (class-instance-slots self)))

(defmethod class-direct-slots ((self structure-class))
  "Returns the list of slots of the class."
  (append (class-direct-class-slots self)
          (class-direct-instance-slots self)))


(defmethod class-slots ((self standard-class))
  "Returns the list of slots of the class."
  (append (class-class-slots self)
          (class-instance-slots self)))

(defmethod class-direct-slots ((self standard-class))
  "Returns the list of slots of the class."
  (append (class-direct-class-slots self)
          (class-direct-instance-slots self)))


(defmethod class-slots ((self built-in-class))
  "Returns the list of slots of the class."
  (append (class-class-slots self)
          (class-instance-slots self)))

(defmethod class-direct-slots ((self built-in-class))
  "Returns the list of slots of the class."
  (append (class-direct-class-slots self)
          (class-direct-instance-slots self)))


;;;
;;;   The following are mop specialized methods to prevent breakage
;;;   (in the documentation system anyway)  when ``classes'' like T
;;;   are encountered where it makes no sense for them to have slots etc.
;;;

(defmethod class-direct-class-slots ((Self T))
  "Call makes little sense. Returns NIL."
  ())

(defmethod class-class-slots ((Self T))
  "Call makes little sense. Returns NIL."
  ())

(defmethod class-instance-slots ((Self T))
  "Call makes little sense. Returns NIL."
  ())

(defmethod class-direct-instance-slots ((Self T))
  "Call makes little sense. Returns NIL."
  ())

(defmethod class-direct-subclasses ((Self T))
  "Call makes little sense. Returns NIL."
  ())

(defmethod class-precedence-list ((Self T))
  "Call makes little sense. Returns NIL."
  ())

(defmethod class-direct-superclasses ((Self T))
  "Call makes little sense. Returns NIL."
  ())

;;;
;;;
;;;


(defun collect-methods (fun)
  "Collects all the methods associated with the generic-function fun, ~
   organizes these into an association list whose keys are the qualifier ~
   lists for each method and whose values are the list of methods having ~
   those qualifiers.  Those methods which do not have qualifiers, i.e. ~
   whose qualifer list is NIL, will be stored with key (\:primary)."
  
  (let ((methods (clos::generic-function-methods
                  (cond
                   ((symbolp fun) (symbol-function fun))
                   (T fun))))
        (result NIL)
        qualifiers
        )
    (loop for method in methods
          do
          (setf qualifiers (or (method-qualifiers method)
                               (list :primary)))
          (if (assoc qualifiers result :test #'equal)
            (push method (cdr (assoc qualifiers result :test #'equal)))
            (setf result (acons qualifiers (list method) result))))
    result))

(defun collect-accessors (class)
  "Collects all the accessor methods associated with the class class, ~
   organizes these into an association list whose keys are either ~
   :readers or :writers. ~
   The value associated with each key is a list of lists of reader or ~
   writer methods respectively for all slots of class.  The first element ~
   in each value list is the list of method qualifiers; ~
   The remaing elements are the method-objects themselves."
  (cond
   ((symbolp class) (setf class (find-class class)))
   ((stringp class) (setf class (find-class
                                 (with-input-from-string (s class)
                                   (read s)))))
   )
  (let ((reader-methods (slot-definition-readers class))
        (writer-methods (slot-definition-writers class))
        qualifiers
        reader-list
        writer-list)
    (loop for method in reader-methods
          do
          (setf qualifiers (or (method-qualifiers method)
                               (list :primary)))
          (if (assoc qualifiers reader-list :test #'equal)
            (push method (cdr (assoc qualifiers reader-list :test #'equal)))
            (setf reader-list (acons qualifiers (list method) reader-list))))
    (loop for method in writer-methods
          do
          (setf qualifiers (or (method-qualifiers method)
                               (list :primary)))
          (if (assoc qualifiers writer-list :test #'equal)
            (push method (cdr (assoc qualifiers writer-list :test #'equal)))
            (setf writer-list (acons qualifiers (list method) writer-list))))
    
    (list
     (list :readers reader-list)
     (list :writers writer-list))
    )
  )

(defun collect-slot-definitions (class)
  "Collects all the slot-definitions associated with the class class, ~
   organizes these into an association list whose keys are one of ~
   :class-slots :instance-slots :direct-class-slots and :direct-instance-slots. ~
   The value associated with each key is a list of slot-definitions of that type ~
   for this class.   ~
   "
  (cond
   ((symbolp class) (setf class (find-class class)))
   ((stringp class) (setf class (find-class
                                 (with-input-from-string (s class)
                                   (read s)))))
   )
  (let ((direct-class-slots (class-direct-class-slots class))
        (direct-instance-slots (class-direct-instance-slots class))
        (instance-slots (class-instance-slots class))
        (class-slots (class-class-slots class))
        )
    (setf class-slots
          (cons
           :class-slots
           class-slots))
    (setf instance-slots
          (cons
           :instance-slots
           instance-slots))
    (setf direct-class-slots
          (cons
           :direct-class-slots
           direct-class-slots))
    (setf direct-instance-slots
          (cons
           :direct-instance-slots
           direct-instance-slots))
    
    (list class-slots
                  instance-slots
                  direct-class-slots
                  direct-instance-slots)))



(defmethod slot-definition-readers ((thing standard-class))
  (let ((direct-methods (specializer-direct-methods (class-of thing)))
        )
    (loop for method in direct-methods
          when (typep method 'clos::standard-reader-method)
          collect
          method)))

(defmethod slot-definition-writers ((thing standard-class))
  (let ((direct-methods (specializer-direct-methods (class-of thing)))
        )
    (loop for method in direct-methods
          when (typep method 'clos::standard-writer-method)
          collect
          method)))

