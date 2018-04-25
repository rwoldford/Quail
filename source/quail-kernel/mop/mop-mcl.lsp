;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               mop-mcl.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1988-1991 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     G. Desvignes 1988,1989
;;;     R.W. Oldford 1988-1991
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
;;; Release notes for MCL 2.0 final say:
;;;
;;;    CLOS includes the following introspection capabilities:
;;;      class-direct-subclasses
;;;      class-direct-superclasses
;;;      class-precedence-list
;;;      class-prototype
;;;      class-direct-instance-slots
;;;      class-direct-class-slots
;;;      class-instance-slots
;;;      class-class-slots
;;;      specializer-direct-methods
;;;      specializer-direct-generic-functions
;;;      generic-function-methods
;;;      method-function
;;;      method-generic-function
;;;      method-name
;;;      method-qualifiers
;;;      method-specializers
;;;      slot-definition-name
;;;      copy-instance
;;;      method-exists-p
;;;
;;;  The above actually reside in the :ccl package.

(in-package :quail-kernel)

#|
;;; NOT IN CCL YET !!!!!!!!!!! ... are there ways of getting at these with
;;;                                the other primitives ??  ... dga

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(ccl::slot-definition-initargs
            ccl::slot-definition-initform
            ccl::slot-definition-readers
            ccl::slot-definition-writers 
            ccl::slot-definition-type)))
|#

;;  We're breaking the rules by putting import before export, so we
;;  have to be careful and use eval-when.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(ccl:class-direct-subclasses
            ccl:class-direct-superclasses
            ccl:class-precedence-list
            ccl:class-prototype
            ccl:class-direct-instance-slots
            ccl:class-direct-class-slots
            ccl:class-instance-slots
            ccl:class-class-slots
            ccl:specializer-direct-methods
            ccl:specializer-direct-generic-functions
            ccl:generic-function-methods
            ccl:method-function
            ccl:method-generic-function
            ccl:method-name
            ccl::method-qualifiers
            ccl:method-specializers
            ccl:slot-definition-name
            ccl:copy-instance
            ccl:method-exists-p))
  )

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(class-direct-subclasses
          class-direct-superclasses
          class-precedence-list
          class-prototype
          class-direct-instance-slots
          class-direct-class-slots
          class-instance-slots
          class-class-slots
          specializer-direct-methods
          specializer-direct-generic-functions
          generic-function-methods
          method-function
          method-generic-function
          method-name
          method-qualifiers
          method-specializers
          slot-definition-name
          copy-instance
          method-exists-p)))

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(class-p
          direct-slots
          class-slots
          generic-function-p
          reader-method-p
          writer-method-p)))

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(collect-methods
          collect-accessors
          collect-slot-definitions
          slot-definition
          slot-definition-readers
          slot-definition-writers)))

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


(defun class-p (arg)
  "Return T if argument is a class."
  (typep  arg 'standard-class))

(defun generic-function-p (arg)
  "Return T if argument is a generic-function."
  (typep  arg 'generic-function))


(defun mapappend (fun &rest args)
  "Like mapcar except the results are appended together."
  (if (some #'null args)
    ()
    (append (apply fun (mapcar #'car args))
            (apply #'mapappend fun (mapcar #'cdr args)))))

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
  
  (let ((methods (generic-function-methods 
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
    result));;;
;;;
;;;

;;;  Because no slot-definition class exists in MCL, we invent one here.
;;;
 
;;;(for EFFECTIVE-SLOT-DEFINITION metaobjects:)
;;;slot-definition-location
 

(defclass slot-definition (ccl::metaobject)
  ((class :initarg :class
          :accessor doc-class-object
          :initform NIL
          :documentation "The class for which this slot is defined.")
   (name :initarg :name
         :accessor slot-definition-name
         :initform NIL
         :documentation "The name of the slot.")
   (allocation :accessor slot-definition-allocation
               :initarg :allocation
               :initform NIL
               :documentation "The allocation of the slot.")
   (initform :accessor slot-definition-initform
             :initarg :initform
             :documentation "The initialization form, if any, of the slot. ~
                             Note that this is stored in an implementation ~
                             specific way!")
   (initargs :accessor slot-definition-initargs
            :initarg :initargs
            :initform NIL
            :documentation "The initialization arguments, if any, for the slot.")
   (initfunction :accessor slot-definition-initfunction
            :initarg :initfunction
            :initform NIL
            :documentation "The initialization function, if any, for the slot.")
   (type :accessor slot-definition-type
           :initarg :type
           :initform NIL
           :documentation "The type for the slot.")
   (readers :accessor slot-definition-readers
           :initarg :readers
           :initform NIL
           :documentation "The reader methods, if any, for the slot.")
   (writers :accessor slot-definition-writers
           :initarg :writers
           :initform NIL
           :documentation "The writer methods, if any, for the slot.")
   (documentation :initarg :documentation
                  :initform NIL
                  :documentation "The documentation string, if any, ~
                                  for the slot.")
   )
  (:documentation "Information on the named slot"))

(defmethod documentation ((thing slot-definition) &optional doc-type)
  (declare (ignore doc-type))
  (slot-value thing 'documentation))

(defmethod (setf documentation) (new-value (thing slot-definition)
                                           &optional doc-type)
  (declare (ignore doc-type))
  (setf (slot-value thing 'documentation) new-value))
  

(defun collect-accessors (class)
  "Collects all the accessor methods associated with the class class. ~
   Returns NIL if none are found for that class.  ~
   Those found are returned organized as an association list whose keys are ~
   either \:readers or \:writers. ~
   The value associated with each key is a list of lists of reader or ~
   writer methods respectively for all slots of class.  The first element ~
   in each value list is the list of method qualifiers; ~
   The remaining elements are the method-objects themselves."
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
    
    (cond
     ((and reader-list writer-list)
      (list
       (cons :readers reader-list)
       (cons :writers writer-list)))
     (reader-list
      (list (cons :readers reader-list)))
     (writer-list
      (list (cons :writers writer-list)))
     (T NIL))
    )
  )



(defun collect-slot-definitions (class)
  "Collects all the slot-definitions associated with the class class, ~
   organizes these into an association list whose keys are one of ~
   :class-slots :instance-slots :direct-class-slots and :direct-instance-slots. ~
   The value associated with each key is a list of slot-definitions of that type ~
   for this class."
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
           (loop
             for slot in class-slots
             collect
             (let ((slot-def
                    (make-instance 'slot-definition
                      :class class
                      :name (first slot)
                      :initform (second slot)
                      :initargs (third slot))))
               slot-def))))
    (setf instance-slots
          (cons
           :instance-slots
           (loop
             for slot in instance-slots
             collect
             (let ((slot-def
                    (make-instance 'slot-definition
                      :class class
                      :name (first slot)
                      :initform (second slot)
                      :initargs (third slot))))
               slot-def))))
    (setf direct-class-slots
          (cons
           :direct-class-slots
           (loop
             for slot in direct-class-slots
             collect
             (let ((slot-def
                    (make-instance 'slot-definition
                      :class class
                      :name (first slot)
                      :initform (second slot)
                      :initargs (third slot))))
               slot-def))))
    (setf direct-instance-slots
          (cons
           :direct-instance-slots
           (loop
             for slot in direct-instance-slots
             collect
             (let ((slot-def
                    (make-instance 'slot-definition
                      :class class
                      :name (first slot)
                      :initform (second slot)
                      :initargs (third slot))))
               slot-def))))
    
    (list class-slots
          instance-slots
          direct-class-slots
          direct-instance-slots)))


(defmethod slot-definition-readers ((thing standard-class))
  (let ((direct-methods (specializer-direct-methods (class-of thing)))
        )
    (loop for method in direct-methods
          when (typep method 'ccl::standard-reader-method)
          collect
          method)))

(defmethod slot-definition-writers ((thing standard-class))
  (let ((direct-methods (specializer-direct-methods (class-of thing)))
        )
    (loop for method in direct-methods
          when (typep method 'ccl::standard-writer-method)
          collect
          method)))
