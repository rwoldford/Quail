;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                           documentation.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (C) 1991 Statistical Computing Laboratory, University Of Waterloo
;;;
;;;
;;;  Authors:
;;;     M.E. Lewis 1991.
;;;     R.W. Oldford 1991-94
;;;
;;;
;;;----------------------------------------------------------------------------


(in-package :quail-kernel)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(documentation-object)))



;;;-----------------------------------------------------------------------------
;;;
;;;  General class: documentation-object
;;;
;;;-----------------------------------------------------------------------------


(defclass documentation-object ()
  ((name :accessor name 
         :initarg :name 
         :initform NIL)
   (package :accessor package
            :initarg :package
            :initform NIL)
   (symbol :accessor doc-symbol
            :initarg :symbol
            :initform NIL)
   (doc-capsule :accessor doc-capsule 
                :initarg :doc-capsule 
                :initform NIL)
   (doc-elaboration :accessor doc-elaboration 
                    :initarg :doc-elaboration 
                    :initform NIL)
   (examples :accessor examples 
             :initarg :examples 
             :initform NIL)
   (references :accessor references 
               :initarg :references 
               :initform NIL)
   (see-also :accessor see-also 
             :initarg :see-also 
             :initform NIL)
   (super-topics :accessor super-topics 
                 :initarg :super-topics 
                 :initform NIL)
   (annote :accessor annote
           :initarg :annote
           :initform nil))
  (:documentation "An object to store documentation."))

;;;------------------------------
;;;
;;;  Here we extend the "name" method to return a simple string in general,
;;;  and the first item in a list if the argument is a list.
;;;  This is useful for topics and subtopics.  It doesn't force us to 
;;;  establish all documentation objects that could reside on sub-topic
;;;  and super-topic lists.

(defmethod name ((arg T))
  "Returns the argument as a string in general."
  (string arg))

(defmethod name ((arg list))
  "Returns the first item in the list argument as a string.  This is useful for ~
   topics."
  (string (car arg)))


;;;-----------------------------------------------------------------------------
;;;
;;;  Variable documentation
;;;
;;;-----------------------------------------------------------------------------


(defclass variable-documentation (documentation-object)
  ((value :accessor value :initarg :value :initform NIL))
  (:documentation "Documentation for defined special variables.  ~
                   That is those defined with defvar."))

;;;-----------------------------------------------------------------------------
;;;
;;;  Built-in-class documentation
;;;
;;;-----------------------------------------------------------------------------


(defclass built-in-class-documentation (documentation-object)
  ()
  (:documentation "Documentation for Common Lisp built in classes."))



;;;-----------------------------------------------------------------------------
;;;
;;;  Structure documentation
;;;
;;;-----------------------------------------------------------------------------


(defclass structure-documentation (documentation-object)
  ()
  (:documentation "Documentation for Common Lisp structures.  ~
                   That is those defined with defstruct."))



;;;-----------------------------------------------------------------------------
;;;
;;;  Constant documentation
;;;
;;;-----------------------------------------------------------------------------


(defclass constant-documentation (variable-documentation)
  ()
  (:documentation "Documentation for defined constants.  ~
                   That is those defined with defconstant."))



;;;-----------------------------------------------------------------------------
;;;
;;;  Parameter documentation
;;;
;;;-----------------------------------------------------------------------------


(defclass parameter-documentation (variable-documentation)
  ()
  (:documentation "Documentation for defined parameters.  ~
                   That is those defined with defparameter."))


;;;-----------------------------------------------------------------------------
;;;
;;;  Procedure-documentation
;;;
;;;-----------------------------------------------------------------------------

(defclass procedure-documentation (documentation-object)
  ((lambda-list :accessor lambda-list 
                :initarg :lambda-list 
                :initform NIL)
   (arguments :accessor arguments
              :initarg :arguments 
              :initform NIL)
   (returns :accessor returns 
            :initarg :returns
            :initform NIL)
   (side-effects :accessor side-effects
                 :initarg :side-effects 
                 :initform NIL))
  (:documentation
   "A class that contains information on ~
    the named procedure."))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Special Forms
;;;

(defclass special-form-documentation (procedure-documentation)
  ()
  (:documentation
   "A class that contains information on the named special-form."))




;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Functions
;;;

(defclass function-documentation (procedure-documentation)
  ()
  (:documentation
   "A class that contains information on the named function."))



;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Macros
;;;


(defclass macro-documentation (procedure-documentation)
  ()
  (:documentation
   "A class that contains information on the named macro."))



;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Generic Functions
;;;  and Methods
;;;

(defclass generic-function-documentation (procedure-documentation)
  ((method-list
    :initform NIL
    :accessor method-list
    :initarg :method-list
    :documentation "An association list of pairs each of whose key is a list ~
                    of method-qualifiers and whose value is a ~
                    list of method objects."))
  (:documentation
   "A class that contains information on the named generic-function."))

(defclass method-documentation (procedure-documentation)
  ((generic-function
    :initform NIL
    :accessor doc-generic-function
    :initarg :generic-function
    :documentation "The generic function associated with these ~
                    method objects.")
   (method-object
    :initform NIL
    :accessor doc-method-object
    :initarg :method
    :documentation "The actual method object being documented.")
   (specializers
    :initform NIL
    :accessor doc-method-specializers 
    :initarg :specializers
    :documentation "A list of the names of the specializers of this method.")
   (qualifiers
    :initform NIL
    :accessor doc-method-qualifiers
    :initarg :qualifiers
    :documentation "A list of the names of the qualifiers of this method.")
   )
  (:documentation
   "A class that contains information on the named method."))


(defclass methods-documentation (generic-function-documentation)
  ((generic-function
    :initform NIL
    :accessor doc-generic-function
    :initarg :generic-function
    :documentation "The generic function associated with these ~
                    method objects."))
  (:documentation
   "A class that contains information on a collection of methods for ~
    a single generic function."))



;;;---------------------------------------------------------------------------
;;;
;;;  Lambda list arguments
;;;
;;;---------------------------------------------------------------------------

(defclass argument-documentation ()
  ((&required :accessor &required
              :initarg :&required 
              :initform NIL)
   (&optional :accessor &optional 
              :initarg :&optional 
              :initform NIL)
   (&rest :accessor &rest
          :initarg :&rest
          :initform NIL)
   (&aux :accessor &aux
         :initarg :&aux 
         :initform  NIL)
   (&key :accessor &key 
         :initarg :&key 
         :initform NIL)
   (&body :accessor &body 
          :initarg :&body 
          :initform NIL)
   (&allow-other-keys :accessor &allow-other-keys
                      :initarg :&allow-other-keys 
                      :initform NIL)
   )
(:documentation
   "A place to hold documentation for the arguments of a procedure."))

(defun arg-info-p (arg-doc)
  (with-accessors 
    ((&required qk::&required)
     (&rest qk::&rest)
     (&key qk::&key)
     (&optional qk::&optional)
     (&aux qk::&aux)
     (&body qk::&body)
     (&allow-other-keys qk::&allow-other-keys))
    
    arg-doc
    (if
      (or &required &rest &key &body
          &optional &aux &allow-other-keys)
      (let* ((arg-lists (list &required &rest &key &body
                              &optional &aux &allow-other-keys))
             (n (- (length arg-lists) 1)))
        (do*  ((i 0 (+ i 1))
               (items (elt arg-lists i)
                      (elt arg-lists i))
               (answer NIL))
              ((or answer (= i n)) answer)
          (loop for item in items
                do
                (if (cdr item) (setf answer T))))))
    ))

;;;---------------------------------------------------------------------------
;;;
;;;  Classes, Accessors, and slots
;;;
;;;---------------------------------------------------------------------------

(defclass class-documentation (documentation-object)
  ((class :initarg :class
          :accessor doc-class-object
          :initform NIL
          :documentation "The documented class.")
   (supers :accessor doc-super-classes 
           :initarg :super-classes 
           :initform NIL
           :documentation "The super-classes of the documented class.")
   (class-precedence-list :initarg :class-precedence-list 
                          :initform NIL
                          :accessor doc-class-precedence-list
                          :documentation
                          "The class-precedence-list of the documented class, ~
                           from the class back through its ancestors in order ~
                           of precedence.")
   (subs   :accessor doc-sub-classes 
           :initarg :sub-classes 
           :initform NIL
           :documentation "The sub-classes of the documented class.")
   (slots :accessor doc-slots
          :initarg :slots
          :initform NIL
          :documentation "The slot-definitions of the documented class.")
   (accessors :accessor doc-accessors
              :initarg :accessors
              :initform NIL
              :documentation
              "A list of two elements ~
               containing the reader and writer methods defined for all slots ~
               for this class.  ~
               The first element of this list is a list whose first element is ~
               :reader and whose remaining elements are the reader-methods for ~
               all slots on the documented class.  ~
               Similarily, the second element is a list whose first element is ~
               :writer and its remaining elements are the writer-methods for all ~
               slots the documented class.  ~
               ")
   )
  (:documentation
   "A place to hold documentation on a class."))

;;;
;;;  Accessors-documentation
;;;

(defclass accessors-documentation (documentation-object)
  ((class :initarg :class
          :accessor doc-class-object
          :initform NIL
          :documentation "The class for which these accessors were defined.")
   (accessors :accessor doc-accessors
           :initarg :accessors
           :initform NIL
           :documentation "The accessors methods, if any, defined for the class.  ~
                           (:see-also collect-accessors)")
   #|
   (group? :initarg :group?
           :accessor doc-group-accessors-p
           :initform NIL
           :documentation "A display variable to indicate whether the ~
                           accessor methods should be grouped when displayed.  ~
                           grouping is by element in the accessors list.  No ~
                           grouping means all the accessor methods are displayed ~
                           in a single list.")
   |#
   )
  (:documentation
   "A class that contains information on a collection of accessors for ~
    a single class.  (:see-also collect-accessors)"))

(defclass accessor-documentation (procedure-documentation)
  ((class
    :initarg :class
    :accessor doc-class-object
    :initform NIL
    :documentation "The class for which this accessor was defined.")
   (accessor-object
    :initform NIL
    :accessor doc-accessor-object
    :initarg :accessor
    :documentation "The actual accessor method-object being documented.")
   (type
    :initform NIL
    :accessor doc-accessor-type
    :initarg :accessor
    :documentation "The accessor type, \:reader or \:writer.")
   )
  (:documentation
   "A class that contains information on the named accessor."))

;;;
;;;  Slot-documentation
;;;

(defclass slot-documentation (documentation-object)
  ((class :initarg :class
          :accessor doc-class-object
          :initform NIL
          :documentation "The class for which the documentation was requested.")
   (slot :initarg :slot
         :accessor doc-slot-descriptor
         :initform NIL
         :documentation "The slot-descriptor object being documented.")
    )
  (:documentation
   "A place to hold documentation on a single slot-descriptor."))

;;;
;;;  Slots-documentation
;;;

(defclass slots-documentation (documentation-object)
  ((class :initarg :class
          :accessor doc-class-object
          :initform NIL
          :documentation "The class for which these slots are defined.")
   (direct-class-slots :accessor doc-direct-class-slots
                       :initarg :direct-class-slots
                       :initform NIL)
   (direct-instance-slots :accessor doc-direct-instance-slots
                          :initarg :direct-instance-slots
                          :initform NIL)
   (class-slots :accessor doc-class-slots
                :initarg :class-slots
                :initform NIL)
   (instance-slots :accessor doc-instance-slots
                   :initarg :instance-slots
                   :initform NIL)
   )
  (:documentation
   "A class that contains information on all slots of ~
    a single class."))

;;;---------------------------------------------------------------------------
;;;
;;;  Documenting datasets
;;;
;;;---------------------------------------------------------------------------

(defclass dataset-documentation (documentation-object)
  ())

;;;---------------------------------------------------------------------------
;;;
;;;  Package-documentation.
;;;
;;;---------------------------------------------------------------------------

(defclass package-documentation (topic-documentation)
  ((external-symbol-topic
    :accessor external-symbol-topic
    :initform NIL
    :initarg :external-symbol-topic
    :documentation "A pointer to the topic recording the documentation ~
                    associated with all external symbols.")
   (shadowed-symbol-topic
    :accessor shadowed-symbol-topic
    :initform NIL
    :initarg :shadowed-symbol-topic
    :documentation "A pointer to the topic recording the documentation ~
                    associated with all symbols ~
                    that have been declared as shadowing symbols in this ~
                    package."))
  )

(defun nicknames (package-documentation)
  "Returns the list of nicknames of the package associated with the given~
   package-documentation object"
  (package-nicknames (name package-documentation)))

(defun packages-used (package-documentation)
  "Returns the list of names of packages used by the package associated with the given~
   package-documentation object"
  (loop for p in (package-use-list (name package-documentation))
        collect (package-name p)))

(defun packages-which-use (package-documentation)
  "Returns the list of names of packages which use the package associated with the given~
   package-documentation object"
  (loop for p in (package-used-by-list (name package-documentation))
        collect (package-name p)))


  
;;;---------------------------------------------------------------------------
;;;
;;;  Documentation by topic.
;;;
;;;---------------------------------------------------------------------------

(defclass topic-documentation (documentation-object)
  ((sub-topics :initform NIL :initarg :sub-topics
               :accessor sub-topics)
   )
  )
