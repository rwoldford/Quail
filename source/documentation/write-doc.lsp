;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                                write-doc.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (C) 1991 Statistical Computing Laboratory, University Of Waterloo
;;;
;;;
;;;  Authors:
;;;     M.E. Lewis 1991.
;;;     R.W. Oldford 1991.
;;;
;;;
;;;----------------------------------------------------------------------------


(in-package :quail-kernel)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(write-doc)))        



;;;-----------------------------------------------------------------------------
;;;
;;;  The two driving functions write-doc and get-doc-form
;;;
;;;-----------------------------------------------------------------------------

(defgeneric write-doc (destination documentation-object)
  (:documentation "Writes a form to destination which, when evaluated, sets the ~
                   Common Lisp documentation of a symbol ~
                   to correspond to the Quail documentation."))

(defgeneric get-doc-form (documentation-object)
  (:documentation "Returns a form which, when evaluated, sets the ~
                   common lisp documentation of a symbol ~
                   to correspond to the quail documentation."))


;;;-----------------------------------------------------------------------------
;;;
;;;  Methods for write-doc  ... a single one should suffice.
;;;
;;;-----------------------------------------------------------------------------

(defmethod write-doc (destination (q-d documentation-object))
  (pprint (get-doc-form q-d) destination))



;;;-----------------------------------------------------------------------------
;;;
;;;  Methods for get-doc-form
;;;                      ... specialized for each kind of documentation-object.
;;;
;;;-----------------------------------------------------------------------------




;;;---------------------------------------------------------------------------
;;;
;;;  Quail documentation
;;;
;;;---------------------------------------------------------------------------


(defmethod get-doc-form ((q-d documentation-object))
  (with-accessors 
    ((name name)
     (package package)
     (symbol doc-symbol)
     (doc-capsule doc-capsule)
     (doc-elaboration doc-elaboration)
     (examples examples)
     (references references)
     (see-also see-also)
     (super-topics super-topics))
    q-d
    `(make-instance 'documentation-object
       :name ,name
       :package ,package
       :symbol ',symbol
       :doc-capsule ,doc-capsule
       :doc-elaboration ,doc-elaboration
       :examples ',examples
       :references ,references
       :see-also ',see-also
       :super-topics ',super-topics)))




;;;---------------------------------------------------------------------------
;;;
;;;  Built in class documentation
;;;
;;;---------------------------------------------------------------------------


(defmethod get-doc-form ((bic-d built-in-class-documentation))
  (with-accessors 
    ((name name)
     (package package)
     (symbol doc-symbol)
     (doc-capsule doc-capsule)
     (doc-elaboration doc-elaboration)
     (examples examples)
     (references references)
     (see-also see-also)
     (super-topics super-topics))
    bic-d
    `(make-instance 'built-in-class-documentation
       :name ,name
       :package ,package
       :symbol ',symbol
       :doc-capsule ,doc-capsule
       :doc-elaboration ,doc-elaboration
       :examples ',examples
       :references ,references
       :see-also ',see-also
       :super-topics ',super-topics)))




;;;---------------------------------------------------------------------------
;;;
;;;  Structure documentation
;;;
;;;---------------------------------------------------------------------------


(defmethod get-doc-form ((s-d structure-documentation))
  (with-accessors 
    ((name name)
     (package package)
     (symbol doc-symbol)
     (doc-capsule doc-capsule)
     (doc-elaboration doc-elaboration)
     (examples examples)
     (references references)
     (see-also see-also)
     (super-topics super-topics))
    s-d
    `(make-instance 'structure-documentation
       :name ,name
       :package ,package
       :symbol ',symbol
       :doc-capsule ,doc-capsule
       :doc-elaboration ,doc-elaboration
       :examples ',examples
       :references ,references
       :see-also ',see-also
       :super-topics ',super-topics)))


;;;---------------------------------------------------------------------------
;;;
;;;  Constant documentation
;;;
;;;---------------------------------------------------------------------------


(defmethod get-doc-form ((c-d constant-documentation))
  (with-accessors 
    ((name name)
     (package package)
     (symbol doc-symbol)
     (doc-capsule doc-capsule)
     (doc-elaboration doc-elaboration)
     (examples examples)
     (references references)
     (see-also see-also)
     (super-topics super-topics)
     (value value))
    c-d
    `(make-instance 'constant-documentation
       :name ,name
       :package ,package
       :symbol ',symbol
       :doc-capsule ,doc-capsule
       :doc-elaboration ,doc-elaboration
       :examples ',examples
       :references ,references
       :see-also ',see-also
       :super-topics ',super-topics
       :value ',value)))


;;;---------------------------------------------------------------------------
;;;
;;;  Variable documentation
;;;
;;;---------------------------------------------------------------------------

(defmethod get-doc-form ((v-d variable-documentation))
  (with-accessors 
    ((name name)
     (package package)
     (symbol doc-symbol)
     (doc-capsule doc-capsule)
     (doc-elaboration doc-elaboration)
     (examples examples)
     (references references)
     (see-also see-also)
     (super-topics super-topics)
     (value value))
    v-d
    `(make-instance 'variable-documentation
       :name ,name
       :package ,package
       :symbol ',symbol
       :doc-capsule ,doc-capsule
       :doc-elaboration ,doc-elaboration
       :examples ',examples
       :references ,references
       :see-also ',see-also
       :super-topics ',super-topics
       :value ',value)))



;;;---------------------------------------------------------------------------
;;;
;;;  Parameter documentation
;;;
;;;---------------------------------------------------------------------------

(defmethod get-doc-form ((p-d parameter-documentation))
  (with-accessors 
    ((name name)
     (package package)
     (symbol doc-symbol)
     (doc-capsule doc-capsule)
     (doc-elaboration doc-elaboration)
     (examples examples)
     (references references)
     (see-also see-also)
     (super-topics super-topics)
     (value value))
    p-d
    `(make-instance 'parameter-documentation
       :name ,name
       :package ,package
       :symbol ',symbol
       :doc-capsule ,doc-capsule
       :doc-elaboration ,doc-elaboration
       :examples ',examples
       :references ,references
       :see-also ',see-also
       :super-topics ',super-topics
       :value ',value)))



;;;---------------------------------------------------------------------------
;;;
;;;  Function documentation
;;;
;;;---------------------------------------------------------------------------

(defmethod get-doc-form ((f-d function-documentation))
  (with-accessors
    ((name name)
     (package package)
     (symbol doc-symbol)
     (doc-capsule doc-capsule)
     (doc-elaboration doc-elaboration)
     (examples examples)
     (references references)
     (see-also see-also)
     (super-topics super-topics)
     (lam-list lambda-list)
     (arg arguments))
    f-d
    `(setf (doc ',symbol :function)
           (make-instance 'function-documentation
             :name ,name
             :package ,package
             :symbol ',symbol
             :doc-capsule ,doc-capsule
             :doc-elaboration ,doc-elaboration
             :examples ',examples
             :references ,references
             :see-also ',see-also
             :super-topics ',super-topics
             :lambda-list ',lam-list
             :arguments
             ,(get-doc-form arg)))))



;;;---------------------------------------------------------------------------
;;;
;;;  Macro documentation
;;;
;;;---------------------------------------------------------------------------

(defmethod get-doc-form ((m-d macro-documentation))
  (with-accessors
    ((name name)
     (package package)
     (symbol doc-symbol)
     (doc-capsule doc-capsule)
     (doc-elaboration doc-elaboration)
     (examples examples)
     (references references)
     (see-also see-also)
     (super-topics super-topics)
     (lam-list lambda-list)
     (arg arguments))
    m-d
    `(setf (doc ',symbol  :macro)
           (make-instance 'macro-documentation
             :name ,name
             :package ,package
             :symbol ',symbol
             :doc-capsule ,doc-capsule
             :doc-elaboration ,doc-elaboration
             :examples ',examples
             :references ,references
             :see-also ',see-also
             :super-topics ',super-topics
             :lambda-list ',lam-list
             :arguments
             ,(get-doc-form arg)))))



;;;---------------------------------------------------------------------------
;;;
;;;  Special-form documentation
;;;
;;;---------------------------------------------------------------------------

(defmethod get-doc-form ((sf-d special-form-documentation))
  (with-accessors
    ((name name)
     (package package)
     (symbol doc-symbol)
     (doc-capsule doc-capsule)
     (doc-elaboration doc-elaboration)
     (examples examples)
     (references references)
     (see-also see-also)
     (super-topics super-topics)
     (lam-list lambda-list)
     (arg arguments))
    sf-d
    `(setf (doc ',symbol  :special-form)
           (make-instance 'special-form-documentation
             :name ,name
             :package ,package
             :symbol ',symbol
             :doc-capsule ,doc-capsule
             :doc-elaboration ,doc-elaboration
             :examples ',examples
             :references ,references
             :see-also ',see-also
             :super-topics ',super-topics
             :lambda-list ',lam-list
             :arguments
             ,(get-doc-form arg)))))



;;;---------------------------------------------------------------------------
;;;
;;;  Generic-function documentation
;;;
;;;---------------------------------------------------------------------------

(defmethod get-doc-form ((gf-d generic-function-documentation))
  (with-accessors
    ((name name)
     (package package)
     (symbol doc-symbol)
     (doc-capsule doc-capsule)
     (doc-elaboration doc-elaboration)
     (examples examples)
     (references references)
     (see-also see-also)
     (super-topics super-topics)
     (lam-list lambda-list)
     (arg arguments))
    gf-d
    `(setf (doc ',symbol  :generic-function)
           (make-instance 'generic-function-documentation
             :name ,name
             :package ,package
             :symbol ',symbol
             :doc-capsule ,doc-capsule
             :doc-elaboration ,doc-elaboration
             :examples ',examples
             :references ,references
             :see-also ',see-also
             :super-topics ',super-topics
             :lambda-list ',lam-list
             :arguments
             ,(get-doc-form arg)))))



;;;---------------------------------------------------------------------------
;;;
;;;  Method documentation
;;;
;;;---------------------------------------------------------------------------

(defmethod get-doc-form ((m-d method-documentation))
  (with-accessors
    ((name name)
     (package package)
     (symbol doc-symbol)
     (doc-capsule doc-capsule)
     (doc-elaboration doc-elaboration)
     (examples examples)
     (references references)
     (see-also see-also)
     (super-topics super-topics)
     (lam-list lambda-list)
     (arg arguments))
    m-d
    `(setf (doc ',symbol  :method)
           (make-instance 'method-documentation
             :name ,name
             :package ,package
             :symbol ',symbol
             :doc-capsule ,doc-capsule
             :doc-elaboration ,doc-elaboration
             :examples ',examples
             :references ,references
             :see-also ',see-also
             :super-topics ',super-topics
             :lambda-list ',lam-list
             :arguments
             ,(get-doc-form arg)))))

;;;---------------------------------------------------------------------------
;;;
;;;  Argument documentation
;;;
;;;---------------------------------------------------------------------------

(defmethod get-doc-form ((a-i argument-documentation))
  (with-accessors 
    ((&required &required)
     (&rest &rest)
     (&key &key)
     (&optional &optional)
     (&aux &aux)
     (&allow-other-keys &allow-other-keys))
    a-i
    `(make-instance 'argument-documentation
       :&required ',&required
       :&rest ',&rest
       :&key ',&key
       :&optional ',&optional
       :&aux ',&aux
       :&allow-other-keys ',&allow-other-keys)))



;;;---------------------------------------------------------------------------
;;;
;;;  Class documentation
;;;
;;;---------------------------------------------------------------------------

(defmethod get-doc-form ((c-d class-documentation))
  (with-accessors
    ((name                    name)
     (package                 package)
     (symbol                  doc-symbol)
     (doc-capsule             doc-capsule)
     (doc-elaboration         doc-elaboration)
     (examples                examples)
     (references              references)
     (see-also                see-also)
     (super-topics            super-topics)
     (class-object            doc-class-object)
     (super-classes           doc-super-classes)
     (class-precedence-list   doc-class-precedence-list)
     (sub-classes             doc-sub-classes)
     (accessors               doc-accessors)
     (slots                   doc-slots)
     )
    c-d
    `(setf (doc ',symbol  :class)
           (make-instance 'class-documentation
             :name ,name
             :package ,package
             :symbol ',symbol
             :doc-capsule ,doc-capsule
             :doc-elaboration ,doc-elaboration
             :examples ',examples
             :references ,references
             :see-also ',see-also
             :super-topics ',super-topics
             :class (find-class ',(class-name class-object))
             :super-classes
             ,(let (result)
                (setf result
                      (loop for
                            class in super-classes
                            collect
                            (list 'find-class
                                  (list 'quote 
                                        (class-name class)))))
                (if result (push 'list result))
                result)
             :class-precedence-list
             ,(let (result)
                (setf result
                      (loop for
                            class in class-precedence-list
                            collect
                            (list 'find-class
                                  (list 'quote 
                                        (class-name class)))))
                (if result (push 'list result))
                result)
             :sub-classes 
             ,(let (result)
                (setf result
                      (loop for
                            class in sub-classes
                            collect
                            (list 'find-class
                                  (list 'quote 
                                        (class-name class)))))
                (if result (push 'list result))
                result)
             :accessors ',accessors
             :slots ,(get-doc-form slots)

             )
           )
    
    )
  )


;;;---------------------------------------------------------------------------
;;;
;;;  Slots documentation
;;;
;;;---------------------------------------------------------------------------

(defmethod get-doc-form ((ss-d slots-documentation))
  (with-accessors
    ((capsule       doc-capsule)
     (class         doc-class-object)
     (name          name)
     (package       package)
     (elaboration   doc-elaboration)
     (examples      examples)
     (references    references)
     (see-also      see-also)
     (super-topics super-topics)
     )
    ss-d
    `(make-slots-documentation
      (find-class ',(class-name class))
      :doc-capsule ,capsule
      :doc-elaboration ,elaboration
      :examples ',examples
      :references ,references
      :see-also ',see-also
      :super-topics ',super-topics)
    )
  )

#|

;;;---------------------------------------------------------------------------
;;;
;;;  Slot documentation
;;;
;;;---------------------------------------------------------------------------

(defmethod get-doc-form ((s-d slot-documentation))
  (with-accessors
    ((name name)
     (package package)
     (symbol doc-symbol)
     (doc-capsule doc-capsule)
     (doc-elaboration doc-elaboration)
     (examples examples)
     (references references)
     (see-also see-also)
     (super-topics super-topics)
     (initarg slot-initargs)
     (initform slot-initform)
     (reader slot-readers)
     (writer slot-writers)  
     (type slot-type))
    s-d
    `(setf (doc ',symbol  :slot)
           (make-instance 'slot-documentation
             :name ,name
             :package ,package
             :symbol ',symbol
             :doc-capsule ,doc-capsule
             :doc-elaboration ,doc-elaboration
             :examples ',examples
             :references ,references
             :see-also ',see-also
             :super-topics ',super-topics
             :initargs ,initarg
             :initform ,initform
             :readers ,reader
             :writers ,writer
             :type ,type))))

|#

;;;---------------------------------------------------------------------------
;;;
;;;  Topic documentation
;;;
;;;---------------------------------------------------------------------------

(defmethod get-doc-form ((t-d topic-documentation))
  (with-accessors 
    ((name name)
     (package package)
     (symbol doc-symbol)
     (doc-capsule doc-capsule)
     (doc-elaboration doc-elaboration)
     (examples examples)
     (references references)
     (see-also see-also)
     (super-topics super-topics)
     (sub-topics sub-topics))
    t-d
    `(make-instance 'topic-documentation
       :name ,name
       :package ,package
       :symbol ',symbol
       :doc-capsule ,doc-capsule
       :doc-elaboration ,doc-elaboration
       :examples ',examples
       :references ,references
       :see-also ',see-also
       :super-topics ',super-topics
       :sub-topics ',sub-topics
       )))


;;;---------------------------------------------------------------------------
;;;
;;;  Package documentation
;;;
;;;---------------------------------------------------------------------------

(defmethod get-doc-form ((p-d package-documentation))
  (with-accessors 
    ((name name)
     (package package)
     (symbol doc-symbol)
     (doc-capsule doc-capsule)
     (doc-elaboration doc-elaboration)
     (examples examples)
     (references references)
     (see-also see-also)
     (sub-topics sub-topics)
     (super-topics super-topics))
    p-d
    `(make-instance 'package-documentation
       :name ,name
       :package ,package
       :symbol ',symbol
       :doc-capsule ,doc-capsule
       :doc-elaboration ,doc-elaboration
       :examples ',examples
       :references ,references
       :see-also ',see-also
       :super-topics ',super-topics
       :sub-topics ',sub-topics)))
