;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                           doc-type.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; copyright (c) 1991 statistical computing laboratory, university of waterloo
;;;
;;;
;;;  authors:
;;;     m.e. lewis 1991.
;;;     r.w. oldford 1991, 1992
;;;
;;;
;;;----------------------------------------------------------------------------


(in-package :quail-kernel)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(quail-doc-types quail-doc-classes doc-type-p)))

;;;---------------------------------------------------------------------------
;;;
;;; Dictionary functions, for looking up documentation types and classes
;;; based on Common Lisp types.
;;;
;;;---------------------------------------------------------------------------

(defvar *quail-doc-type-to-cl-type* 
  NIL "A variable to store the translation table.")

(defun cl-type (type)
  (unless *quail-doc-type-to-cl-type* (init-quail-doc-type-to-cl-type))
  (gethash type *quail-doc-type-to-cl-type*))

(defun init-quail-doc-type-to-cl-type ()
  (setf *quail-doc-type-to-cl-type*
        (make-hash-table :test #'equal :size 11))
  (dolist (type '(:constant
                  :variable
                  :parameter))
    (set-quail-to-cl-doc-type type 'variable)
    )
  (dolist (type '(:function
                  :special-form
                  :generic-function
                  :method
                  :macro))
    (set-quail-to-cl-doc-type type 'function)
    )
  (dolist (type '(:built-in-class
                  :structure
                  :class))
    (set-quail-to-cl-doc-type type 'type)
    )
  *quail-doc-type-to-cl-type*)



(defun set-quail-to-cl-doc-type (quail-doc-type cl-type)
  "Sets quail-doc-type to cl-type in the Quail documentation type ~
   to Common Lisp doc-type to the translation table."
  (unless *quail-doc-type-to-cl-type* (init-quail-doc-type-to-cl-type))
  (setf (gethash quail-doc-type *quail-doc-type-to-cl-type*)
        cl-type)
  )

;;;
;;;
;;;

(defvar *quail-doc-type-to-doc-class* 
  NIL "A variable to store the translation table.")

(defun quail-doc-class (type)
  (unless *quail-doc-type-to-doc-class* (init-doc-type-to-doc-class))
  (gethash type *quail-doc-type-to-doc-class*)
  )

(defun init-doc-type-to-doc-class ()
  (setf *quail-doc-type-to-doc-class*
        (make-hash-table :test #'equal :size 15))
  (set-doc-type-to-doc-class :constant 'constant-documentation)
  (set-doc-type-to-doc-class :variable 'variable-documentation)
  (set-doc-type-to-doc-class :parameter 'parameter-documentation)
  (set-doc-type-to-doc-class :function 'function-documentation)
  (set-doc-type-to-doc-class :special-form 'special-form-documentation)
  (set-doc-type-to-doc-class :macro 'macro-documentation)
  (set-doc-type-to-doc-class :generic-function 'generic-function-documentation)
  (set-doc-type-to-doc-class :method 'method-documentation)
  (set-doc-type-to-doc-class :methods 'methods-documentation)
  (set-doc-type-to-doc-class :accessor 'accessor-documentation)
  (set-doc-type-to-doc-class :accessors 'accessors-documentation)
  (set-doc-type-to-doc-class :built-in-class 'built-in-class-documentation)
  (set-doc-type-to-doc-class :structure 'structure-documentation)
  (set-doc-type-to-doc-class :class 'class-documentation)
  (set-doc-type-to-doc-class :package 'package-documentation)
  (set-doc-type-to-doc-class :topic 'topic-documentation)
  (set-doc-type-to-doc-class :slot 'slot-documentation)
  *quail-doc-type-to-doc-class*)


(defun set-doc-type-to-doc-class (doc-type doc-class)
  "Sets doc-type to correspond to doc-class in the Quail documentation type ~
   to Quail documentation-object translation."
  (unless *quail-doc-type-to-doc-class* (init-doc-type-to-doc-class))
  (setf (gethash doc-type *quail-doc-type-to-doc-class*)
        doc-class)
  )


(defun quail-doc-types ()
  "Returns a list of Quail doc types that are currently defined ~
   and to which there correspond classes of Quail documentation objects.  ~
   (:see-also quail-doc-classes)  ~
   (:examples ~
   (:files ~
   (Starting documentation ~
   q:Examples;Documentation;documentation-example.lisp) ~
   (Extending the documentation system ~
   q:Examples;Documentation;defining-new-doc-objects.lisp) ~
   ))"
  (unless *quail-doc-type-to-doc-class* (init-doc-type-to-doc-class))
  (let ((result NIL))
    (with-hash-table-iterator
      (next-doc-class *quail-doc-type-to-doc-class*)
      (labels ((try (got-one? &optional key value)
                 (declare (ignore value))
                 (when got-one?
                   (push key result)
                   (multiple-value-call #'try (next-doc-class)))))
        (multiple-value-call #'try (next-doc-class))))
    result))


(defun quail-doc-classes ()
  "Returns a list of class names for the currently defined ~
   documentation objects that correspond to Quail doc-types.  ~
   (:see-also quail-doc-types)."
  (unless *quail-doc-type-to-doc-class* (init-doc-type-to-doc-class))
  (let ((result NIL)
        )
    (with-hash-table-iterator
      (next-doc-class *quail-doc-type-to-doc-class*)
      (labels ((try (got-one? &optional key value)
                 (declare (ignore key))
                 (when got-one?
                   (push value result)
                   (multiple-value-call #'try (next-doc-class)))))
        (multiple-value-call #'try (next-doc-class))))
    result))


(defun doc-type-p (thing)
  "Test whether the argument is a supported documentation type."
  (cond
   ((stringp thing)
    (member thing (quail-doc-types) :test #'equalp))
   (T (member thing (quail-doc-types) :test #'eq))))
  
(defgeneric doc-type (thing)
  (:documentation "Returns the keyword representing the documentation type ~
                   of the argument if available."))

(defmethod doc-type ((thing T))
  (missing-method 'doc-type thing))

(defmethod doc-type ((thing list))
  (cond ((listp (cdr thing)) (cadr thing))
        (T (cdr thing))))

(defmethod doc-type ((thing variable-documentation))
  :variable)

(defmethod doc-type ((thing built-in-class-documentation))
  :built-in-class)

(defmethod doc-type ((thing constant-documentation))
  :constant)

(defmethod doc-type ((thing parameter-documentation))
  :parameter)

(defmethod doc-type ((thing procedure-documentation))
  :procedure)

(defmethod doc-type ((thing special-form-documentation))
  :special-form)

(defmethod doc-type ((thing function-documentation))
  :function)

(defmethod doc-type ((thing macro-documentation))
  :macro)

(defmethod doc-type ((thing generic-function-documentation))
  :generic-function)

(defmethod doc-type ((thing method-documentation))
  :method)

(defmethod doc-type ((thing argument-documentation))
  :argument)

(defmethod doc-type ((thing class-documentation))
  :class)

(defmethod doc-type ((thing slot-documentation))
  :slot)

(defmethod doc-type ((thing dataset-documentation))
  :dataset)

(defmethod doc-type ((thing topic-documentation))
  :topic)

(defmethod doc-type ((thing accessor-documentation))
   :accessor)

(defmethod doc-type ((thing accessors-documentation))
  :accessors)
