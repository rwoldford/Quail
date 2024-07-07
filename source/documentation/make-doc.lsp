;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                                make-doc.lisp                               
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

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(make-doc)))


;;;---------------------------------------------------------------------------
;;;
;;; A generic function for constructing the appropriate make-instance form for
;;; various classes of Quail documentation.
;;;
;;;---------------------------------------------------------------------------

(defgeneric make-doc (thing type)
  (:documentation "Creates and returns a quail documentation object ~
                   that is appropriate for thing of quail documentation ~
                   type type.  ~
                   (:see-also quail-doc-classes quail-doc-types)  "))

;;; Add do-nothing FEB 04 1998
;(defmethod make-doc  (thing type)
;   (declare (ignorable thing type)) ;(declare (ignore thing type)) ; 30JUL2023
;   (call-next-method))


(defmethod make-doc ((thing T) (type T))
    (make-instance (quail-doc-class type)
             :name (string-downcase (format NIL"~s" thing))
             :document 
             (interpret-quail-documentation (documenation thing 'type)) ;(documentation thing)) 05SEP2023
             )
    )

(defmethod make-doc :around (thing type)
  (if (quail-doc-class type)
    (call-next-method)
    (quail-error
        "Sorry, I don't know how to produce documentation of type ~s for ~s."
        type thing)))


(defmethod make-doc ((thing symbol) type)
  (let* ((doc-class (quail-doc-class type))
         (doc-type (cl-type type))
         (document
          (interpret-quail-documentation
           (if doc-type
             (documentation thing doc-type)
             (documentation thing 'type) ;(documentation thing) ; 05SEP2023
             )))
         )
    (make-instance doc-class
           :name (string-downcase (format NIL "~s" thing))
           :symbol thing
           :document document)
    ))

(defmethod make-doc ((thing symbol) (type (eql :package)))
  (generate-package-topics thing))

(defmethod make-doc ((thing symbol) (type (eql :methods)))
  (let* ((doc-class (quail-doc-class type))
         (doc-type (cl-type :generic-function))
         (document
          (interpret-quail-documentation
           (documentation thing doc-type)))
         )
    (make-instance doc-class
           :name (string-downcase (format NIL"~s" thing))
           :symbol thing
           :document document)
    )
  )

(defmethod make-doc ((thing symbol) (type (eql :method)))
  (declare (ignorable type)) ;(declare (ignore type)) ; 30JUL2023
  (make-doc (cons thing (collect-methods thing))
            :methods))

(defmethod make-doc ((thing list) (type (eql :method)))
  "Thing is a list whose first element is the symbol, and second ~
   element is a method object."
  (let* ((doc-class (quail-doc-class type))
         (document
          (interpret-quail-documentation (documentation (second thing))))
         )
    (make-instance doc-class
           :symbol (first thing)
           :document document
           :method (second thing)
           )
    )
  )
    

(defmethod make-doc ((thing list) (type (eql :methods)))
  "Thing is a list whose first element is the symbol, and whose ~
   remaining elements are each lists whose first element is the ~
   qualifier list and whose remaining elements are the method objects. ~
   (:see-also collect-methods)"
  (let* ((doc-class (quail-doc-class type))
         (doc-type (cl-type :generic-function))
         (document
          (interpret-quail-documentation
           (documentation (first thing) doc-type)))
         )
    (make-instance doc-class
           :symbol (first thing)
           :document document
           :method-list (rest thing))
    )
  )

    
(defmethod make-doc ((thing list) (type (eql :accessors)))
  "Thing is a list whose first element is the class, and whose ~
   remaining elements are each lists whose first element is either :readers ~
   or :writers and whose remaining elements are themselves lists of which the ~
   first elements is the qualifier list and whose remaining elements are the ~
   method objects. ~
   (:see-also collect-accessors)"
  (let* ((doc-class (quail-doc-class type))
         (doc-type (cl-type :class))
         (document
          (interpret-quail-documentation
           (concatenate 'string
                        (format NIL "Accessor methods for class: ~s.  "
                                (class-name  (first thing)))
                        (documentation (class-name  (first thing)) doc-type)))
          )
         )
    (make-instance doc-class
           :symbol (class-name (first thing))
           :class (first thing)
           :document document
           :accessors (rest thing))
    )
  )

    
(defmethod make-doc ((thing list) (type (eql :accessor)))
  "Thing is a list whose first element is the class and whose ~
   second and remaining element is the method-object that is the accessor."
  
  (let* ((doc-class (quail-doc-class type))
         (doc-type (cl-type :class))
         (document
          (interpret-quail-documentation
           (concatenate 'string
                        (format NIL "Accessor methods for class: ~s.  "
                                (class-name  (first thing)))
                        (documentation (class-name  (first thing)) doc-type)))
          )
         (accessor (second thing))
         )
    (make-instance doc-class
           :symbol (qk::method-name accessor)
           :class (first thing)
           :document document
           :accessor accessor)
    )
  )
    

;;;---------------------------------------------------------------------------
;;;
;;; All quail documentation
;;;
;;;---------------------------------------------------------------------------

(defmethod initialize-instance :after ((me documentation-object) 
                                       &key symbol document)
  (with-slots ((doc-symbol symbol)
               name package doc-capsule doc-elaboration
               examples references see-also super-topics
               annote)
              me
    (unless symbol
      (cond
       (doc-symbol (setf symbol doc-symbol))
       (name
          (if package
            (setf doc-symbol
                  (find-symbol (string-upcase name) package))
            (setf doc-symbol
                  (find-symbol (string-upcase name)))
            )
          (setf symbol doc-symbol)
          )
       ))
    (unless name (if symbol
                   (setf name (string-downcase (symbol-name symbol)))))
    (unless package (if symbol
                      (setf package (package-name (symbol-package symbol)))))
    (unless doc-capsule (setf doc-capsule (get-doc-capsule document)))
    (unless doc-elaboration (setf doc-elaboration (get-doc-elaboration document)))
    (unless examples (setf examples (get-examples document)))
    (unless references (setf references (get-references document)))
    (unless see-also (setf see-also (get-see-also document)))
    (unless super-topics (setf super-topics (get-super-topics document)))
    (if (null doc-capsule)
      (setf doc-capsule (get-narrative document))
      (setf annote (get-narrative document))))
  me)

;;;---------------------------------------------------------------------------
;;;
;;; Constants, variables and parameters
;;;
;;;---------------------------------------------------------------------------

(defmethod initialize-instance :after ((me variable-documentation)
                                       &key symbol document)
  (declare (ignore document))
  (with-slots (value) me
    (if (boundp symbol)
      (setf value (eval symbol))
      (setf value "Unbound")))
  me)

;;;---------------------------------------------------------------------------
;;;
;;; Functions, special-forms, generic-functions and macros
;;;
;;;---------------------------------------------------------------------------

(defmethod initialize-instance :after ((me procedure-documentation)
                                       &key symbol document)
  (let* ((lam-list (get-lambda-list symbol))
         (u-lam-list (untangle-lambda-list lam-list)))
    (with-slots (lambda-list arguments returns side-effects) me
      (unless lambda-list (setf lambda-list lam-list))
      (unless arguments
        (setf arguments
              (make-instance 'argument-documentation
                :symbol symbol
                :document document
                :lambda-list u-lam-list
                )))
      (unless returns (setf returns (get-returns document)))
      (unless side-effects (setf side-effects (get-side-effects document)))))
  me)


;;;---------------------------------------------------------------------------
;;;
;;; generic-functions
;;;
;;;---------------------------------------------------------------------------

(defmethod initialize-instance :after ((me generic-function-documentation)
                                       &key symbol document)
  (declare (ignore document))
  (unless (method-list me)
    (setf (method-list me) (collect-methods symbol)))
  me)


;;;---------------------------------------------------------------------------
;;;
;;; method
;;;
;;;---------------------------------------------------------------------------

(defmethod initialize-instance :after ((me method-documentation)
                                       &key symbol document)
  (declare (ignore document))
  (let ((mo (doc-method-object me)))
    (unless (doc-generic-function me)
      (if symbol
        (setf (doc-generic-function me) (symbol-function symbol))))
    (unless (doc-method-qualifiers me)
      (setf (doc-method-qualifiers me) (method-qualifiers mo)))
    (unless (doc-method-specializers me)
      (setf (doc-method-specializers me)
            (method-specializers  mo))
      )
    me))


;;;---------------------------------------------------------------------------
;;;
;;; Arguments
;;;
;;;---------------------------------------------------------------------------

(defmethod initialize-instance :after ((me argument-documentation)
                                       &key symbol document lambda-list)
  (declare (ignore symbol))
  (flet ((eqname (x y)
           (cond
            ((and (listp x) (listp y))
             (equal x y))
            ((or (listp x) (listp y))
             NIL)
            (T (equalp (string x)
                       (string y)))))
         (complete-annotated-arglist (arglist commented-arglist key-args test)
           (let ((result nil))
             (dolist (item arglist (nreverse result))
               (push (or (find item commented-arglist :test test) item)
                     result)))))
    (with-slots (required-args optional-args rest-args aux-args key-args allow-other-keys-args body-args) me
      (let (
            ;;
            (lam-required (mapcar #'list (cdr (assoc 'required-args lambda-list))))
            (lam-rest (mapcar #'list (cdr (assoc 'rest-args lambda-list))))
            (lam-key (mapcar #'list (cdr (assoc 'key-args lambda-list))))
            (lam-optional (mapcar #'list (cdr (assoc 'optional-args lambda-list))))
            (lam-aux (mapcar #'list (cdr (assoc 'aux-args lambda-list))))
            (lam-body (mapcar #'list (cdr (assoc 'body-args lambda-list))))
            (lam-allow-other-keys (cdr (assoc 'allow-other-keys-args lambda-list)))
            ;;
            (user-required (get-required-arg document))
            (user-rest (get-rest-arg document))
            (user-key (get-key-arg document))
            (user-optional (get-optional-arg document))
            (user-aux (get-aux-arg document))
            (user-body (get-body-arg document))
            ;;
            )
        (setf required-args (complete-annotated-arglist
                         lam-required user-required 
                         :test #'(lambda (x y) (eqname (car x) (car y)))))
        (setf rest-args (complete-annotated-arglist
                     lam-rest user-rest
                     :test #'(lambda (x y) (eqname (car x) (car y)))))  
        (setf key-args (complete-annotated-arglist 
                    lam-key user-key
                    :test #'(lambda (x y) (eqname (car x) (car y)))))
        (setf optional-args (complete-annotated-arglist 
                         lam-optional user-optional
                         :test #'(lambda (x y) (eqname (car x) (car y)))))
        (setf aux-args (complete-annotated-arglist 
                    lam-aux user-aux
                    :test #'(lambda (x y) (eqname (car x) (car y)))))
        (setf body-args (complete-annotated-arglist 
                     lam-body user-body
                     :test #'(lambda (x y) (eqname (car x) (car y)))));;
        (setf allow-other-keys-args lam-allow-other-keys)
        )))
  me)
  

  
;;;---------------------------------------------------------------------------
;;;
;;;  Class documentation
;;;
;;;---------------------------------------------------------------------------

(defun make-slots-documentation (class &rest doc-keyword-args)
  "Creates and makes a slots-documentation object for class if ~
   class has any slots otherwise returns NIL."
  (let* ((slot-defs (collect-slot-definitions class))
         (class-slots (cdr (assoc :class-slots slot-defs)))
         (instance-slots
          (cdr (assoc :instance-slots slot-defs)))
         (direct-class-slots
          (cdr (assoc :direct-class-slots slot-defs)))
         (direct-instance-slots
          (cdr (assoc :direct-instance-slots slot-defs)))
         )
    (when (or class-slots instance-slots
              direct-class-slots direct-instance-slots)
      (apply #'make-instance
             'slots-documentation
              :symbol (class-name class)
              :name   (string-downcase
                       (format NIL "~a" (class-name class)))
              :class class
              :class-slots class-slots
              :instance-slots instance-slots
              :direct-class-slots direct-class-slots
              :direct-instance-slots direct-instance-slots
              doc-keyword-args)
      ))
  )

(defmethod initialize-instance :after ((me class-documentation)
                                       &key symbol document)
  (declare (ignore document))
  (with-slots (class
               supers
               class-precedence-list
               subs
               slots
               accessors)
              me
    (unless class (setf class (find-class symbol)))
    
    (unless supers
      (setf supers (class-direct-superclasses class)))
    
    (unless class-precedence-list
      (setf class-precedence-list (class-precedence-list class)))
    
    (unless subs
      (setf subs (class-direct-subclasses class)))
    
    (unless slots
      (setf slots
            (make-slots-documentation
             class
             :doc-capsule (doc-capsule me))))
    
    (unless accessors
      (setf accessors (collect-accessors class)))
    )
  me)


  
;;;---------------------------------------------------------------------------
;;;
;;;  Accessors documentation
;;;
;;;---------------------------------------------------------------------------

(defmethod initialize-instance :after ((me accessors-documentation)
                                       &key symbol document)
  (declare (ignore document))
  (with-slots (class
               accessors)
              me
    (unless class (setf class (find-class symbol)))
    (unless symbol (setf (slot-value me 'symbol)
                         (class-name class)))
    (unless accessors
      (setf accessors (collect-accessors class)))
    )
  me)

;;;---------------------------------------------------------------------------
;;;
;;;  Accessor documentation
;;;
;;;---------------------------------------------------------------------------

(defmethod initialize-instance :after ((me accessor-documentation)
                                       &key symbol document)
  (declare (ignore symbol document))
  (with-slots (class
               accessor
               type)
              me
    (unless type
      (setf type
            (cond
             ((reader-method-p accessor) :reader)
             ((writer-method-p accessor) :writer)
             (T NIL))))
    
    (unless class
      (setf class
            (cond 
             ((eq type :reader) (first (method-specializers accessor)))
             ((eq type :writer) (second (method-specializers accessor)))
             (T NIL)))))
    me)


;;;---------------------------------------------------------------------------
;;;
;;;  Slot documentation
;;;
;;;---------------------------------------------------------------------------

(defmethod initialize-instance :after ((me slot-documentation)
                                       &key symbol slot)
  (declare (ignore symbol))
  (with-slots (name package doc-capsule)
              me
    (unless name
      (setf name (string (slot-definition-name slot))))
    (unless package
      (setf package (package-name (symbol-package (slot-definition-name slot)))))
    (unless doc-capsule
      (setf doc-capsule (documentation slot)))
    )
  me)

