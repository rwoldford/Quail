;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               array.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1993 Statistical Computing Laboratory, University of Waterloo
;;;
;;; 
;;;  Authors:
;;;     Greg Anglin 1993.
;;;
;;;--------------------------------------------------------------------------------

(in-package :quail-kernel)

#+:sbcl-linux(shadow '(array dimensions))

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(array)))

;;;
;  array-default-dimensions
;

(defgeneric array-default-dimensions (contents))

(defmethod array-default-dimensions ((contents t))
  (missing-method 'array-default-dimensions contents))

(defmethod-multi array-default-dimensions ((contents (number symbol string)))
  (list 1))

(defmethod array-default-dimensions ((contents array))
  (array-dimensions contents))

(defmethod array-default-dimensions ((contents list))
  (nested-list-dimensions contents))

(defmethod array-default-dimensions ((contents dimensioned-ref-object))
  (dimensions-of contents))

(defmethod array-default-dimensions ((contents scan-env))
  (list t))

(defun nested-list-dimensions (nested)
  (declare (type list nested))
  (let ((first (first nested)))
    (list* (length nested)
           (if (listp first)
             (nested-list-dimensions first)))))

;;;
;  array-default-class-name
;

(defgeneric array-default-class-name (contents dimensions &optional deconstruct))

(defmethod array-default-class-name ((contents t) dimensions &optional deconstruct)
  (declare (ignore dimensions deconstruct))
  (missing-method 'array-default-class-name contents))

(defmethod-multi array-default-class-name 
  ((contents (number (eql nan) (eql infinity) (eql +infinity) (eql -infinity)))
   dimensions
   &optional deconstruct)
  (declare (ignore deconstruct))
  (if (<= (length dimensions) 2)
    'matrix
    'num-array))

(defmethod-multi array-default-class-name 
  ((contents (dimensioned-ref-object list array))
   dimensions
   &optional (deconstruct t))
  (if deconstruct
    (if (<= (length dimensions) 2)
      'matrix
      'num-array)
    'ref-array))

(defmethod array-default-class-name ((contents scan-env)  
                                     dimensions &optional deconstruct)
  (declare (ignore deconstruct))
  ;; for the moment assume its numerical, until -derive is online
  (if (<= (length dimensions) 2)
        'matrix
        'num-array))

(defmethod-multi array-default-class-name 
  ((contents (symbol string)) dimensions &optional deconstruct)
  (declare (ignore dimensions deconstruct))
  'ref-array)

;;;
;  array-derive-class-name
;

(defgeneric array-derive-class-name (contents &optional dimensions deconstruct))

(defmethod array-derive-class-name ((contents t) &optional (dimensions :default)
                                    (deconstruct t))
  (declare (ignore dimensions deconstruct))
  (missing-method 'array-derive-class-name contents))

;;;
;  array-provide-dimensions
;

(defgeneric array-provide-dimensions (dimensions contents &optional deconstruct))

(defmethod array-provide-dimensions ((dimensions t) contents &optional deconstruct)
  (declare (ignore deconstruct))
  (missing-method 'array-provide-dimensions dimensions contents))

(defmethod array-provide-dimensions ((dimensions (eql :default)) contents
                                     &optional (deconstruct t))
  (declare (ignore dimensions))
  (if deconstruct
    (array-default-dimensions contents)
    (list 1)))

(defmethod array-provide-dimensions ((dimensions (eql :default)) (contents string)
                                     &optional (deconstruct nil))
  (declare (ignore dimensions))
  (if deconstruct
    (list (length contents))
    (list 1)))

(defmethod array-provide-dimensions ((dimensions (eql :matrix)) contents
                                     &optional (deconstruct t))
  (declare (ignore dimensions))
  (if deconstruct
    (subseq (full-matrix-dimensions
             (array-default-dimensions contents))
            0
            2)
    (list 1 1)))

;; following changed from fixnum to integer by Greg Anglin 93 12
;; to support Allegro port
(defmethod array-provide-dimensions ((dimensions integer) contents
                                     &optional deconstruct)
  (declare (ignore contents deconstruct))
  (list dimensions))

(defmethod array-provide-dimensions ((dimensions list) contents
                                     &optional deconstruct)
  (declare (ignore contents deconstruct))
  dimensions)

;;;
;  array-provide-class-name
;

(defgeneric array-provide-class-name (class dimensions contents &optional deconstruct))

#|
;; following removed by Greg Anglin 93 12
;; to support Allegro port
(defmethod-multi array-provide-class-name ((class (keyword t)) dimensions
                                           contents &optional deconstruct)
  (declare (ignore deconstruct))
  (missing-method 'array-provide-class-name class dimensions contents))
|#

(defmethod-multi array-provide-class-name ((class symbol) dimensions contents
                                           &optional deconstruct)
  (declare (ignore contents dimensions deconstruct))
  (if (keywordp class)
    (quail-error "Unrecognized array class specifier ~S." class)
    class))

(defmethod array-provide-class-name ((class (eql :default)) dimensions contents
                                      &optional (deconstruct t))
  (array-default-class-name contents dimensions deconstruct))
         
(defmethod-multi array-provide-class-name ((class (null (eql :derive)))
                                           dimensions contents
                                           &optional (deconstruct t))
  (array-derive-class-name contents dimensions deconstruct))

(defmethod array-provide-class-name ((class standard-class) dimensions contents
                                     &optional deconstruct)
  (declare (ignore dimensions contents deconstruct))
  (class-name class))
  
;;;
;  array
;

#-:sbcl-linux(defgeneric array (contents &rest initargs 
                          &key &allow-other-keys)
  (:documentation
   "Creates an array whose contents are given by the first argument. ~
    Various methods are implemented that depend on the class of the ~
    contents given.  Typically the contents can be any ref'able data ~
    structure like a list or an array. ~
    (:required ~
    (:arg contents The cell contents of the array.  Usually just a list of ~
    the elements, perhaps organized as a list of lists in row-major order. ~
    Other possibilities include anything that is returned by array, ~
    :empty to get an empty array, :default, a number, a string, or a symbol. ~
    If the number of elements of the contents do not match the dimensions ~
    given then array tries to fill out to an array the required dimensions ~
    using the contents as elements.) ~
    ) ~
    (:key ~
    (:arg dimensions :default A list of the dimensions of the array.  For example ~
    a 10 by 4 matrix would have dimensions (list 10 4). ~
    By default the dimensions will be inferred from the contents.) ~
    (:arg class :default  If given this will be the class of the array returned. ~
      If :default, some effort is made to produce an appropriate return class.) ~
    (:arg fill :row  The major order in which the array is filled.  If :row ~
    then row major order is used.  If :col, then column-major order.) ~
    (:arg deconstruct :default  If deconstruct is NIL, contents is used like an initial-element ~
    ie. each element of the resulting instance is the value of contents. ~
    The default value of deconstruct is NIL if source is a number, symbol, or string ~
    (for speed, mostly) T   otherwise.) ~
    (:arg element-copy #'identity Function to be applied to each element of contents ~
    that will appear in the array.) ~
    (:arg type  NIL No further information available.) ~
    ) ~
    (:rest (:arg initargs NIL Other keyword value initialization arguments to be passed ~
    on to the class to be created by array.))"
   )
    
;; valid keys:  dimensions class fill deconstruct element-copy type
;; future?:     instance-copy element-type
  )

#+:sbcl-linux(sb-ext:with-unlocked-packages (:common-lisp)(defgeneric array (contents &rest initargs 
                          &key &allow-other-keys)
  (:documentation
   "Creates an array whose contents are given by the first argument. ~
    Various methods are implemented that depend on the class of the ~
    contents given.  Typically the contents can be any ref'able data ~
    structure like a list or an array. ~
    (:required ~
    (:arg contents The cell contents of the array.  Usually just a list of ~
    the elements, perhaps organized as a list of lists in row-major order. ~
    Other possibilities include anything that is returned by array, ~
    :empty to get an empty array, :default, a number, a string, or a symbol. ~
    If the number of elements of the contents do not match the dimensions ~
    given then array tries to fill out to an array the required dimensions ~
    using the contents as elements.) ~
    ) ~
    (:key ~
    (:arg dimensions :default A list of the dimensions of the array.  For example ~
    a 10 by 4 matrix would have dimensions (list 10 4). ~
    By default the dimensions will be inferred from the contents.) ~
    (:arg class :default  If given this will be the class of the array returned. ~
      If :default, some effort is made to produce an appropriate return class.) ~
    (:arg fill :row  The major order in which the array is filled.  If :row ~
    then row major order is used.  If :col, then column-major order.) ~
    (:arg deconstruct :default  If deconstruct is NIL, contents is used like an initial-element ~
    ie. each element of the resulting instance is the value of contents. ~
    The default value of deconstruct is NIL if source is a number, symbol, or string ~
    (for speed, mostly) T   otherwise.) ~
    (:arg element-copy #'identity Function to be applied to each element of contents ~
    that will appear in the array.) ~
    (:arg type  NIL No further information available.) ~
    ) ~
    (:rest (:arg initargs NIL Other keyword value initialization arguments to be passed ~
    on to the class to be created by array.))"
   )
    
;; valid keys:  dimensions class fill deconstruct element-copy type
;; future?:     instance-copy element-type
  ))



(defmethod array ((contents t) &rest initargs 
                &key (dimensions :default) (class :default))
  (declare (ignore initargs dimensions class))
  (missing-method 'array contents))

(defmethod-multi array ((contents ((eql :empty) (eql :default)))
                      &rest initargs 
                      &key (dimensions :default) (class :default))
  (setf dimensions (array-provide-dimensions dimensions contents))
  ;; use a number as contents to default to num-array when class not provided
  (setf class (array-provide-class-name class dimensions 1))
  (apply #'initialize-contents 
         class 
         :empty
         :dimensions dimensions
         initargs))

(defmethod-multi array ((contents (number symbol))
                      &rest initargs 
                      &key (dimensions :default) (class :default))
  (setf dimensions (array-provide-dimensions dimensions contents))
  (setf class (array-provide-class-name class dimensions contents))
  (apply #'initialize-contents 
         class 
         contents
         :dimensions dimensions
         initargs))

(defmethod-multi array ((contents (dimensioned-ref-object list array scan-env))
                        &rest initargs 
                        &key (dimensions :default) (class :default)
                        (deconstruct (not (stringp contents))))
  ;;
  ;; string is handled here to allow for the possibility that one might want to fill
  ;; a ref-array with characters, but by default it's not deconstructed.
  ;;
  (setf dimensions (array-provide-dimensions dimensions contents deconstruct))
  (setf class (array-provide-class-name class dimensions contents deconstruct))
  (apply #'initialize-contents 
         class 
         contents
         :dimensions dimensions
         initargs))

;;;
;  fill-contents
;

;; A function useful to some initialize-contents methods

;; If deconstruct is NIL, source is used like an initial-element
;; ie. each element of the resulting instance is source.
;; The default value of deconstruct is
;;   NIL if source is a number, symbol, or string (for speed, mostly)
;;   T   otherwise

(defun fill-contents (target source &key (fill :row)
                             (element-copy #'identity)
                             (deconstruct 
                              :default)
                             &allow-other-keys)                                        
  (if (eq deconstruct :default)
    (setf deconstruct (not (or (numberp source) (symbolp source) (stringp source)))))
  (let ((num-target (number-of-elements target))
        (num-source (if deconstruct (number-of-elements source) 1))
        (fill-function
         (ecase fill
           ((:default row :row) #'row-major-eset)
           ((:column :col col) #'column-major-eset))))
    (if deconstruct
      (progn
        (loop for i from 0 to (1- num-target)
              do (funcall fill-function
                          target
                          i
                          (funcall element-copy (row-major-eref source i))))
        (cond ((zerop (rem num-target num-source)) nil)
              ((< num-target num-source)
               (warn "Filled object ~S could not contain all of the elements of ~
                      initializing object ~S."
                     target
                     source))
              ((> num-target num-source)
               (warn "Number of elements in initializing object ~S does not divide evenly ~
                      into the number of elements of filled object ~S."
                     source
                     target))))
      (loop for i from 0 to (1- num-target)
            do (funcall fill-function
                        target
                        i
                        (funcall element-copy source))))
    ))

;;;
;  initialize-contents
;

(defgeneric initialize-contents (ref-array init &rest initargs &key &allow-other-keys))

;; These methods for initialize-contents is responsible for making the instance,
;; and reinvoking initialize-contents on that instance.

(defmethod initialize-contents ((class symbol) (init t)
                                &rest initargs)
  (apply #'initialize-contents (find-class class) init initargs))

(defmethod initialize-contents ((class standard-class) (init t)
                                &rest initargs 
                                &key (dimensions :not-provided-is-an-error))
  (declare (ignore dimensions))
  (let ((new-instance (apply #'make-instance
                             class
                             :proto init
                             (remove-illegal-make-instance-initargs class initargs)
                             )))
    (apply #'initialize-contents
           new-instance
           init
           ;; initargs already contains :dimensions dimensions
           initargs)
    new-instance))

;;;
;  remove-illegal-make-instance-initargs
;
                                                
(defun remove-illegal-keys (legal-keys keyword-value-list)
  (let ((len-keys (length keyword-value-list)))
    (if (/= 0 (rem len-keys 2))
      (quail-error "The list of keys ~S is not of even length."
                   keyword-value-list))
    (loop for key-value on keyword-value-list
          as i from 0 to (- len-keys 1)
          when (and (evenp i)
                    (member (first key-value) legal-keys))
          append (list (first key-value) (second key-value)))))

(defgeneric remove-illegal-make-instance-initargs (class initargs))

(defmethod remove-illegal-make-instance-initargs ((class-name symbol)
                                                  initargs)
  (remove-illegal-make-instance-initargs (find-class class-name)
                                                  initargs))

#|
(defmethod remove-illegal-make-instance-initargs ((class standard-class)
                                                  initargs)
  (if initargs
    (remove-illegal-keys 
     #+:ccl initargs ;(ccl::class-make-instance-initargs class)
     #+:(and :aclunix (not :aclpc)) (let ((legal-initargs (clos::compute-class-initargs class)))
               (or legal-initargs
                   ;; this is a bit bummed if the class *really* doesn't have
                   ;; any initargs :-) [ or if it breaks unless it gets some ]
                   ;; ... gotta be a better way.
                   (progn
                     (make-instance class)
                     (clos::compute-class-initargs class))))
     initargs))
     #+:aclpc initargs
     initargs)

;; Version from backup to work with acl
(defmethod remove-illegal-make-instance-initargs ((class standard-class)
                                                  initargs)
  #+(or :ccl (and :aclunix (not :aclpc)))
  (if initargs
    (remove-illegal-keys 
     #+:ccl (ccl::class-make-instance-initargs class)
     #+(and :aclunix (not :aclpc)) (let ((legal-initargs (clos::compute-class-initargs class)))
                                     (or legal-initargs
                                         ;; this is a bit bummed if the class *really* doesn't have
                                         ;; any initargs :-) [ or if it breaks unless it gets some ]
                                         ;; ... gotta be a better way.
                                         (progn
                                           (make-instance class)
                                           (clos::compute-class-initargs class))))
     initargs))
  #+:aclpc initargs
  )
|#
;;; version checked with acl-express-10.1, sbcl-1.4.0, ccl-1.11
(defmethod remove-illegal-make-instance-initargs ((class standard-class)
                                                  initargs)
  ;#+(or :ccl :sbcl-linux (and :aclunix (not :aclpc)))
  (if initargs
    (remove-illegal-keys 
     #+:ccl (ccl::class-make-instance-initargs class)
     #+:aclpc-linux (excl::compute-class-initargs class)                                 
     #+:sbcl-linux (mapcar #'car (mapcar 'sb-mop:slot-definition-initargs
      (sb-mop:compute-slots class)))
     initargs))
  #+:aclpc-mswin initargs
  )
