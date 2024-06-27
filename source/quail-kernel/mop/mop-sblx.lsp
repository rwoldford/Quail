;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               mop-sblx.lsp
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
;;;     Greg Bennett 2017
;;;
;;;----------------------------------------------------------------------------------
;;;
;;; BASED on mop-excl.lisp
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
;;; y on N shows what aclwin recognises    gwb  042097
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

#|
;;; Debugging stuff ?
;;; 28oct2005 gwb
;;; ACL7.0 has moved function-information to the sys package
;;; here we import it to common-lisp and common-lisp-user,
;;; then export it from both before proceeding as before.
;;; This has to be done early in the LOAD of Quail.
(in-package "COMMON-LISP")
(import 'sys::function-information)
(export 'function-information)
(in-package "COMMON-LISP-USER")
(import 'sys::function-information)
(export 'function-information)
;;; 28oct2005
|#

(in-package :quail-kernel)

;;  We're breaking the rules by putting import before export, so we
;;  have to be careful and use eval-when.

;;; In aclwin there is no CLOS package  <= *****

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(sb-mop::class-direct-subclasses sb-mop::class-direct-superclasses
            sb-mop::class-direct-slots sb-mop::specializer-direct-methods
            sb-mop::specializer-direct-generic-functions sb-mop::generic-function-methods
            sb-mop::method-function  sb-mop::method-generic-function
            sb-mop::method-specializers sb-mop::slot-definition-readers
            sb-mop::slot-definition-writers sb-mop::slot-definition-initargs
            sb-mop::slot-definition-initform
            sb-mop::slot-definition-type
            sb-mop::slot-definition-name
            sb-mop::slot-definition-allocation
            sb-mop::slot-definition-initfunction)
	    ))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (shadow '(sb-mop::class-precedence-list
            sb-mop::class-prototype
            sb-mop::class-slots
            ))
  )

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(class-direct-subclasses class-direct-superclasses
            class-direct-slots specializer-direct-methods
            specializer-direct-generic-functions generic-function-methods
            method-function  method-generic-function
            method-specializers slot-definition-readers
            slot-definition-writers slot-definition-initargs
            slot-definition-initform
            slot-definition-type
            slot-definition-name
            slot-definition-allocation
            slot-definition-initfunction)
    )
  )

(eval-when (:compile-toplevel :load-toplevel :execute)
 (export '(class-p
          direct-slots
          generic-function-p
          reader-method-p
          writer-method-p)))

(eval-when (:compile-toplevel :load-toplevel :execute)
 (export '(collect-methods
          collect-accessors
          collect-slot-definitions
          )))

;;; Had to ensure that the class was finalized before calling
;;;
;;; Added for the benefit of ACL6.0 for right-button problem
(sb-ext:with-unlocked-packages (:sb-mop)
(defmethod sb-mop::finalize-inheritance
  ((thing T))
 ))

(defmethod class-precedence-list ((thing standard-class))
   (unless (sb-mop::class-finalized-p  thing)
      (sb-mop::finalize-inheritance thing))
   (sb-mop::class-precedence-list thing)
   )

;; Added 02DEC98 for errors on right-button - see porting.lsp [57]
(defmethod class-precedence-list ((thing T))
   (unless (sb-mop::class-finalized-p thing)
      (sb-mop::finalize-inheritance thing))
   (if (sb-mop::class-finalized-p thing)
      (sb-mop::class-precedence-list thing)
      (remove thing (sb-mop::compute-class-precedence-list thing))
      )
   )

(defmethod class-prototype ((thing standard-class))
   (unless (sb-mop::class-finalized-p  thing)
      (sb-mop::finalize-inheritance thing))
   (sb-mop::class-prototype thing)
   )

(defmethod class-slots ((thing standard-class))
   (unless (sb-mop::class-finalized-p  thing)
      (sb-mop::finalize-inheritance thing))
   (sb-mop::class-slots thing)
   )
#|
;;; There is sb-pcl::specializer-direct-methods (specialilzer)
;;; is this what is needed ?
(defmethod class-methods ((thing standard-class))
   (unless (sb-mop::class-finalized-p  thing)
      (sb-mop::finalize-inheritance thing))
   (cg::all-specializer-direct-methods thing)
   )
|#
(eval-when (:compile-toplevel :load-toplevel :execute)
  (export
   '(class-prototype class-precedence-list class-slots
     class-direct-methods class-methods
     )
   )
  )
;;; -----------------------------

(defun class-p (arg)
  "Return T if argument is a class."
  (typep  arg 'standard-class))

(defun generic-function-p (arg)
  "Return T if argument is a generic-function."
  (typep  arg 'generic-function))

(defgeneric method-name (method-object)
  (:documentation "Returns the name of the method."))


(sb-ext:with-unlocked-packages (:sb-mop)
  (defmethod method-name ((thing sb-mop::standard-method))
   (let ((gf (slot-value thing 'generic-function))
         )
      (when gf
         (slot-value gf 'sb-mop::name))
      )
   ))

 (sb-ext:with-unlocked-packages (:sb-mop)     
  (defmethod method-name ((thing sb-mop::standard-writer-method))
   (let ((gf (slot-value thing 'generic-function))
         name)
      (when gf
         (setf name (slot-value gf 'sb-mop::name)))
      (when (and name (listp name))
         (setf name (second name)))
      name)
      ))     


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

(defmethod class-direct-class-slots ((self standard-class))
  "Call makes little sense. Returns NIL."
  (loop for slot in (class-direct-slots self)
    when (not (eq (sb-mop::slot-definition-allocation slot) :instance))
    collect slot))

(defmethod class-class-slots ((Self standard-class))
  "Call makes little sense. Returns NIL."
  (loop for slot in (class-slots self)
    when (not (eq (sb-mop::slot-definition-allocation slot) :instance))
    collect slot))

(defmethod class-instance-slots ((Self standard-class))
  "Call makes little sense. Returns NIL."
   (loop for slot in (class-slots self)
    when (eq (sb-mop::slot-definition-allocation slot) :instance)
    collect slot))

(defmethod class-direct-instance-slots ((Self standard-class))
  "Call makes little sense. Returns NIL."
  (loop for slot in (class-direct-slots self)
    when (eq (sb-mop::slot-definition-allocation slot) :instance)
    collect slot))

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
          (if (assoc  qualifiers result :test #'equal)
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
     (cond
      ((and reader-list writer-list)
       (list
        (cons :readers reader-list)
        (cons :writers writer-list))
       )
      (reader-list (list (cons :readers reader-list)))
      (writer-list (list (cons :writers writer-list)))
      (T NIL))
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

(sb-ext:with-unlocked-packages (:sb-mop)
(defmethod slot-definition-readers ((thing T))
   NIL))

(sb-ext:with-unlocked-packages (:sb-mop)
(defmethod slot-definition-writers ((thing T))
   NIL))

(sb-ext:with-unlocked-packages (:sb-mop)
(defmethod slot-definition-readers ((thing standard-class))
  (let ((direct-methods (specializer-direct-methods thing))
        )
    (loop for method in direct-methods
          when (typep method 'sb-mop::standard-reader-method)
          collect
          method))))

(sb-ext:with-unlocked-packages (:sb-mop)
(defmethod slot-definition-writers ((thing standard-class))
  (let ((direct-methods (specializer-direct-methods thing))
        )
    (loop for method in direct-methods
          when (typep method 'sb-mop::standard-writer-method)
          collect
          method))))

(sb-ext:with-unlocked-packages (:sb-mop)
(defmethod slot-definition-readers ((thing sb-mop:standard-effective-slot-definition))
  (let ((direct-methods (specializer-direct-methods thing))
        )
    (loop for method in direct-methods
          when (typep method 'sb-mop::standard-reader-method)
          collect
          method))))

(sb-ext:with-unlocked-packages (:sb-mop)
(defmethod slot-definition-writers ((thing sb-mop:standard-effective-slot-definition))
  (let ((direct-methods (specializer-direct-methods thing))
        )
    (loop for method in direct-methods
          when (typep method 'sb-mop::standard-writer-method)
          collect
          method))))

