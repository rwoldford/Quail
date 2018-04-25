;;;------------------------------------------------------------------------
;;;  ref-behavior.lisp
;;;
;;;  copyright, 1991, r. wayne oldford
;;;------------------------------------------------------------------------

(in-package :quail-kernel)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(defrefclass copy-slots shared-initialize)))

(proclaim '(declaration author))

(proclaim '(author "michael lewis"))

;--------------------------------------------------------------------------
; defgeneric: copy-slots
;--------------------------------------------------------------------------

(defgeneric copy-slots (self)
  (:documentation
   
   "Decides the destiny of the contents of the slots of an object ~
    after it is referenced.  Persistent slots are copied to the ~
    result; transient slots are reinitialized."))

(defmethod copy-slots ((self t))
  nil)

;--------------------------------------------------------------------------

(defmacro make-copy-slots (class-name direct-slots)
  (block the-entire-macro
    (if (not (listp direct-slots))
      (return-from the-entire-macro
                   nil))
    (if (not (listp (car direct-slots))) 
      (return-from the-entire-macro
                   `(make-copy-slots ,class-name ,(list direct-slots))))
    (let ((defmethod-form
            (multiple-value-bind
              (slot-names reader-names writer-names)
              (loop for slot in direct-slots
                    as persistent = (second (member :persistent slot))  
                    with slot-name = nil
                    with accessor = nil
                    with reader = nil
                    with writer = nil
                    when persistent
                    do 
                    (setf slot-name (first slot))
                    (setf accessor (second (member :accessor slot)))
                    (setf reader (second (member :reader slot)))
                    (setf writer (second (member :writer slot)))
                    (setf reader
                          (if accessor
                            `(lambda ()
                               (,accessor read-object))
                            (if reader
                              `(lambda ()
                                 (,reader read-object))
                              `(lambda ()
                                 (slot-value read-object ',slot-name)))))
                    (setf writer
                          (if accessor
                            `(lambda (new-value)
                               (setf (,accessor write-object) new-value))
                            (if writer
                              `(lambda (new-value)
                                 (,writer new-value write-object))
                              `(lambda (new-value)
                                 (setf (slot-value write-object ',slot-name) new-value)))))
                    and collect slot-name into slot-names
                    and collect reader into reader-names
                    and collect writer into writer-names
                    finally
                    (return 
                     (values slot-names reader-names writer-names)))
              `(defmethod copy-slots ((write-object ,class-name))
                 (let* ((proto-list (proto-of write-object))
                        (read-object (find-if #'(lambda (x) 
                                                  (eql (class-of x) (class-of write-object)))
                                              proto-list)))
                   (when read-object
                     ,.(loop for slot in slot-names
                             as reader in reader-names
                             as writer in writer-names
                             collect
                             `(funcall ,writer (funcall ,reader)))))))))
      defmethod-form)))

;----------------------------------------------------------------------------------
; defmethod: shared-initialize
;----------------------------------------------------------------------------------

(defmethod shared-initialize :after ((self proto-mixin) slot-names &rest initargs)
  (declare (ignore slot-names initargs))
  (with-slots (proto) self
    (if (eq proto :ignore)
      (setf proto nil)
      (progn
        (if (not (listp proto))
          (setf proto (list proto)))
        (copy-slots self)
        (fill-unbound-slots self)))))
              
;-----------------------------------------------------------------------------------

(defun make-slot-defs (direct-slots)
  (cond ((null direct-slots) nil)
        ((equal direct-slots (list nil)) direct-slots)
        (t
         (let ((a (car direct-slots))
               (b (cdr direct-slots)))
           (cond ((atom a)
                  (if (eql a :persistent)
                    (make-slot-defs (cdr b))
                    (cons a (make-slot-defs b))))
                 ((listp a)
                  (cons (make-slot-defs a)
                        (make-slot-defs b)))
                 (t 
                  (quail-error "~s is an unreadable slot definition"
                         direct-slots)))))))

(defmacro make-defclass (name direct-superclasses direct-slots &rest options)
  `(defclass ,name ,direct-superclasses ,(make-slot-defs direct-slots) ,.options))

(defmacro defrefclass (name direct-superclasses direct-slots &rest options)
  `(prog1
     (make-defclass ,name ,direct-superclasses ,direct-slots ,.options)
     (make-copy-slots ,name ,direct-slots)
     (push-extension-class ',name)
     ))

;--------------------------------------------------------------------------------------
