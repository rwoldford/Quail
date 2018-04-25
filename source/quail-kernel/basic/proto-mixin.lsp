(in-package :qk)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               proto-mixin.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1990 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     Greg Anglin 1989, 1990.
;;;
;;;     mel, rwo touched in places
;;;--------------------------------------------------------------------------------

(defclass proto-mixin ()
  ((proto :accessor proto-of
          :initarg :proto
          :initform nil)))

;;  The following is defined on shared-initialize :after so that it gets used
;;  when reinitialize-instance, like initialize-instance, is called.  It also
;;  gives a slot-names which are permitted to be modified.  Usually
;;  this is T, meaning any slot.  See CLtL 2, section 28.1.9.5.
;;
;;  If make-instance is called with :proto :ignore, then that make-instance MUST
;;  be followed by a reinitialize-instance (not having :proto :ignore) BEFORE
;;  the created instance is used in any way other than method dispatching.
;;
;;  proto-mixins, say of class 'quail-proto-using-class,
;;  should be initialized in one of two ways:
;;
;;     1.  (setf self (make-instance 'quail-proto-using-class ...))
;;  or
;;     2.  (setf self (make-instance 'quail-proto-using-class :proto :ignore ...))
;;         ;; then later, when possible (using :proto if desired)
;;         (reinitialize-instance self ...) 
;;
;;  CAUTION:  In scheme 2, initialize-instance :after is executed only once,
;;            during the make-instance.  If you want code executed both times,
;;            put it onto shared-initialize :after.  I suppose you could instead
;;            put different code on reinitialize-instance :after, but at the very
;;            least it's prudent to have similar code on initialize-instance :after
;;            as well, in which case shared-initialize :after would probably serve
;;            the purpose.
;;  

(defmethod shared-initialize :after ((self proto-mixin) slot-names &rest initargs)
  (declare (ignore slot-names initargs))
  (with-slots (proto) self
    (if (eq proto :ignore)
      (setf proto nil)
      (progn
        (if (not (listp proto))
          (setf proto (list proto)))
        (fill-unbound-slots self)))))  ;; proto is destroyed --> nil by default :around method

#|
;;  This may be useful for debugging, since can't trace shared-initialize.
;;  Substitute whatever class is of interest (not proto-mixin !).

(defmethod shared-initialize :after ((self ref-array) slot-names &rest initargs)
    (format *quail-terminal-io* "~&Shared-initialize ~S slot-names ~S initargs ~S"
                          self
                          slot-names
                          initargs))
|#

;;
;;  fill-unbound-slots is a very important generic function called from
;;  initialize-instance :after ((self proto-mixin) ...).
;;
;;  The purpose of fill-unbound-slots is to perform complex initialization
;;  of slots of new instances of subclasses of proto-mixin.  The slot proto
;;  contains a list of instances which were involved in the construction
;;  of the new instance, and these can be used to provide information
;;  about the best values for any unbound slots of the new instance.
;;  New methods will be :after methods, thus slots from superclasses
;;  can be relied upon to be bound.
;;
;;  It is crucial to the operation of this generic function that any slot involved
;;  has no :initform, since each :after method relies upon the unbound-ness of
;;  the slot to ascertain that it was not specifically initialized with an
;;  :initarg.
;;
;;  The :around method provided leaves slot proto with value nil, and thus avoids
;;  keeping unnecessary references to the proto objects (this helps with garbage
;;  collection).  This can be overridden by more specialized :around methods, but
;;  such methods should be sure to invoke #'call-next-method !!
;;

;; I CHANGED THIS - MEL
;; (defmacro fill-slot-p (self slot-name)
;;  `(not (slot-boundp ,self ,slot-name)))

(defun fill-slot-p (self slot-name)
  (not (slot-boundp self slot-name)))

(defgeneric fill-unbound-slots (any-proto-mixin))

(defmethod fill-unbound-slots ((self proto-mixin))
  ;; do nothing ... just here for :around
  )

(defmethod fill-unbound-slots :around ((self proto-mixin))
  (call-next-method self)
  (setf (proto-of self) nil))

;;  The structure for more specialized proto-mixins (say quail-proto-using-class), should be
;;  something like:

#|
(defmethod fill-unbound-slots :after ((self quail-proto-using-class))
  (with-slots ((protos proto)) self
    (setf protos (remove-if-not #'(lambda (proto) (typep proto 'quail-proto-using-class))
                                protos))
    (if (fill-slot-p self 'quail-proto-using-class-slot)
      (setf (slot-value self 'quail-proto-using-class-slot) 
            (some-optimal-value self protos 'quail-proto-using-class-slot)))))
|#


