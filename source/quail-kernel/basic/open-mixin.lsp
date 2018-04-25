;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               open-mixin.lisp                               
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
;;;
;;;--------------------------------------------------------------------------------

(in-package :quail-kernel)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(with-open-quail-objects with-faster-eref-open-objects)))

;;;  Some quail-objects require allocation of space which is not lisp space.
;;;  Non-lisp space is NOT automatically garbage collected by lisp.
;;;
;;;  The two main examples of this in quail are objects which create storage
;;;  as files (eg. class file-matrix), and those which allocate storage
;;;  in static space in non-lisp memory (eg. foreign-array).
;;;
;;;  We use the general terminology from the specific case of files to describe
;;;  the state of such a quail-object.  We say the object is 'open' if the non-lisp
;;;  space has been allocated and some handle to the space is being maintained by
;;;  the lisp object, 'closed' otherwise.  In the case of files, the handle is the
;;;  open file-stream associated with the object, and in the case of foreign
;;;  arrays, the handle is a pointer to static space (ie. not maintained by the
;;;  lisp garbage collector).
;;;
;;;  It is undesirable for the garbage collector to throw away an open
;;;  quail-object. If this happened, the system would keep the storage allocated
;;;  but there would remain no way to free it since the handle to
;;;  the space had been lost along with the trashed quail-object.  In the case of
;;;  files, an extra file which perhaps should be destroyed will remain on
;;;  disk, and in the case of foreign arrays, static storage space which
;;;  should be free remains allocated.
;;;
;;;  The intention is that Quail functions should look something like the following:
;;;
;;; (defun z-function-1 (obj-1 obj-2 obj-3)
;;;   (with-open-quail-objects (obj-1 obj-2 obj-3 
;;;                               :keep result1 result2
;;;                               :temp temp) #'predicate
;;;     (setf temp (z-function-2 obj-1 obj-2))
;;;     (setf result1 (z-function-3 temp obj-3))
;;;     (setf result2 (z-function-4 obj-3 result1))
;;;     (values result1 result2)))
;;;  
;;;  The function predicate should return t for objects for which this is an
;;;  appropriate place to use the open/close.  
;;;  An alternate syntax, more commonly used perhaps, uses a type or list
;;;  of types in place of the predicate.  Any object which satisfies
;;;  typep with any one of these types is opened.
;;;
;;;  The function #'identity may be
;;;  used to always return t; objects for which open/close is meaningless will
;;;  be unaffected.  However, it should be noted that there *are* situations
;;;  when one might want to open file-matrix instances, but not foreign-array
;;;  instances, for example.  
;;;
;;;  For this reason there are innocuous mixin classes
;;;  which can be used to specify certain types of opening behavior to the
;;;  with-open-... series of macros.  The ultimate
;;;  opening class is quail-open-mixin, which can be used as a superclass to other
;;;  mixins to specify opening behavior.  The subclass faster-eref-open-mixin
;;;  is used as
;;;  a superclass of objects which when open are faster to eref.  The subclass
;;;  foreign-mixin specifies a class which knows how to open to an object with
;;;  foreign space allocated.
;;;
;;;  Notice :keep and :temp create the
;;;  the objects they mention; one can use these objects as if they had been
;;;  bound by a let form where the with-open-quail-objects appears (in fact,
;;;  the expansion of the macro does exactly that!).
;;;
;;;  One idea here is to ensure that no function returns an open quail-object
;;;  unless all of the quail-objects provided to it in its arguments are open.
;;;  If code adheres to this style, closed quail-objects can be used at the top-level
;;;  without fear of creating spurious open quail-objects.
;;;
;;;  The class quail-open-state-mixin provides a slot which maintains the current open
;;;  status of the object.  If open-state is 0, then the object is 'closed'.
;;;  If open-state is greater than 0, this indicates the number of times the
;;;  object needs to be logically closed before being physically closed.
;;;
;;;  ***** VERY IMPORTANT:
;;;  The result of _any_ intermediate calculation performed which may create
;;;  an object for which open/close is meaningful MUST be bound to a :temp
;;;  or a :keep variable.
;;;  *****
;;;   
;;;  All :temp variables are destroyed when the with-open-quail-objects is complete,
;;;  and should never be returned by the body of the macro. All :keep variables
;;;  are logically closed once so that they may be returned by the body.  A z-function
;;;  that returns an object should ensure that the object, if created open, has
;;;  an open-state equal to the _minimum_ open-state of the objects passed to the
;;;  function.  If not required, :keep or :temp may be omitted.
;;; 
;;;  Most Z internal functions and generic functions are written assuming that
;;;  they may be called with open quail-objects.  THIS STYLE IS RECOMMENDED AS A
;;;  PRECAUTION in all functions which create quail-objects.

(defclass quail-open-mixin ()
  ())

(defclass quail-open-state-mixin (quail-open-mixin proto-mixin)
  ((open-state :accessor open-state-of
               :initarg :open-state)))   ; fill-unbound-slots o/w does init

;; for classes for which eref is always faster if the object is open eg. file-matrix
;; Note:  faster-eref-open-mixin is NOT a subclass of quail-open-state-mixin ... there
;; are classes which open by opening the contents of a slot, which contents are an
;; instance of a subclass of quail-open-state-mixin.

(defclass faster-eref-open-mixin (quail-open-mixin)
  ())

(push-extension-class 'quail-open-mixin)
(push-extension-class 'quail-open-state-mixin)
(push-extension-class 'faster-eref-open-mixin)

;;;
;  VERY IMPORTANT:
;   -  fill-unbound-slots sets the open-state of a new instance to the minimum
;      open-state of all proto objects with quail-open-state-mixin, defaulting to 0 (closed).
;   -  fill-unbound-slots is called by shared-initialize :after ((self quail-object)),
;      hence quail-open-state-mixin must be mixed in with a subclass of quail-object to use
;      this method.
;   -  then other shared-initialize :after methods MUST use this info to set up
;      a consistent open/closed state for the instance.
;               

(defmethod fill-unbound-slots :after ((self quail-open-state-mixin))
  (with-slots ((protos proto)) self
    (setf protos (remove-if-not #'(lambda (proto) (typep proto 'quail-open-state-mixin))
                                protos))
    (if (fill-slot-p self 'open-state)
      (let* ((open-states (mapcar #'open-state-of protos))
             (min-open-state (if (null open-states)
                               0
                               (apply #'min open-states))))
        (setf (slot-value self 'open-state) min-open-state)))))

;;;
;  DEFGENERICS for quail-open-object, etc.
;

(defgeneric quail-open-object (arg))

(defgeneric quail-close-preserve-object (arg))

(defgeneric quail-close-destroy-object (arg))

(defgeneric quail-close-physically-object (arg))

;;;
;  DEFMETHODs for class t
;

(defmethod quail-open-object ((self t))
  ;; do nothing
  )

(defmethod quail-close-preserve-object ((self t))
  ;; do nothing
  )

(defmethod quail-close-destroy-object ((self t))
  ;; do nothing
  )

(defmethod quail-close-physically-object ((self t))
  ;; do nothing
  )

;;;
;  DEFMETHODs for class quail-open-state-mixin
;

;;  If there is no overriding primary method from a more specific class,
;;  then it's  an oversight ... call it an error, forcing the
;;  definition of such a primary method.

(defmethod quail-open-object ((self quail-open-mixin))
  (missing-method 'quail-open-object self))

(defmethod quail-close-preserve-object ((self quail-open-mixin))
  (missing-method 'quail-close-preserve-object self))

(defmethod quail-close-destroy-object ((self quail-open-mixin))
  (missing-method 'quail-close-destroy-object self))

(defmethod quail-close-physically-object ((self quail-open-mixin))
  (missing-method 'quail-close-physically-object self))

;;  The :after methods, assuming that the primary methods have done any
;;  required physical opening/closing, do the modifications to open-state.
;;
;;  Note that since physical modifications have already been done, the
;;  debug statements MUST follow the mods to open-state for the object
;;  to be consistent.  Note also that the debug statements will cause
;;  all sorts of difficulties with any objects which must be open
;;  in order to print !!

#|
(eval-when (compile load eval) (pushnew :open-mixin-debug *features*))
|#

(defmethod quail-open-object :after ((self quail-open-state-mixin))
  (with-slots (open-state) self
    (incf open-state)
    #+:open-mixin-debug
    (format *quail-terminal-io*
            "~&Done quail-open-object of ~S, open-state ~S --> ~S"
            self
            (1- open-state)
            open-state)))

(defmethod quail-close-preserve-object :after ((self quail-open-state-mixin))
  (with-slots (open-state) self
    #-:open-mixin-debug
    (if (> open-state 0)
      (decf open-state))
    #+:open-mixin-debug
    (let ((prev-state open-state))
      (if (> open-state 0)
        (decf open-state))
      (format *quail-terminal-io*
              "~&Doing quail-close-preserve-object of ~S, open-state ~S --> ~S"
              self
              prev-state
              open-state))
    ))

(defmethod quail-close-destroy-object :after ((self quail-open-state-mixin))
  (with-slots (open-state) self
    #-:open-mixin-debug
    (setf open-state 0)
    #+:open-mixin-debug
    (let ((prev-state open-state))
      (setf open-state 0)
      (format *quail-terminal-io*
              "~&Doing quail-close-destroy-object of ~S, open-state ~S --> ~S **destroy**"
              self
              prev-state
              open-state))
    ))

(defmethod quail-close-physically-object :after ((self quail-open-state-mixin))
  (with-slots (open-state) self
    #-:open-mixin-debug
    (setf open-state 0)
    #+:open-mixin-debug
    (let ((prev-state open-state))
      (setf open-state 0)
      (format *quail-terminal-io*
              "~&Doing quail-close-physically-object of ~S, open-state ~S --> ~S"
              self
              prev-state
              open-state))
    ))


;;;
;  quail-open, quail-close-destroy, quail-close-preserve, and quail-close-physically
;

(defun quail-open (objects &optional (predicate #'identity))
  (loop for object in objects
        do (if (funcall predicate object)
             (quail-open-object object))))

(defun quail-close-preserve (objects &optional (predicate #'identity))
  (loop for object in objects
        do (if (funcall predicate object)
             (quail-close-preserve-object object))))

(defun quail-close-destroy (objects &optional (predicate #'identity))
  (loop for object in objects
        do (if (funcall predicate object)
             (quail-close-destroy-object object))))

(defun quail-close-physically (objects &optional (predicate #'identity))
  (loop for object in objects
        do (if (funcall predicate object)
             (quail-close-physically-object object))))

;;;
;  Other functions
;

;;;  quail-open-p-object returns T if instance is open.  Objects which are not instances
;;;  of a subclass of quail-open-mixin are deemed always to be open.

(defmethod quail-open-p-object ((self t))
  t)

(defmethod quail-open-p-object ((self quail-open-state-mixin))
  (with-slots (open-state) self
    (> open-state 0)))

;;;  originally-open-p-object returns T if instance was open *before* the previous
;;;  quail-open call, NIL if it was closed.

(defmethod originally-open-p-object ((self t))
  t)

(defmethod originally-open-p-object ((self quail-open-state-mixin))
  (with-slots (open-state) self
    (> open-state 1)))

(defvar *create-closed-only*)
(setf *create-closed-only* nil)

(defun quail-all-originally-open-p (&rest args)
  (declare (special *create-closed-only*))
  (if *create-closed-only*
    nil
    (all-non-nil-p (mapcar #'originally-open-p-object args))))

;;;
;  Predicates for with-open-quail-objects
;

(defmacro one-of-these-types-p (object types)
  (let ((type (gensym))
        (pred (gensym)))
    `(let ((,pred nil))
       (loop for ,type in ,types
             until (setf ,pred (typep ,object ,type)))
       ,pred)))

;;;  The with-open-quail-objects macro

(defun quail-open-interpret (objects)
  (let (env-objects
        list-objects
        keep-objects
        temp-objects
        (state :env))
    (loop for obj in objects
          do (if (keywordp obj)
               (setf state obj)
               (case state
                 (:env (push obj env-objects))
                 (:list (push obj list-objects))
                 (:keep (push obj keep-objects))
                 (:temp (push obj temp-objects))
                 (t (quail-error "~S is not a permitted keyword for with-open-quail-objects."
                           state)))))
    (values env-objects list-objects keep-objects temp-objects)))

(defmacro with-open-quail-objects
          (objects predicate-or-types &body body)
  (let* ((result (gensym))
         (thru-safely (gensym))
         (predicate (gensym))
         (predicate-body
          (cond ((symbolp predicate-or-types)
                 `(function (lambda (x) 
                              (typep x (quote ,predicate-or-types)))))
                ((and (listp predicate-or-types)
                      (not (eq (car predicate-or-types) 'function)))
                 `(function (lambda (x) 
                              (one-of-these-types-p x (quote ,predicate-or-types)))))
                (t `(eval ,predicate-or-types)))))
    (multiple-value-bind (env-objects list-objects keep-objects temp-objects)
                         (quail-open-interpret objects)
      `(let (,result
             (,thru-safely nil)
             ,@keep-objects
             ,@temp-objects
             (,predicate ,predicate-body))
         (unwind-protect
           (progn
             ,@(if (or env-objects list-objects)
                 (list
                  `(quail-open (append (list ,@env-objects) ,@list-objects)
                               ,predicate)))
             (setf ,result (multiple-value-list (progn ,@body)))
             (setf ,thru-safely t)
             (values-list ,result))
           ,@(if (or env-objects keep-objects list-objects)
               (list
                `(if ,thru-safely
                   (quail-close-preserve (append (list ,@env-objects ,@keep-objects)
                                                 ,@list-objects)
                                         ,predicate)
                   (quail-close-physically (append (list ,@env-objects ,@keep-objects)
                                                   ,@list-objects)
                                           ,predicate))))
           ,@(if (or temp-objects)
               (list `(quail-close-destroy (list ,@temp-objects) ,predicate))))))))

(defmacro with-faster-eref-open-objects
          (objects &body body)
  `(with-open-quail-objects ,objects faster-eref-open-mixin ,@body))

