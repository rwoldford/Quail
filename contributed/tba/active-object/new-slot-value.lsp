;;;------------------------------------------------------------------------
;;;
;;;  new-slot-value.lisp
;;;
;;;------------------------------------------------------------------------

(in-package 'quail-user)

(defvar *old-slot-value* ())

(eval-when (compile load eval)
  (unless *old-slot-value* (setf *old-slot-value* (symbol-function 'slot-value))))

;-----

(defvar *old-setf-slot-value* ())

(eval-when (compile load eval)
  (unless *old-setf-slot-value* 
    (setf *old-setf-slot-value*
          (symbol-function (first (macroexpand '(setf (slot-value object slot-name) new-value)))))))

;-----

(defgeneric slot-value-method (self slot-name))

(defmethod slot-value-method ((self t) slot-name)
  (funcall *old-slot-value* self slot-name))

;-----

(defgeneric setf-slot-value-method (self slot-name new-value))

(defmethod setf-slot-value-method ((self t) slot-name new-value)
  (funcall *old-setf-slot-value* self slot-name new-value))

;-----

(eval-when (compile load eval)
  (setf (symbol-function 'slot-value) 
        (symbol-function 'slot-value-method)))

(eval-when (compile load eval)
  (defsetf slot-value (object slot-name) (new-value)
    `(setf-slot-value-method ,object ,slot-name ,new-value)))

;-----

