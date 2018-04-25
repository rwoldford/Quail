;;;------------------------------------------------------------------------
;;;
;;;  active-value.lisp
;;;
;;;------------------------------------------------------------------------

(in-package 'quail-user)

(defclass active-value (quail-object)
  ((value :initarg :value
          :initform nil
          :accessor value-of)
   (get-function :initarg :get-function
                 :initform nil
                 :accessor get-function-of) 
   (put-function :initarg :put-function
                 :initform nil
                 :accessor put-function-of)))

;-----

(defun active-value-p (val)
  (typep val 'active-value))

;----------------------------------------------------------------------------------

(defmethod slot-value-method ((self quail-object) slot-name)
  (let ((slot (funcall *old-slot-value* self slot-name)))
    (if (active-value-p slot)
      (get-value self slot)
      slot)))

;-----

(defmethod (setf slot-value) (new-value (self quail-object) &rest slot-name)
  (let ((slot (apply *old-slot-value* self slot-name)))
    (if (active-value-p slot)
      (put-value self slot new-value)
      (funcall *old-setf-slot-value* self (first slot-name) new-value))))

;-----

(defun get-value (obj slot)
  (funcall (get-function-of slot) obj slot))

;-----

(defun put-value (obj slot new-value)
  (funcall (put-function-of slot) obj slot new-value))

;-----

(defclass test (quail-object) ((a :initarg :a)
                           (b :initarg :b)
                           (c :initarg :c)))

(defmethod a-of ((self test))
  (funcall (symbol-function 'slot-value) self 'a))

(defmethod b-of ((self test))
  (funcall (symbol-function 'slot-value) self 'b))

(defmethod c-of ((self test))
  (funcall (symbol-function 'slot-value) self 'c))

;-----

(setf c (make-instance 'active-value 
                       :get-function 
                       #'(lambda (object slot)
                           (or (value-of slot)
                               (max (a-of object)
                                    (b-of object))
                               :put-function
                               #'(lambda (object slot new-value)
                                   (declare (ignore object slot new-value))
                                   (quail-print "PUT"))))

(setf x (make-instance 'test :a 9 :b y))

;-----
