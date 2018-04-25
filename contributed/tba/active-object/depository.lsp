;;;------------------------------------------------------------------------
;;;  inverse.lisp
;;;
;;;------------------------------------------------------------------------

(in-package :pcl)

;-----

(defvar *old-slot-value* ())

(unless *old-slot-value* (setf *old-slot-value* (symbol-function 'slot-value)))

;-----

(defgeneric slot-value-method (self slot-name))

(defmethod slot-value-method ((self t) slot-name)
  (funcall *old-slot-value* self slot-name))

;-----

(setf (symbol-function 'slot-value) 
      #'(lambda (self slot-name)
          (slot-value-method self slot-name)))

;-----

(defvar *new-slot-value* ())

(unless *new-slot-value* (setf *new-slot-value* (symbol-function 'slot-value)))

;-------------------------------------------------------------------------------------

(defvar *old-describe* ())

(unless *old-describe* (setf *old-describe* (symbol-function 'describe)))

(setf (symbol-function 'describe)
      #'(lambda (object)
          (describe-object object *quail-standard-output*)
          (values)))

;-----

(defmethod describe-object ((self t) stream)
  (funcall *old-describe* self))

;------------------------------------------------------------------------------------

(defclass test (matrix)
  ((object :initarg :object
           :initform nil
           :accessor object-of) 
   (calculated? :initarg :calculated?
                :initform nil
                :accessor calculated-of)))

;-----

(defmethod print-object :around ((self test) stream)
  (if (calculated-p self)
    (call-next-method)
    (format stream "~A" "#<TEST>"))
  self)

;-----

(defmethod describe-object ((self test) stream)
  (if (calculated-p self)
    (call-next-method)
    (unwind-protect
      (progn
        (setf (symbol-function 'slot-value)
              *old-slot-value*)
        (call-next-method))
      (setf (symbol-function 'slot-value)
            *new-slot-value*)))
  self)

;-----

(defmethod calculated-p ((self test))
  (not (null (calculated-of self))))

;---------------------------------------------------------------------------------

(defmethod inverse :around ((x matrix))
  (if (apply #'= (dimensions-of x))
    (call-next-method)
    (quail-error "INVERSE only defined for square matrices.")))

(defmethod inverse ((x matrix))
  (let ((result (make-instance 'test :object (change-class x 'test))))
    (setf (object-of x) result)
    (setf (dimensions-of result) (dimensions-of x))
    (setf (calculated-of x) t)
    result))

(defmethod inverse ((self test))
  (object-of self))

;-----

(defmethod slot-value-method ((self test) slot-name)
  (if (calculated-p self)
    (call-next-method)
    (unwind-protect
      (progn
        (setf (symbol-function 'slot-value)
              *old-slot-value*)
        (slot-value (force-evaluation self) slot-name))
      (setf (symbol-function 'slot-value)
            *new-slot-value*))))

;----------------------------------------------------------------------------

(defmethod ref :after ((self test) &rest args)
  (setf (calculated-of self) t))

(defmethod eref :after ((self test) &rest args)
  (setf (calculated-of self) t))

(defmethod sel :after ((self test) &rest args)
  (setf (calculated-of self) t))

;-----

(defgeneric force-evaluation (self))

(defmethod force-evaluation ((self t))
  self)

(defmethod force-evaluation ((self test))
  (if (calculated-p self)
    (call-next-method)
    (let ((inv (calculate-inverse (object-of self))))
      (with-slots ((self-proto         proto) 
                   (self-ref-obj       ref-obj)
                   (self-specs-mask    specs-mask)
                   (self-specs         specs) 
                   (self-dimensions    dimensions)
                   (self-ref-contents  ref-contents)
                   (self-decomposition decomposition)
                   (self-calculated?   calculated?)) self
        (with-slots ((inv-proto proto) 
                     (inv-ref-obj       ref-obj)
                     (inv-specs-mask    specs-mask)
                     (inv-specs         specs) 
                     (inv-dimensions    dimensions)
                     (inv-ref-contents  ref-contents)
                     (inv-decomposition decomposition)) inv
          (psetf self-proto         inv-proto
                 self-ref-obj       inv-ref-obj
                 self-specs-mask    inv-specs-mask
                 self-specs         inv-specs
                 self-dimensions    inv-dimensions
                 self-ref-contents  inv-ref-contents
                 self-decomposition inv-decomposition
                 self-calculated?   t)))
      self)))

;-----

(defmethod dot-times-object :around ((x test) (y test))
  (if (or (eql x (object-of y))
          (eql y (object-of x)))
    (make-identity-matrix (first (dimensions-of x)))
    (call-next-method)))

;-------------------------------------------------------------------------
;DEFCLASS: identity-matrix
;-------------------------------------------------------------------------

(defclass identity-matrix (matrix)
  ())

(defun make-identity-matrix (extent)
  (array (list extent extent) 
         :initial-contents
         (loop for i from 1 to extent
               collect
               (loop for j from 1 to extent
                     collect
                     (if (= i j) 1 0)))
         :class 'identity-matrix))

(defmethod inverse ((self identity-matrix))
  self)

;-------------------------------------------------------------------------
;DEFUN: calculate-inverse
;-------------------------------------------------------------------------

(defun calculate-inverse (x)
  (let ((lu (lu-of x)))
    (if (= 1.0 (+ 1.0 (eref (rcond-of lu))))
      (quail-error "Matrix is singular to working precision.")
      (let* ((a (sel (a-of lu)))
             (ipvt (ipvt-of lu))
             (det (array (list 2) :initial-element 0))
             (job 1))
        (dgedi a ipvt det job)
        a))))

;-----
