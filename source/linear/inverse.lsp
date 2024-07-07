;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                              inverse.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1994 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     Michael Lewis 1991.
;;;     R.W. Oldford 1994.
;;;
;;;
;;;--------------------------------------------------------------------------------
;;;
(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(inverse-matrix inverse inverse-object-of)))

(defgeneric inverse-object-of (thing)
  (:documentation
   "Returns the cached inverse of thing.  Cannot be used with setf to ~
    set the cache if one exists."))

(defmethod inverse-object-of ((thing t))
  (if (quail-y-or-n-p "INVERSE-OBJECT-OF: No inverse-object cache exists for ~s, ~
                       perhaps you meant to take the inverse?" thing)
    (inverse thing)
    (quail-error
     "INVERSE-OBJECT-OF: No inverse-object cache exists for ~s."
     thing)
    ))

(defmethod (setf inverse-object-of) (new-value (thing t))
  (declare (ignore new-value))
  (quail-error
   "Cannot directly set the cached inverse-object of thing."
   thing))


(defclass inverse (quail-object)
  ((inverse-object :initarg :inverse-object
                   :reader inverse-object-of
                   :initform :NONE)
   (inverse-calculated? :initarg :inverse-calculated?
                        :initform NIL
                        :reader inverse-calculated-p))
  (:documentation "A general class representing the mathematical ~
                   inverse of something. It's primary use is to delay ~
                   evaluation of the actual inverse and to provide ~
                   programmatic opportunity to use the inverse in ~
                   a numerically reliable fashion.~
                   (:see-also (inverse :generic-function)~
                   )")
  )


(defclass inverse-matrix (inverse matrix)
  ()
  (:documentation "The matrix inverse of another matrix.  ~
                   (:see-also (inverse :generic-function))")
  )

;;;
;;; Make sure that matrix is the returned class type when inverse-matrix
;;; is combined with any of the list
;;;

(qk::put-return-class 'matrix
                      'inverse-matrix
                      '(inverse-matrix matrix num-array ref-array 
                        vector cons sequence list array
                        integer fixnum rational float complex
                        symbol))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  The inverse function
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric inverse (object)
  (:documentation
   "Returns the mathematical inverse of the object.~
    It may suppress explicit determination of the inverse for numerical ~
    reasons until the result is actually needed in calculation.  ~
    (:see-also (inverse :generic-function)~
    (solve :function))"))

(defmethod inverse ((object t))
  "Not defined in general; missing-method invoked."
  (missing-method 'inverse object))


(defmethod inverse ((self inverse))
  "Returns inverse-object-of self."
  (inverse-object-of self))

(defmethod inverse ((x number))
  (cl:/ x))

(defmethod-multi inverse ((x ((eql infinity)
                              (eql +infinity)
                              (eql -infinity)
                              (eql NaN))))
  (/ x))

(defmethod inverse :around ((x matrix))
  "Checks dimensions of x to make sure that it is square."
  (let*
    ((dims (dimensions-of x))
     (p (length dims)))
    (cond
     ((null dims) (inverse (eref x)))
     ((and (= p 1) (= 1 (first dims)))
      (inverse (eref x 0)))
     ((and (= p 2) (apply #'cl::= dims))
      (call-next-method))
     (T (quail-error "INVERSE: Matrix ~s not square." x)))
    ))



(defmethod inverse ((x matrix))
  "Returns the inverse-matrix of the square matrix x. ~
   If x is non-square it calculates and returns the Moore-Penrose ~
   generalized inverse of x. ~
   (:see-also (solve :function) (qr-solution :topic))~
   "
  (if (cl:= (nrows x) (ncols x))
    (let ((result (make-instance
                    'inverse-matrix 
                    :inverse-object x)))
      (setf (dimensions-of result) (dimensions-of x))
      result)
    (inverse (qrd-of x))))

(defmethod inverse ((lu lu-decomposition))
  (if (cl::=
       1.0
       (cl::+ 1.0 (eref (rcond-of lu))))
    (quail-error "Matrix is singular to working precision.")
    (let* ((a (sel (lu-of lu)))
           (n (first (dimensions-of a)))
           (ipvt (ipvt-of lu))
           (det (array 0.0D0 :dimensions (list 2)))
           (work (array 0.0D0 :dimensions (list n)))
           (job 1))
      (dgedi a n ipvt det work job)
      a)))


(defmethod inverse ((c cholesky-decomposition))
  (cond
   ((jpvt-of c)
    ;; a pivoted cholesky
    (quail-error
     "Inverse not yet implemented for a pivoted Cholesky decomposition.~
      ~&Try using an unpivoted decomposition first.")
    )
   (T
    ;; an unpivoted cholesky
    (if (or (not (zerop (info-of c)))
            (cl::=
             1.0
             (cl::+ 1.0 (eref (rcond-of c)))))
      (quail-error "Inverse of Cholesky: ~
                    Matrix is singular to working precision.")
      (let* ((a (sel (upper-triangle-of c)))
             (n (first (dimensions-of a)))
             (det (array 0.0D0 :dimensions (list 2)))
             (job 1))

        ;; calculate the inverse from LINPACK
        (dpodi a n det job)

        ;; copy the top into the bottom.
        (loop
          for i from 0 below n
          do
          (loop
            for j from 0 below i
            do 
            (setf (eref a i j) (eref a j i))
            )
          )
        a)))
   )
  )

(defmethod inverse ((e eigen-decomposition))
  (let* ((V (eigen-vectors-of e))
         (D (eigen-values-of e))
         (p (nrows V))
         (result (array 0.0 :dimensions (list p p)))
         tmp)
    (do ((i 0 (incf i)))
        ((= i p))
      (do ((j i (incf j)))
          ((= j p))
        (setf tmp 0)
        (do ((k 0 (incf k)))
            ((= k p))
          (setf tmp (+ tmp (/ (* (eref V i k) (eref V j k))
                              (eref D k))))
          )
        (setf (eref result i j) tmp)
        (setf (eref result j i) tmp)
        )
      )
    result))

;;;
;;;  request-inverse-evaluation
;;;
(defgeneric request-inverse-evaluation (inverse)
  (:documentation "Force inverse to actually calculate itself if it hasn't ~
                   already done so. (:see-also (inverse :generic-function))")
  )

;;; Add do-nothing primary FEB 04 1998
(defmethod request-inverse-evaluation  ((self inverse))
   (declare (ignorable self)) ;(declare (ignore self)) ; 31JUL2023
   (call-next-method))

(defmethod request-inverse-evaluation :around ((self inverse))
  "Checks to see whether the evaluation has already been done.~
   If it has then it returns self, if it has not then it calls the ~
   next method."
  (if (inverse-calculated-p self)
    self
    (call-next-method)))

(defmethod request-inverse-evaluation ((self t))
  "In general does nothing but return self."
  self)

(defmethod request-inverse-evaluation ((self inverse-matrix))
  (unless (eq (inverse-object-of self) :NONE)
      (let ((inv (inverse
                  (lud-of
                   (inverse-object-of self)))))
        (with-slots ((self-proto                 qk::proto) 
                     (self-ref-obj               qk::ref-obj)
                     (self-specs-mask            qk::specs-mask)
                     (self-specs                 qk::specs) 
                     (self-dimensions            qk::dimensions)
                     (self-ref-contents          qk::ref-contents)
                     (self-decomposition         decomposition)
                     (self-inverse-calculated?   inverse-calculated?))
                    self
          (with-slots ((inv-proto         qk::proto) 
                       (inv-ref-obj       qk::ref-obj)
                       (inv-specs-mask    qk::specs-mask)
                       (inv-specs         qk::specs) 
                       (inv-dimensions    qk::dimensions)
                       (inv-ref-contents  qk::ref-contents)
                       (inv-decomposition decomposition))
                      inv
            (setf self-proto         inv-proto
                  self-ref-obj       inv-ref-obj
                  self-specs-mask    inv-specs-mask
                  self-specs         inv-specs
                  self-dimensions    inv-dimensions
                  self-ref-contents  inv-ref-contents
                  self-decomposition inv-decomposition
                  self-inverse-calculated?   t)))
        self))
  )

;;; Add do-nothing primary FEB 04 1998
(defmethod fill-unbound-slots  ((self inverse-matrix))
   (declare (ignorable self)) ;(declare (ignore self)) ; 31JUL2023
   (call-next-method))

(defmethod fill-unbound-slots :after ((self inverse-matrix))
  (labels
    ((best-inverse-object (protos)
       (if protos
         (inverse-object-of (first protos))
         nil))
     (best-inverse-calculated (protos)
       (let ((best-one (best-inverse-object protos)))
         (and best-one (inverse-calculated-p best-one))))
     )
    (with-slots ((protos qk::proto))
                self
      (setf protos
            (remove-if-not
             #'(lambda (proto)
                 (typep proto 'inverse-matrix))
             protos))
      (if (qk::fill-slot-p self 'inverse-object)
        (setf (slot-value self 'inverse-object)
              (best-inverse-object protos)))
      (if (qk::fill-slot-p self 'inverse-calculated?)
        (setf (slot-value self 'inverse-calculated?)
              (best-inverse-calculated protos)))))
  )

;-----

(defmethod ref ((self inverse-matrix) &rest args)
  (declare (ignore args))
  (request-inverse-evaluation self)
  (let ((result (call-next-method)))
    (change-class result 'matrix)
    result))

;-----

(defmethod sel ((self inverse-matrix) &rest args)
  (declare (ignore args))
  (request-inverse-evaluation self)
  (let ((result (call-next-method)))
    (change-class result 'matrix)
    result))

;-----

(defmethod eref ((self inverse-matrix) &rest args)
  (declare (ignore args))
  (request-inverse-evaluation self)
  (call-next-method))

;-----

(defmethod (setf ref) (new-value (self inverse-matrix) &rest args)
  (declare (ignore args new-value))
  (request-inverse-evaluation self)
  (let ((result (call-next-method)))
    (change-class self 'matrix)
    result))

;-----

(defmethod (setf eref) (new-value (self inverse-matrix) &rest args)
  (declare (ignore args new-value))
  (request-inverse-evaluation self)
  (let ((result (call-next-method)))
    (change-class self 'matrix)
    result))

;-----

(defmethod print-object ((self inverse-matrix) stream)
  (if (inverse-calculated-p self)
    (call-next-method)
    (format stream "#<Matrix inverse of ~A>" (inverse-object-of self))))


