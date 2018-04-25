;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                            identity-matrix.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1994 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1994.
;;;
;;;
;;;--------------------------------------------------------------------------------
;;;

(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(identity-matrix)))


;;;-------------------------------------------------------------------------
;;; DEFCLASS: identity-matrix
;;;-------------------------------------------------------------------------

(defclass identity-matrix (matrix)
  ((identity-filled? :initform NIL
                     :reader identity-filled-p))
  (:documentation "The identity matrix.  ~
                   (:see-also ~
                   (identity-matrix :function)~
                   (inverse :generic-function))"))


(defun identity-matrix (extent)
  "Creates and returns an extent by extent identity matrix."
  (make-instance 'identity-matrix :dimensions (list extent extent)))



(defgeneric request-identity-filled (identity-matrix)
  (:documentation "Force identity to actually fill itself if it hasn't ~
                   already done so.")
  )


(defmethod request-identity-filled ((self identity-matrix))
  (let*
    ((extent (or (first (dimensions-of self)) 1))
     (iden
      (array (loop for i from 1 to extent
                   collect
                   (loop for j from 1 to extent
                         collect
                         (if (= i j) 1 0)))
             :dimensions (list extent extent)
             :class 'identity-matrix))
     )
    (with-slots ((self-proto                 qk::proto) 
                 (self-ref-obj               qk::ref-obj)
                 (self-specs-mask            qk::specs-mask)
                 (self-specs                 qk::specs) 
                 (self-dimensions            qk::dimensions)
                 (self-ref-contents          qk::ref-contents)
                 (self-identity-filled?     identity-filled?))
                self
      (with-slots ((iden-proto         qk::proto) 
                   (iden-ref-obj       qk::ref-obj)
                   (iden-specs-mask    qk::specs-mask)
                   (iden-specs         qk::specs) 
                   (iden-dimensions    qk::dimensions)
                   (iden-ref-contents  qk::ref-contents))
                  iden
        (setf self-proto         iden-proto
              self-ref-obj       iden-ref-obj
              self-specs-mask    iden-specs-mask
              self-specs         iden-specs
              self-dimensions    iden-dimensions
              self-ref-contents  iden-ref-contents
              self-identity-filled?   t)))
    self)
  )


(defmethod request-identity-filled :around ((self identity-matrix))
  "Checks to see whether the filling has already been done.~
   If it has then it returns self, if it has not then it calls the ~
   next method."
  (if (identity-filled-p self)
    self
    (call-next-method)))

(defmethod request-identity-filled ((self t))
  "In general does nothing but return self."
  self)


(defmethod ref ((x identity-matrix) &rest args)
  (let ((result (call-next-method))
        (num-args (length args))
        (arg-1 (first args))
        (old-extent (or (first (dimensions-of x)) 1)))
    (unless
      (or (zerop num-args)
          (and (= num-args 1)
               (or (eq (first args) T)
                   (equal (iseq old-extent) arg-1)
                   ))
          (and (= num-args 2)
               (equal arg-1 (second args))
               (or (eq arg-1 T)
                   (numberp arg-1)
                   (and (listp arg-1)
                        (= (length arg-1)
                           (length (remove-duplicates arg-1 :test #'eq))))))
          )
      (change-class result 'matrix))
    result)
  )

(defmethod sel ((x identity-matrix) &rest args)
  (let ((result NIL)
        (num-args (length args))
        (old-extent (or (first (dimensions-of x)) 1)))
    (cond
     ((zerop num-args)
      (setf result (identity-matrix old-extent)))
     ((and (= num-args 1)
           (or (eq (first args) T)
               (equal (iseq old-extent) (first args))
               )
           )
      (setf result (identity-matrix old-extent)))
     ((= num-args 2)
      (let ((arg-1 (first args))
            (arg-2 (second args)))
        (when (equal arg-1 arg-2)
          (cond
           ((eq arg-1 T) (setf result (identity-matrix old-extent)))
           ((numberp arg-1) (identity-matrix 1))
           ((and (listp arg-1)
                 (= (length arg-1)
                    (length (remove-duplicates arg-1 :test #'eq))))
            (if (eq (first arg-1) :c)
              (setf result
                    (identity-matrix (- old-extent
                                        (length (rest arg-1)))))
              (setf result
                    (identity-matrix (length arg-1)))
              ))
           )
          )
        )
      ))
    
    (unless result
      (let ((extent (or (first (dimensions-of x)) 1)))
        (setf result
              (array (loop for i from 1 to extent
                           collect
                           (loop for j from 1 to extent
                                 collect
                                 (if (= i j) 1 0)))
                     :dimensions (list extent extent)))
        (setf result (apply #'sel result args))))
    result))


(defmethod eref ((x identity-matrix) &rest args)
  (if (not (= (length args) 2))
    (quail-error "Referencing elements of ~s requires 2 subscripts; ~
                  eref called with ~s."
                 x
                 args))
  (let* ((i (first args))
         (j (second args))
         (dims (dimensions-of x))
         (n (if dims
              (first dims)
              1)))
    (cond
     ((or (>= i n) (< i 0))
      (quail-error "eref: Index ~s out of bounds for ~s."
                   i
                   x))
     ((or (>= j n) (< j 0))
      (quail-error "eref: Index ~s out of bounds for ~s."
                   j
                   x))
     (T 
      (if (apply #'cl:= args)
        1
        0)))))

(defmethod print-object ((self identity-matrix) stream)
  (let*
    ((dims (dimensions-of self))
     (n (if dims
          (first dims)
          1))
     )
    (format stream "#<~a by ~a Identity Matrix>" n n)))
;-----

(defmethod (setf ref) (new-value (self identity-matrix) &rest args)
  (declare (ignore args new-value))
  (request-identity-filled self)
  (change-class (call-next-method) 'matrix))

;-----

(defmethod (setf eref) (new-value (self identity-matrix) &rest args)
  ;;(declare (ignore args new-value))
  (when (or (null args)
            (and (= (length args) 2)
                 (if (apply #'= args)
                   (/= new-value 1)
                   (/= new-value 0)))
            )
    (Quail-error "Cannot set the ~s element of an identity matrix to ~s."
                 args new-value))
  new-value)


(qk::put-return-class 'matrix
                      'identity-matrix
                      '(inverse-matrix matrix num-array ref-array 
                        vector sequence list array number
                        ;;integer fixnum rational float complex
                        symbol))

(defmethod inverse ((self identity-matrix))
  self)


(defmethod tp ((self identity-matrix) &key perm)
  (declare (ignore perm))
  self)
