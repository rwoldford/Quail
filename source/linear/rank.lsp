;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                                 rank.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1994 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1994.
;;;
;;;
;;;-----------------------------------------------------------------------------
;;;

(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(rank-of)))


(defgeneric rank-of (thing &key &allow-other-keys)
  (:documentation "Determines and returns the rank of its argument. ~
                   For some methods different keywords may be applicable."
                  ))

(defmethod rank-of ((X T) &key &allow-other-keys)
  (missing-method 'rank-of X))

(defmethod rank-of ((x identity-matrix) &key &allow-other-keys)
  (nrows x))

(defmethod rank-of ((X matrix) &key (tolerance NIL))
  "Returns the numerical rank of X as determined by the number of~
   its non-zero singular-values.  If a positive number tolerance is given, ~
   then the ~
   rank is defined to be the number of singular-values greater than ~
   tolerance. ~
   (:see-also (singular-values-of :generic-function) ~
   )"
  (unless (and (numberp tolerance) (cl:> tolerance 0))
      (setf tolerance 0))
  (let ((rank 0)
        (svs (singular-values-of X)))
    (loop
      for i
      from 0 below (number-of-elements svs)
      when (cl:> (eref svs i) tolerance)
      do (incf rank))
    rank))

(defmethod rank-of ((X sv-decomposition) &key (tolerance NIL))
  "Returns the numerical rank of X as determined by the number of~
   its non-zero singular-values.  If a positive number tolerance is given, ~
   then the ~
   rank is defined to be the number of singular-values greater than ~
   tolerance. ~
   (:see-also (singular-values-of :generic-function) ~
   )"
  (unless (and (numberp tolerance) (cl:> tolerance 0))
    (setf tolerance 0))
  (let ((rank 0)
        (svs (singular-values-of X)))
    (loop
      for i
      from 0 below (number-of-elements svs)
      when (cl:> (eref svs i) tolerance)
      do (incf rank))
    rank)
  )

(defmethod rank-of ((qrd qr-decomposition) &key (tolerance NIL))
  "Returns the numerical rank of the original matrix ~
   from its QR decomposition.  It does this by counting the number ~
   of diagonal elements of the triangular R matrix ~
   that are greater than 0. ~
   If a positive number tolerance is given, ~
   then the rank is defined to be the number greater than ~
   tolerance. "
  (unless (and (numberp tolerance)
               (cl:> tolerance 0))
      (setf tolerance 0))
  (with-slots (qr) qrd
    (let ((p (second (dimensions-of qr))))
      (if p
        (loop
          for i from 0 to (1- p) 
          count (cl:> 
                 (cl:abs (eref qr i i))
                 tolerance))
        1))))

(defmethod rank-of ((lu lu-decomposition) &key (tolerance NIL))
  "Returns the numerical rank of the original matrix ~
   from its lu decomposition.  It does this by counting the number ~
   of diagonal elements of the upper triangular matrix U ~
   that are greater than 0. ~
   If a positive number tolerance is given, ~
   then the rank is defined to be the number greater than ~
   tolerance. "
  (unless (and (numberp tolerance)
               (cl:> tolerance 0))
    (setf tolerance 0))
  (let* ((u (lu-of lu))
         (p (second (dimensions-of u))))
    (if p
      (loop
        for i from 0 below p 
        count (cl:> 
               (cl:abs (eref u i i))
               tolerance))
      1)
    ))

(defmethod rank-of ((c cholesky-decomposition) &key (tolerance NIL))
  "Returns the numerical rank of the original matrix ~
   from its Cholesky decomposition.  It does this by querying the ~
   info on the decomposition when the tolerance is zero.  ~
   When the tolerance is > 0, it tries to determine the rank ~
   by counting the number ~
   of diagonal elements of the upper triangular matrix R ~
   that are greater than the tolerance where this is legitimate. ~
   If the operation is known to be illegitimate then NaN is returned and ~
   the user warned. "
  (unless (and (numberp tolerance)
               (cl:> tolerance 0))
    (setf tolerance 0))
  (cond
   ((jpvt-of c)
    ;; have a pivoted cholesky
    (if (zerop tolerance)
      (info-of c)
      (let ((r (upper-triangle-of c))
            (count 0))
        (loop for i from 0 below (nrows r)
              when (cl::> (eref r i i) tolerance)
              do (incf count))
        (min (info-of c) count)))
    )
   ;; else an unpivoted cholesky
   ((zerop (info-of c))
    (if (zerop tolerance)
      (nrows (upper-triangle-of c))
      (let ((r (upper-triangle-of c)))
        (loop for i from 0 below (nrows r)
              count (cl::> (eref r i i) tolerance))
        ))
    )
   (T (warn "Could not determine the rank from this Cholesky ~
             decomposition. ~&Returning NaN.")
      NaN)
   
   )
  )
