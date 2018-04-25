(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(factor-object
          nominal-factor
          ordinal-factor
          interval-factor
          binary-factor
          factor-array
          nominal-factor-array
          ordinal-factor-array
          interval-factor-array
          binary-factor-array
          )))

(defclass factor-object (ref-object)
  ((levels :accessor levels-of :initarg :levels))
  (:documentation 
   "The abstract class of categorical variables. ~
    If a factor-object is instantiated, an instance of subclass nominal-factor ~
    will be created. ~%~
    [levels]  the possible values the variable may take."))

(defmethod num-levels ((factor factor-object))
  (with-slots (levels) factor
    (length levels)))

(defclass nominal-factor (factor-object)
  ()
  (:documentation 
   "A subclass of factor-object for which the levels of the factor are purely nominal; ~
    in models involving these factors, levels should be exchangeable without ~
    impacting inferences."))

(defclass ordinal-factor (factor-object)
  ()
  (:documentation 
   "A restriction of class factor-object to the case when there is an order on the ~
    levels of the factor.  This is encoded as (elt levels i) < (elt levels j) ~
    whenever i < j, but the numerical value of each level in the encoding is ~
    not otherwise meaningful."))

(defclass interval-factor (ordinal-factor)
  ()
  (:documentation 
   "A restriction of class ordinal-factor to the case when the ~
    levels of the factor are numerical. The ~
    numerical value of the factor is meaningful and is used to ~
    encode the factor in a response-matrix or model-matrix."))

(defclass binary-factor (ordinal-factor)
  ()
  (:documentation
   "A restriction of class ordinal-factor to the case when there ~
    are only two levels of the factor.  The first level is always ~
    encoded as 0 and the second level as 1 in a response-matrix ~
    or model-matrix."))
  
(defclass factor-array (ref-array factor-object) ())

(defclass nominal-factor-array (factor-array nominal-factor) ())

(defclass ordinal-factor-array (factor-array ordinal-factor) ())

(defclass interval-factor-array (ordinal-factor-array interval-factor) ())

(defclass binary-factor-array (ordinal-factor-array binary-factor) ())

(defmethod make-ref-array ((self factor-array) dimensions &rest rest)
  ;; allows more specific methods to manipulate levels ..
  ;; cf.  binary-response-vector
  (if (not (slot-boundp self 'levels))
    (multiple-value-bind (levels extra)
                         (qk::interpret-keys-only rest
                                                  '(:levels)
                                                  'make-ref-array
                                                  t)
      (setf (levels-of self) levels)
      (apply #'call-next-method self dimensions extra))
    ;; on this branch, rest has already had :levels removed
    (apply #'call-next-method self dimensions rest)))

(defmethod ref ((r factor-array) &rest args)
  (let ((ref-r (ref-instantiate r)))
    (ref-kernel ref-r r args)))

(defmethod ref-instantiate ((r factor-array))
  (make-instance 'factor-array))

(defmethod ref-kernel ((ref-r factor-array) r args)
  (apply #'call-next-method ref-r r args)
  (setf (levels-of ref-r) (levels-of r)))

(defmethod print-object ((fac factor-array) stream)
  (qk::print-dim-ref-object fac 'FAC stream)
  fac)

#|
;; too confusing ... do later ...

(defmethod as-factor ((x ref-array) &key class levels)
  (typecase factor-specifier
    (null )
    (factor-object (change-class x (class-of factor-specifier))
            (setf (levels-of x) (levels-of factor-specifier))
            x)
    ((standard-class symbol)
     (change-class x factor-specifier)
     (set-levels x factor-levels)
     x)))

|#

