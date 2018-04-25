;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               fit.lisp                              
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1992 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     Greg Anglin 1992-94.
;;;     ... rwo 95
;;;
;;;--------------------------------------------------------------------------------
;;;

(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(fit-object response-model-fit fit
          fit-using-methodology fit-instantiate fit-class)))

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(model-of data-frame-of model-frame-of model-matrix-of response-matrix-of
          model-degrees-of-freedom-of weight-of coef-of resid-of pred-of deviance-of)))

(defclass fit-object (quail-object)
  ((model :accessor model-of :initarg :model
          :documentation "The model-object which was fitted.")
   (data :accessor data-of :initarg :data
               :documentation "The data to which the model was fitted"))
  (:documentation 
   "The abstract class at the top of the hierarchy of fitted model ~
    objects. ~
    (:see-also ~
    (fit-object :class) ~
    (fit :generic-function) ~
    (fit-class :generic-function) ~
    (fit-instantiate :generic-function) ~
    ) ~
    ")
  )

(defmethod formula-of ((self fit-object))
  (formula-of (model-of self)))

;;; there's probably too much junk on this class.

(defclass response-model-fit (fit-object)
  ((model-frame :accessor model-frame-of :initarg :model-frame
                :documentation
                "A model-frame built from the model and data ~
                 which were given to fit.")
   (model-matrix :accessor model-matrix-of :initform nil
                 :documentation 
                 "A numerical model-matrix of predictors built from model-frame.")
   (response-matrix :accessor response-matrix-of :initform nil
                    :documentation 
                    "A numerical matrix of the response built from model-frame.")
   ;;(rank ;; see defmethods below :accessor rank-of
   ;;      :documentation "The rank of model-matrix")
   (model-degrees-of-freedom 
    :accessor model-degrees-of-freedom-of
    :documentation "The degrees of freedom for the model ~
                    (i.e. the column rank of the model-matrix).")
   (weight :accessor weight-of :initarg :weight
           :documentation "Vector of initial weights for observations.")
   (coef :initform nil :accessor coef-of
         :documentation "Vector of fitted coefficients.")   
   (resid :initform nil :accessor resid-of
          :documentation "Residual vector.")  
   (pred :initform nil :accessor pred-of
         :documentation "The fitted values of the response at each ~
                         predictor-vector under the model.")
   (deviance :accessor deviance-of
             :documentation "The deviance of the fitted model."))
  (:documentation
   "The class of fitted model objects for models ~
    which have a response separable from predictors. ~
    In practice, only particular subclasses of response-model-fit ~
    are instantiated."))

#|
(defmethod rank-of ((thing response-model-fit) &key &allow-other-keys)
  "The rank of the model-matrix."
  (slot-value thing 'rank))

(defmethod (setf rank-of) (new-value
                           (thing response-model-fit)
                           &key &allow-other-keys)
  "Set the rank of the model-matrix."
  (setf (slot-value thing 'rank) new-value))
|#
(defmethod data-frame-of ((fit response-model-fit))
  "An alternative accessor to the data on a fitted response model object, ~
   for which objects the data is usually of class data-frame."
  (data-of fit))

(defmethod (setf data-frame-of) (new-value (fit response-model-fit))
  "An alternative accessor to the data on a fitted response model object, ~
   for which objects the data is usually of class data-frame."
  (setf (data-of fit) new-value))

(defgeneric fit-class (model data methodology)
  (:documentation
   "Returns the class of a fitted model object ~
    suitable for use with model when fitting by technique specified by methodology. ~
    The class may be returned as a class or a symbol naming a class. ~
    (:required ~
    (:arg model An instance of a subclass of model-object) ~
    (:arg data An instance of a subclass of data-object) ~
    (:arg methodology A specification of a methodology for fitting ~
    (usually a keyword such as :maximum-likelihood or :least-squares)) ~
    )~
    (:see-also ~
    (fit-object :class) ~
    (fit :generic-function) ~
    (fit-using-methodology :generic-function) ~
    (fit-instantiate :generic-function) ~
    ) ~
    ")
  )

(defmethod fit-class ((model model-object) (data data-object) (methodology t))
  "The default fit-class method ... should never really be used in practice."
  'fit-object)

(defmethod fit-class ((model response-model) (data data-object) (methodology t))
  "The fit-class method for response-model in general ... probably will never ~
   be used in practice."
  'response-model-fit)

(defgeneric fit-instantiate (model 
                            data
                            methodology
                            &rest keys
                            &key
                            &allow-other-keys)
  (:documentation
   "Makes and initializes an instance of a subclass ~
    of fit-object to represent the fit of ~
    model to data by methodology. ~
    fit-instantiate is usually called by fit-using-methodology, but may also ~
    be called in advance of fit-using-methodology. ~
    (:required ~
    (:arg model An instance of a subclass of model-object) ~
    (:arg data An instance of a subclass of data-object) ~
    (:arg methodology A specification of a methodology for fitting ~
    (usually a keyword such as :maximum-likelihood or :least-squares)) ~
    ) ~
    (:see-also ~
    (fit-object :class) ~
    (fit :generic-function) ~
    (fit-using-methodology :generic-function) ~
    (fit-class :generic-function) ~
    ) ~
    ")
  )

(defmethod fit-instantiate ((model response-model) 
                            (data data-frame)
                            (methodology t)
                            &rest keys
                            &key 
                            (model-matrix nil model-matrix-provided-p)
                            (model-frame nil model-frame-provided-p)
                            relevant-model-matrix
                            &allow-other-keys)
  "fit-instantiate method for (response-model data-frame t). ~
   (:elaboration Calls fit-class to obtain the ~
   appropriate subclass of fit-object, ~
   makes an instance of it using model, data, and initargs, ~
   and then calls compute-model-matrix ~
   to initialize.) ~
   (:required ~
   (:arg model An instance of a subclass of model-object) ~
   (:arg data An instance of a subclass of data-object) ~
   (:arg methodology A specification of a methodology for fitting ~
   (usually a keyword such as :maximum-likelihood or :least-squares)) ~
   )"
  (setf model-frame (if model-frame-provided-p
                      model-frame
                      (model-frame (formula-of model)
                                   data)))
  (let* ((class (fit-class model data methodology))
         (fit
         (apply #'make-instance
                class
                :model model
                :data data
                :model-frame model-frame
                (qk::remove-illegal-make-instance-initargs class keys))))
    (if model-matrix-provided-p
      (setf (model-matrix-of fit) model-matrix)
      (compute-model-matrix fit relevant-model-matrix)   ;; do degrees of freedom, too.
      )
    fit))

(defgeneric fit (model data 
                       &rest keys 
                       &key methodology tolerance fit-object
                       &allow-other-keys)
  (:documentation
   "Performs a fit of model to data, producing a fit-object. ~
    (:elaboration ~
    Usually implemented as a call to fit-using-methodology, viz. ~
    (apply #'fit-using-methodology model data methodology keys). ~
    The default for methodology is :default, which must be handled by ~
    fit-using-methodology.) ~
    (:required ~
    (:arg model An instance of a subclass of model-object) ~
    (:arg data An instance of a subclass of data-object) ~
    ) ~
    (:keys ~
    (:arg methodology :default A specification of a methodology for fitting ~
    (usually a keyword such as :maximum-likelihood or :least-squares)) ~
    (:arg tolerance nil The tolerance to use in assessing convergence) ~
    (:arg fit-object nil If present, a previously created fit-object for ~
    (these specific instances of) model and data) ~
    ) ~
    (:allow-other-keys)
    (:see-also ~
    (fit-object :class) ~
    (fit-using-methodology :generic-function) ~
    (fit-class :generic-function) ~
    (fit-instantiate :generic-function) ~
    ) ~
    ")
)


(defmethod fit ((model model-object) (data T)
                &rest keys
                &key (methodology :default)
                &allow-other-keys)
  "The method for fit to be used when the data is not of class data-object."
  (let* ((variates (list-variates data))
         (data-frame (data-frame data (copy-list variates)))
         )
    (dataset data-frame :variates variates :identifiers (list-identifiers data))
    (apply #'fit-using-methodology model data-frame methodology keys))
  )

(defmethod fit ((model model-object) (data data-object)
                &rest keys
                &key (methodology :default)
                &allow-other-keys)
  "The default method for fit which simply does ~
   (apply #'fit-using-methodology model data methodology keys)."
  (apply #'fit-using-methodology model data methodology keys))

(defmethod fit ((model (eql t)) (data (eql t))
                &rest keys
                &key fit-object
                &allow-other-keys)
  "A method for fit which allows fit to be called with model==T and ~
   data==T, so long as fit-object is provided."
  (if fit-object
    (apply #'fit
           (model-of fit-object)
           (data-of fit-object)
           keys)
    (error "Can't fit model ~S to data ~S with fit-object ~S."
           model data fit-object)))

(defgeneric fit-using-methodology
  (model data methodology &rest keys
         &key tolerance fit-object &allow-other-keys)
  (:documentation
   "Performs a fit of model to data by methodology, producing a fit-object. ~
    (:elaboration ~
    Implementors may need to provide a method for a specific model and data ~
    class and (methodology (eql :default)), which redispatches to ~
    fit-using-methodology with a default appropriate for that model and data. ~
    The default is to redispatch with methodology set to :maximum-likelihood. ~
    If fit-object is NIL or not provided, a new fit-object instance is ~
    created by fit-instantiate, and will have class as returned by fit-class. ~
    If fit-object is provided, it must be an instance previously created by fit-instantiate ~
    called with these specific instances of model, data, and methodology (where ~
    methodology is by now something other than :default).) ~
    (:required ~
    (:arg model An instance of a subclass of model-object) ~
    (:arg data An instance of a subclass of data-object) ~
    (:arg methodology A specification of a methodology for fitting ~
    (usually a keyword such as :maximum-likelihood or :least-squares)) ~
    ) ~
    (:keys ~
    (:arg methodology :ignored This keyword is often passed from fit, ~
    but is ignored by fit-using-methodology) ~
    (:arg tolerance nil The tolerance to use in assessing convergence) ~
    (:arg fit-object nil If present, a previously created fit-object for ~
    (these specific instances of) model and data) ~
    ) ~
    (:allow-other-keys) ~
    (:see-also ~
    (fit-object :class) ~
    (fit :generic-function) ~
    (fit-class :generic-function) ~
    (fit-instantiate :generic-function) ~
    ) ~
    ")
)

(defmethod fit-using-methodology ((model model-object)
                                  (data data-object)
                                  (methodology (eql :default))
                                  &rest keys
                                  &key 
                                  &allow-other-keys)
  "The default method for fit which simply does ~
   (apply #'fit-using-methodology model data :maximum-likelihood keys)."
  (apply #'fit-using-methodology model data :maximum-likelihood keys))

