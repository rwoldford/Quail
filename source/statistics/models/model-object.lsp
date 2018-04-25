;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                           model-object.lisp                              
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1992 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     Greg Anglin 1992, 1995.
;;;
;;;
;;;--------------------------------------------------------------------------------
;;;

(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(model-object response-model model)))

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(formula-of)))

(defclass model-object (quail-object)
  ()
  (:documentation 
   "Abstract class at the top of the hierarchy of model objects."))

(defclass response-model (model-object)
  ((formula :accessor formula-of :initarg :formula
            :documentation "an object of class formula.")
   (predictor-fn :accessor predictor-fn-of :initarg :predictor-fn
                 :documentation "a function producing the value of the predictor ~
               (eta) from predictor values provided by a single argument ~
                of class fit-object and optional keyword args."))
  (:documentation 
   "The class of models for which the response variable is separable from the ~
    predictor variables."
    ))

(defmethod print-object ((rm response-model) stream)
  (cond ((and (slot-boundp rm 'formula)
              (slot-boundp (formula-of rm) 'literal))
         (format stream "#<~S ~S>"
                 (class-name (class-of rm))
                 (literal-of (formula-of rm))))
        (t
         (format stream "#<~S ~S>"
                 (class-name (class-of rm))
                 (qk::system-get-pointer rm)))))
        
;;  a quick attempt at a model-maker

(defgeneric model (model formula
                         &rest initargs)
  (:documentation 
   "Initializes a model object of class specified by model ~
    with associated formula. ~
    (:elaboration Initializes and returns an instance of a subclass of model-object ~
    corresponding to model, formula, and (method-specific) initargs.) ~
    (:required ~
    (:arg model ~
    The argument model ~
    may be either a symbol naming a subclass of model-object or an instance of a ~
    subclass of model-object requiring initialization. When model ~
    is a symbol, the generic function is applied again to arguments (make-instance ~
    model) formula initargs.  When model is an instance of a subclass of ~
    model-object, this same instance is returned by the generic function, fully ~
    initialized.) ~
    (:arg formula ~
    The argument formula ~
    may be either a string specifying a model formula or an instance of a subclass ~
    of formula-object.  When formula is a string, the generic function is applied ~
    again to args model, an instance of a subclass of formula-object, ~
    and initargs.  The appropriate subclass is determined by the class of ~
    argument model and by the nature of the formula itself.) ~
    )~
    (:rest ~
    initargs ~
    These are parsed from the &rest list by individual methods. ~
    which methods determine which initargs are applicable.) ~
    ")
)

(defmethod model ((model symbol) formula &rest initargs)
  "This method handles the case when model is a symbol naming a subclass ~
   of model-object. (:elaboration This method simply applies the model generic ~
   function to args (make-instance ~
   model) formula initargs.  Usually at this point formula is a string.)"
  (apply #'model (make-instance model) formula initargs))

(defmethod model ((model standard-class) formula &rest initargs)
  "This method handles the case when model is a class which is a subclass ~
   of model-object. (:elaboration This method simply applies the model generic ~
   function to args (make-instance ~
   model) formula initargs.  Usually at this point formula is a string.)"
  (apply #'model (make-instance model) formula initargs))
