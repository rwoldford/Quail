;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                           gam.lisp                              
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1992 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     Greg Anglin 1992.
;;;
;;;--------------------------------------------------------------------------------
;;;

(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(generalized-additive-model additive-model
          generalized-additive-model-fit additive-model-fit
          formula-of family-of link-of weight-fn-of)))

(defclass generalized-additive-model (response-model)
  ((formula :accessor formula-of :initarg :formula :type additive-formula
            :documentation "An object of class additive-formula.")
   (link :accessor link-of :initarg :link :initform identity-link
         :documentation 
         "A link-object providing the link function info. ~
          Defaults to identity link.")
   (family :accessor family-of :initarg :family :initform gaussian-family
           :documentation 
           "A family object containing family info such as ~
            variance function, deviance function, etc. ~
            Defaults to gaussian family.")
   (weight-fn :accessor weight-fn-of :initarg :weight-fn
              :documentation
              "The weight function; if not provided, is calculated from ~
               variance function and derivative of link.")
   (offset :accessor offset-of :initarg :offset :initform 0
           :documentation 
           "see McCullagh & Nelder"))
  (:documentation
   "Generalized additive models of Tibshirani & Hastie (1991). ~
    FITTING CURRENTLY UNIMPLEMENTED. ~
    (:elaboration In this class, the formula is restricted to be of class ~
    additive-formula) ~
    "))

(defmethod-multi link-of ((link-identifier (symbol string)))
  (link-of (find-link link-identifier)))

(defmethod-multi family-of ((family-identifier (symbol string)))
  (family-of (find-family family-identifier)))

(defmethod model ((model generalized-additive-model)
                  (formula string)
                  &rest initargs)
  (apply #'model
         model
         (make-instance 'additive-formula :literal formula)
         initargs))

(defmethod model ((model generalized-additive-model)
                  (formula additive-formula)
                  &rest initargs)
  (apply #'reinitialize-instance model :formula formula initargs)
  model)

(defmethod reinitialize-instance :after ((model generalized-additive-model)
                                         &rest initargs)
  (declare (ignore initargs))
  (if (slot-boundp model 'family)
    (setf (slot-value model 'family) (find-family (slot-value model 'family))))
  (if (slot-boundp model 'link)
    (setf (slot-value model 'link) (find-link (slot-value model 'link))))
  (if (and (not (slot-boundp model 'weight-fn))
           (slot-boundp model 'family)
           (slot-boundp model 'link))
    (setf (weight-fn-of model)
          (find-weight-fn (slot-value model 'family)
                          (slot-value model 'link))))
  )

(defclass generalized-additive-model-fit (response-model-fit)
  ()
  (:documentation "The class of fits of generalized additive models.  ~
                   Fits of generalized additive models are CURRENTLY UNIMPLEMENTED."))

(defmethod link-of ((fit-obj generalized-additive-model-fit))
  (link-of (model-of fit-obj)))

(defmethod family-of ((fit-obj generalized-additive-model-fit))
  (family-of (model-of fit-obj)))

(defclass additive-model (generalized-additive-model)
  ((link :accessor link-of 
         :initarg :link
         :initform identity-link
         :allocation :class))
  (:documentation 
   "This class has the link restricted to be the identity."))

(defclass additive-model-fit (generalized-additive-model-fit)
  ()
  (:documentation "The class of fits of additive models.  ~
                   Fits of additive models are CURRENTLY UNIMPLEMENTED."))
