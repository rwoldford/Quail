;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                           linear-model.lisp                              
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1992 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     Greg Anglin 1992.
;;;     R.W. Oldford 1993, 1995
;;;
;;;--------------------------------------------------------------------------------
;;;

(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(linear-model linear-model-fit 
          gaussian-linear-model gaussian-linear-model-fit
          lm)))

(defclass linear-model (generalized-linear-model additive-model)
  ()
  (:documentation 
   "As a subclass of both generalized-linear-model and additive-model, ~
    this class has the link restricted to be the identity and the formula ~
    restricted to be linear."))

(defclass gaussian-linear-model (linear-model)
  ((family :accessor family-of 
         :initarg :family
         :initform gaussian-family
         :allocation :class))
  (:documentation 
   "In this class the family is restricted to be Gaussian (Normal)."))

(defclass linear-model-fit (generalized-linear-model-fit
                            additive-model-fit)
  ())


(defclass gaussian-linear-model-fit (linear-model-fit)
  ((qr-solution :accessor qr-solution-of
                :initarg qr-solution
                :initform NIL
                :documentation "Contains an instance of a qr-solution ~
                                which in turn contains all the qr decomposition ~
                                information relevant to the least-squares fit.")
   )
  (:documentation "The result of fitting a Gausian linear model."))

(defmethod fit-class ((model linear-model)
                      (data data-frame)
                      (method (eql :maximum-likelihood)))
  'linear-model-fit)

(defmethod-multi fit-class ((model gaussian-linear-model)
                            (data data-frame)
                            (method ((eql :least-squares)
                                     (eql :maximum-likelihood))))
  'gaussian-linear-model-fit)

(defun lm (model-formula 
           data-frame
           &rest keys
           &key
           (family gaussian-family)
           (methodology :least-squares)
           weight
           (tolerance 1e-7)
           &allow-other-keys)
  (declare (ignore tolerance weight methodology))
  ;; the following is now handled in fit ... rwo
  ;;(when (not (typep data-frame 'data-frame))
  ;;  (setf data-frame (data-frame data-frame
  ;;                               (copy-list (list-variates data-frame))))
    
  (let (model)
    (setf model (if (eq family gaussian-family)
                  (model 'gaussian-linear-model
                         model-formula)
                  (model 'linear-model
                         model-formula)))
    (apply #'fit
           model
           data-frame
           :resid t
           :coef t
           :pred t
           :qy t
           :qty t
           keys
           )
    ;; returns fit
    ))

(defmethod fit-using-methodology ((model gaussian-linear-model)
                                  (data data-frame)
                                  (methodology (eql :least-squares))
                                  &rest keys
                                  &key 
                                  fit-object
                                  weight
                                  tolerance coef resid pred qy qty
                                  &allow-other-keys)
  (declare (ignore weight tolerance coef resid pred qy qty))
  (if (not fit-object)
    (setf fit-object (apply #'fit-instantiate
                            model
                            data
                            methodology
                            keys)))
  (apply #'lsfit fit-object :pivot t :resid t keys)
  ;;
  ;; cheat here and dodge the overhead of #'deviance
  ;;
  (with-slots (deviance resid weight) fit-object
    (setf deviance (.* (tp resid) (* weight resid))))
  fit-object)

(defmethod fit-using-methodology ((model gaussian-linear-model)
                                  (data data-frame)
                                  (methodology (eql :maximum-likelihood))
                                  &rest keys
                                  &key 
                                  &allow-other-keys)
  (apply #'fit-using-methodology model data :least-squares keys))

(defmethod-multi compute-residuals ((fit-obj gaussian-linear-model-fit)
                                    (type ((eql :default)
                                           (eql :response)))
                                    &key)
  ;; the fit method for these fit-objs is assumed to have filled resid ...
  (with-slots
    (resid)
    fit-obj
    resid))
