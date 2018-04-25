;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                           glm.lisp                              
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1992 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     Greg Anglin 1992.
;;;     ... rwo 93, 95
;;;     ... dga 94
;;;
;;;--------------------------------------------------------------------------------
;;;

(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(generalized-linear-model generalized-linear-model-fit glm)))

(defclass generalized-linear-model (generalized-additive-model)
  ((formula :accessor formula-of :initarg :formula :type linear-formula
            :documentation "An object of class linear-formula."))
  (:documentation
   "In this class the formula is restricted to be of class linear-formula. "))

(defmethod model ((model generalized-linear-model)
                  (formula string)
                  &rest initargs)
  (apply #'model
         model
         (make-instance 'linear-formula :literal formula)
         initargs))

(defmethod model ((model generalized-linear-model)
                  (formula linear-formula)
                  &rest initargs)
  (apply #'reinitialize-instance model :formula formula initargs)
  model)

(defclass generalized-linear-model-fit (generalized-additive-model-fit)
  ())

(defmethod fit-class ((model generalized-linear-model)
                      (data data-frame)
                      (method (eql :maximum-likelihood)))
  'generalized-linear-model-fit)

(defmethod initialize-instance :after ((fit generalized-linear-model-fit)
                                       &rest initargs)
  (declare (ignore initargs))
  (if (and (slot-boundp fit 'data)
           (not (slot-boundp fit 'weight)))
    (setf (slot-value fit 'weight) 
          (array 1
                 :dimensions (list (data-size (slot-value fit 'data)))))))

(defmethod deviance ((fit-obj generalized-linear-model-fit) &key)
  (with-slots
    (model (mu pred) (y response-matrix) (w weight)) fit-obj
    (with-slots (family) model
      (deviance family :mu mu :y y :w w))))

(defmethod compute-residuals ((fit-obj generalized-linear-model-fit)
                              (type (eql :default))
                              &key)
  (compute-residuals fit-obj :deviance))

(defun glm (model-formula 
            data-frame
            &rest keys
            &key 
            (weight nil weight-provided-p)
            (methodology :maximum-likelihood)
            (offset nil offset-provided-p)
            (family nil family-provided-p)
            (link nil link-provided-p)
            (weight-fn nil weight-fn-provided-p)
            (max-iter 50)
            (tolerance 1e-7)
            (start nil start-provided-p)
            (verbose *terminal-io*))
  (declare (ignore keys))
  ;; the following is now handled in fit ... rwo
  ;;(when (not (typep data-frame 'data-frame))
  ;;  (setf data-frame (data-frame data-frame
  ;;                               (copy-list (list-variates data-frame)))))
  (let (model-initargs model fit)
    (setf model-initargs (mapcan #'(lambda (pred key value)
                                    (and pred (list key value)))
                                (list family-provided-p
                                      link-provided-p
                                      weight-fn-provided-p
                                      offset-provided-p)
                                (list :family :link :weight-fn :offset)
                                (list family link weight-fn offset)))
    (setf model (apply #'model
                       'generalized-linear-model
                       model-formula
                       model-initargs))
    (apply #'fit
           model
           data-frame
           :fit-object fit
           :methodology methodology
           :max-iter max-iter
           :tolerance tolerance
           :verbose verbose
           (append 
            (and weight-provided-p (list :weight weight))
            (and start-provided-p (list :start start))))
    ;; returns fit
    ))

(defun ensure-zero-weights-effective (weight 1d-object)
  (loop for i upto (1- (first (dimensions-of weight)))
        when (zerop (eref weight i))
        do (setf (eref 1d-object i) 0.0))
  1d-object)
    
(defmethod fit-using-methodology ((model generalized-linear-model)
                                  (data data-frame)
                                  (methodology (eql :maximum-likelihood))
                                  &rest keys
                                  &key 
                                  fit-object
                                  weight
                                  (max-iter 50)
                                  (tolerance 1e-3)
                                  (start nil start-provided-p)
                                  (verbose *terminal-io*)
                                  &allow-other-keys)
  (declare (ignore weight))
  (if (not fit-object)
    (setf fit-object (apply #'fit-instantiate
                            model
                            data
                            methodology
                            keys)))
  (if (eq verbose t)
    (setf verbose *terminal-io*))
  (with-slots 
    (model-frame
     model-matrix
     response-matrix
     weight
     model
     pred
     coef
     resid
     (df model-degrees-of-freedom)
     ;;rank
     deviance)
    fit-object
    (with-slots 
      (family
       (link-obj link)
       weight-fn
       offset)
      model
      (multiple-value-setq (response-matrix weight start)
        (initialize-response family
                             ;; get rid of unimportant NaNs etc, right now.
                             (ensure-zero-weights-effective weight
                                                             response-matrix)
                             weight))
      (with-slots 
        (link inverse-link)
        link-obj
        (let (eta mu deriv)
          (if start-provided-p
            (progn
              (setf eta start)
              (setf mu (fn-call inverse-link eta)))
            (progn
              (setf mu start)
              (setf eta (fn-call link mu))))
          (setf deriv (find-deriv link))
          (let* ((new-dev (deviance family
                                    :mu mu 
                                    :y response-matrix
                                    :w weight))
                 (old-dev (cl:+ 10 (cl:* 10 new-dev)))
                 (iter 0)
                 (wls-model (make-instance 'gaussian-linear-model))
                 (wls-fit (fit-instantiate wls-model
                                           model-frame
                                           :least-squares
                                           :model-frame model-frame
                                           :model-matrix model-matrix
                                           ))
                 deriv-mu
                 converged)
            (with-slots 
              ((zmm model-matrix)
               (z response-matrix)
               (zw weight)
               (zresid resid)
               (zcoef coef)
               (zqrs qr-solution)
               ;;(zrank rank)
               (zdf model-degrees-of-freedom))
              wls-fit
              ;; valid, but unnecessary ...
              ;; (setf zmm model-matrix)
              (loop 
                while 
                (and
                 (not converged)
                 (cl:< iter max-iter))
                do 
                (progn
                  (incf iter)
                  (setf deriv-mu (fn-call deriv mu))
                  (setf z (+ eta
                             (- offset)
                             (* (- response-matrix mu) deriv-mu)))
                  (setf zw (fn-call weight-fn mu weight))
                  ;;
                  ;; this next is here because by now, if weight[i]==0,
                  ;; then probably mu[i] == nan => zw[i]==nan ..
                  ;;
                  (setf zw (ensure-zero-weights-effective weight zw))
                  (setf z (ensure-zero-weights-effective weight z))
                  ;;
                  (setf zqrs nil)     ;; kill qr-solution from previous fit !!!
                  (fit wls-model model-frame :fit-object wls-fit 
                       :resid t :coef t :tolerance tolerance)
                  (setf eta (+ (- z zresid) offset))
                  (setf mu (fn-call inverse-link eta))
                  ;;(inspect (list eta mu))
                  (setf old-dev new-dev)
                  (setf new-dev (deviance family
                                          :mu mu 
                                          :y response-matrix
                                          :w weight))
                  (setf converged
                        (or (and (eq link-obj identity-link)
                                 (eq family gaussian-family))
                            (cl:<
                             (abs 
                              (cl:/ 
                               (cl:- old-dev new-dev)
                               (cl:+ old-dev tolerance)))
                             tolerance)))
                  (if verbose
                    (format verbose 
                            "~&Deviance at iteration ~S is ~S~%"
                            iter new-dev))
                  ))
              (if (not converged)
                (warn "Failed to achieve convergence in ~S iterations."
                      max-iter))
              (setf pred mu)
              (setf deviance new-dev)
              (setf resid zresid)
              (setf coef zcoef)
              ;;(setf rank zrank)
              (setf df zdf)))))))
  fit-object)
          
        
    
    
  
