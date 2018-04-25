;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               lsfit.lisp                              
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1990 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     Michael Lewis 1991.
;;;     Greg Anglin 1992.
;;;     R.W. Oldford 1995.
;;;
;;;--------------------------------------------------------------------------------
;;;  just a change so I could add a comment to the rlog. ... rwo

(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(lsfit)))


;-------------------------------------------------------------------------
;GENERIC FUNCTION: lsfit
;-------------------------------------------------------------------------

(defgeneric lsfit (x &rest keyword-args &key qy qty coef resid pred
                     &allow-other-keys)
  (:documentation 
   "Args: x, a gaussian-linear-model-fit object. ~%~
    On return, the same gaussian-linear-model-fit object with the least squares ~
    solution computed."))

(defmethod lsfit ((self gaussian-linear-model-fit) 
                  &rest keyword-args 
                  &key qy qty coef resid pred 
                  &allow-other-keys) 
  (declare (ignore keyword-args))
  (with-slots (response-matrix
               model-matrix
               weight 
               qr-solution
               (lsfit-df model-degrees-of-freedom)
               (lsfit-coef coef) 
               (lsfit-resid resid)
               (lsfit-pred pred))
              self            
    (let* ((y (apply-weight weight response-matrix))
           (x (apply-weight weight model-matrix))
           solve-coef
           )
      (unless qr-solution
        (multiple-value-setq
          (solve-coef qr-solution)
          (solve x y :qy qy :qty qty :coef coef :resid resid :fit pred)))
      (setf lsfit-df (rank-of qr-solution))
      (if coef (setf lsfit-coef solve-coef))
      (if resid (setf lsfit-resid (resid-of qr-solution)))
      (if pred (setf lsfit-pred (pred-of qr-solution)))
      (if qy (qy-of qr-solution))
      (if qty (qty-of qr-solution))
      self
      )))


;-----

(defmethod lsfit :after ((self gaussian-linear-model-fit) 
                         &rest keyword-args 
                         &key qy qty coef resid pred 
                         &allow-other-keys)
  
  (declare (ignore keyword-args qy qty))
  
  (with-slots (model-matrix
               response-matrix
               weight 
               qr-solution
               (lsfit-coef coef)
               (lsfit-pred pred)
               (lsfit-resid resid)) self
    
    (with-slots (pivot jpvt)
                (qrd-of qr-solution)
      
      (if pivot
        ;; unpivot the coefficients
        (if coef (setf lsfit-coef (unpivot-in-place lsfit-coef jpvt))))
      
      (if weight
        ;; remove the weights from the residuals
        ;; and predictors
        (cond ((null weight))
              
              ((and (mat1dp weight)
                    (or resid pred))
               
               (let ((inv-wt-factor (matrix-sqrt (/ weight))))

                 (if pred (setf lsfit-pred (* lsfit-pred inv-wt-factor)))
                 (if resid (setf lsfit-resid (* lsfit-resid inv-wt-factor)))
                 
                 (if (loop with no-zeros = t
                           for ii upto (1- (first (dimensions-of weight)))
                           while no-zeros
                           ;; take a chance we won't see a symbol ... need ext_zerop
                           when (zerop (eref weight ii 0))
                           do (setf no-zeros nil)
                           finally (return (not no-zeros)))
                   
                   (let* ((wt-zero (multiple-position-if weight #'zerop))
                          (x0 (ref model-matrix wt-zero))
                          (y0 (ref response-matrix wt-zero))
                          (pred0 (.* x0 lsfit-coef)))
                     
                     (if resid (setf (ref lsfit-resid wt-zero) (- y0 pred0)))
                     (if pred (setf (ref lsfit-pred wt-zero) pred0))))))
              
              ((and (mat2dp weight)
                    (or resid pred))
               
               (with-slots ((cholesky-a a)
                            (cholesky-info info))
                           (cholesky-of weight)
                 
                 (if (not (= (eref cholesky-info) (first (dimensions-of weight))))
                   
                   (progn
                     (setf lsfit-pred (.* model-matrix lsfit-coef))
                     (setf lsfit-resid (- response-matrix lsfit-pred)))
                   
                   (let (work-resid work-pred (job 01))
                     
                     (if resid
                       (progn 
                         (setf work-resid lsfit-resid)
                         (dtrls cholesky-a work-resid job)
                         (setf lsfit-resid work-resid)))
                     
                     (if pred
                       (progn
                         (setf work-pred lsfit-pred)
                         (dtrls cholesky-a work-pred job)
                         (setf lsfit-pred work-pred))))))))))))
