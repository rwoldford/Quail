;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                         qr-solution.lisp                              
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

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(qr-solution solve)))

;;;--------------------------------------------------------------------------
;;; CLASS: qr-solution
;;;--------------------------------------------------------------------------

(defclass qr-solution ()  
  ((coef
    :initarg :coef :initform nil
    :documentation        
    "The least-squares solution to a linear system of equations Xb = y ~
     as found by first decomposing X into a QR decomposition and then ~
     solving for b in QRb = y. These will be pivoted if the QR was.")   
   (resid
    :initarg :resid :initform nil
    :documentation        
    "Least squares residuals -- the difference between y and its fitted value.")   
   (fitted-values
    :initarg :fitted-values :initform nil
    :documentation        
    "The fitted values of y.  For the linear system Xb = y, with solution ~
     bhat, this is simply X.*bhat.") 
   (y
    :initarg :y :initform nil
    :documentation        
    "The matrix or vector y in the system Xb = y.")
   (qrd
    :initarg :qrd :initform nil
    :documentation        
    "QR decomposition used in calculations.")   
   (qy
    :initarg :qy :initform nil
    :documentation       
    "QY is the vector: Q times y.")   
   (qty
    :initarg :qty :initform nil
    :documentation        
    "QTY is the vector: transpose(Q) times y.")    
   (rank
    :initarg :rank :initform nil
    :documentation        
    "The numerical rank of the X matrix.  See the slot tolerance as well.")    
   (tolerance
    :initarg :tolerance :initform 1.0E-4 :reader tolerance-of
    :documentation        
    "Tolerance used to determine the rank if the rank is NIL.  ~
     Numerical rank will be the number of diagonal elements of the ~
     triangular matrix R that are greater than tolerance * the maximum ~
     diagonal element.") )
  (:documentation   
   "The class returned by calling solve on a qr-decomposition object. ~
    and a vector y."))


;;;
;;;  export slots
;;;

(push-extension-class 'qr-solution)


;;;
;;;  INVERSE
;;;

(defmethod inverse ((qrd qr-decomposition))
  (solve qrd (identity-matrix (nrows (qr-of qrd))))
  )

;;;
;;;  SOLVE
;;;
  
(defmethod solve ((qrd qr-decomposition) y
                  &rest keyword-args 
                  &key
                  (coef T)
                  qy qty resid fit 
                  (rank NIL rank-supplied-p)
                  (tolerance NIL)
                  &allow-other-keys)
  "Given a qr-decomposition qrd ~
   and a matrix or vector y, solve returns b such that qrb = y. ~
   Uses the linpack routine dqrls, to solve the system.~
   If y is not given, then solve uses the y found on the qr-solution. ~
   (:see-also qrd-of)~
   (:required ~
   (:arg qrd An instance of qr-decomposition.) ~
   (:arg y The matrix or vector y.  If NIL then qrs is queried for the value of y ~
   stored there.)~
   )~
   (:key ~
   (:arg coef T Flag indicating non-NIL if the coefficients are to be calculated. ~
   Note that if the original qr-decomposition had things pivoted then the ~
   coefficients returned here will also be pivoted.) ~  
   (:arg resid  NIL Non-NIL if the residuals are to be calculated.) ~  
   (:arg fitted-values  NIL Non-NIL if the fitted values are to be calculated.) ~
   (:arg qy nil Non-NIL if the vector Q times y is to be calculated.) ~  
   (:arg qty nil Non-NIL if the vector tp(Q) times y is to be calculated. ) ~
   (:arg rank nil If non-NIL, this must be the numerical rank of the X matrix. ~
   Otherwise the rank will be determined from the data.) ~  
   (:arg tolerance 1.0E-4 The numerical value used to determine the rank ~
   if the rank argument is NIL.  ~
   Numerical rank will be the number of diagonal elements of the ~
   triangular matrix R that are greater than tolerance * the maximum ~
   diagonal element. ~
   ) ~
   )~
   (:returns Two values.  The first is the least squares solution if requested ~
   and the second is a qr-solution object containing information as requested.) ~
   (:see-also qrd-of qr-solution qr-decomposition multiple-value-list) ~
   "
  (declare (ignore tolerance rank rank-supplied-p))
  (if (or (null y)
          (notany #'identity (list qy qty coef resid fit)))
    ;;; if don't ask for anything, return NIL
    (values NIL NIL)
    ;;; otherwise, do it.
    (apply #'solve (make-instance 'qr-solution :qrd qrd :y y)
           y keyword-args)
    )
  )


(defmethod solve ((qrs qr-solution) y
                  &rest keyword-args 
                  &key
                  (coef T)
                  qy qty resid fit 
                  (rank NIL rank-supplied-p)
                  (tolerance NIL)
                  &allow-other-keys)
  "Given a qr-solution qrs and a matrix or ~
   vector y, solve returns b such that qrb = y. ~
   Uses the linpack routine dqrls, to solve the system.~
   If y is not given, then solve uses the y found on the qr-solution. ~
   (:see-also qrd-of)~
   (:required ~
   (:arg qrs An instance of qr-solution.) ~
   (:arg y The matrix or vector y.  If NIL then qrs is queried for the value of y ~
   stored there.)~
   )~
   (:key ~
   (:arg coef T Flag indicating non-NIL if the coefficients are to be calculated. ~
   Note that if the original qr-decomposition had things pivoted then the ~
   coefficients returned here will also be pivoted.) ~  
   (:arg resid  NIL Non-NIL if the residuals are to be calculated.) ~  
   (:arg fitted-values  NIL Non-NIL if the fitted values are to be calculated.) ~
   (:arg qy nil Non-NIL if the vector Q times y is to be calculated.) ~  
   (:arg qty nil Non-NIL if the vector tp(Q) times y is to be calculated. ) ~
   (:arg rank nil If non-NIL, this must be the numerical rank of the X matrix. ~
   Otherwise the rank will be determined from the data.) ~  
   (:arg tolerance 1.0E-4 The numerical value used to determine the rank ~
   if the rank argument is NIL.  ~
   Numerical rank will be the number of diagonal elements of the ~
   triangular matrix R that are greater than tolerance * the maximum ~
   diagonal element. ~
   ) ~
   )~
   (:returns Two values.  The first is the least squares solution if requested ~
   and the second is the qr-solution object updated as necessary.) ~
   (:see-also qr-solution qr-decomposition multiple-value-list) ~
   "
  (declare (ignore keyword-args))
  
  (with-slots
    (qrd
     (qrs-y y)
     (qrs-qy qy) 
     (qrs-qty qty)
     (qrs-coef coef) 
     (qrs-resid resid)
     (qrs-fitted-values fitted-values)
     (qrs-tolerance tolerance)
     (qrs-rank rank)) 
    qrs
    (unless tolerance
      (setf tolerance (or qrs-tolerance 1E-4)))
    (setf qrs-tolerance tolerance)
    (if (and (or (null y)
                 (null qrs-y)
                 (eq y qrs-y))
             (notany #'identity (list qy qty coef resid fit))
             (and (numberp tolerance) (= tolerance qrs-tolerance))
             (or (not rank-supplied-p)
                 (= rank qrs-rank))
             )
      ;;; if don't ask for anything, return appropriate info
      (values NIL qrs)
      ;;; otherwise, calculate it.
      (with-slots
        (qr qraux)
        qrd
        (let*
          (job
           (n (first (dimensions-of qr)))
           (p (or (second (dimensions-of qr)) 1))
           (m (second (dimensions-of y)))
           (n-by-m (if m (list n m) (list n)))
           (p-by-m (if m (list p m) (list p)))
           (info (array 0 :dimensions nil))
           (qr-rank
            (or rank
                qrs-rank
                (with-cl-functions (abs - / <)
                  (if (second (dimensions-of qr))
                    (let ((riimax
                           (loop for i from 0 below p
                                 maximize (abs (eref qr i i)))))
                      (loop for i from 0 below p 
                            count
                            (> (abs 
                                (/ (eref qr i i) riimax))
                               tolerance)))
                    1))))
           work-qy work-qty work-resid work-fit work-coef)
          
          ;; Initialize the slots if y is given and different.
          (if y
            (if (not (eq qrs-y y))
              (setf qrs-y y
                    qrs-qty NIL
                    qrs-qy NIL
                    qrs-coef NIL
                    qrs-resid NIL
                    qrs-fitted-values NIL)
              )
            (setf y qrs-y))
          
          ;; The JOB parameter of DQRSL determines what is to be computed.
          ;; JOB has the decimal expansion ABCDE, where: 
          ;;    if A != 0            QY is to be computed
          ;;    if B,C,D or E != 0   QTY is to be computed
          ;;    if C != 0            COEF is to be computed
          ;;    if D != 0            RESID is to be computed
          ;;    if E != 0            PRED is to be computed
          ;; Note that a request to compute COEF, RESID, or PRED automatically
          ;; triggers the computation of QTY, for which storage must be provided
          ;; in the calling sequence.
          
          ;; Now get the correct decimal expansion for job
          ;; as determined by the keyword arguments.
          (setf job 0)
          (mapc #'(lambda (a1 a2) (if a1 (incf job a2)))
                (list qy qty coef resid fit)
                '(10000 1000 100 10 1))
          
          ;; Set up the work arrays
          (flet ((conserve-space (indicator &optional (size n-by-m))
                   (if indicator
                     (array 0 :dimensions size)
                     (array 0 :dimensions 'nil)))
                 )
            
            (setf work-qy (conserve-space qy))
            (setf work-resid (conserve-space resid))
            (setf work-fit (conserve-space fit))
            (setf work-coef (conserve-space coef p-by-m))
            (setf work-qty (conserve-space
                            (or qty
                                (some #'identity
                                      (list coef resid fit)))))
            )
          
          ;; Call the solver
          (dqrls qr qraux y
                 :rank qr-rank
                 :qy work-qy :qty work-qty 
                 :coef work-coef :resid work-resid :pred work-fit
                 :job job :info info)
          ;;
          ;; save the solution
          (setf qrs-rank qr-rank)
          (if qy (setf qrs-qy work-qy))
          (if qty (setf qrs-qty work-qty))
          (if resid (setf qrs-resid work-resid))
          (if fit (setf qrs-fitted-values work-fit))
          (when coef
            (setf qrs-coef work-coef)
            (flet ((extremely-small-p (x)
                     (cl:< (cl:abs x) tolerance))
                   )
              (when (< qr-rank p)
                (loop for i from 0 to (1- p)
                      do (if (extremely-small-p (eref qr i i))
                           (setf (ref qrs-coef i) nan))))
              )
            )
          )
        
        (values (if coef qrs-coef) qrs)
        
        ))))


;;;
;;;  The various readers.
;;;

(defmethod qrd-of ((qrs qr-solution) 
                   &rest keyword-args
                   &key
                   &allow-other-keys)
  "Simply returns whatever is found on the qrd slot, whatever ~
   the values of the remaining args."
  (declare (ignore keyword-args))
  (slot-value qrs 'qrd))


(defmethod (setf qrd-of) ((new-value qr-decomposition)
                          (qrs qr-solution))
  "Sets the value of the qrd slot. If it is different from the ~
   old value (not EQ), then the coefficients are recalculated."
  (declare (ignore keyword-args))
  (setf (slot-value qrs 'qrd) new-value)
  (solve qrs NIL)
  new-value)


(defmethod (setf qrd-of) ((new-value T)
                          (qrs qr-solution))
  "Sets the value of the qrd slot."
  (declare (ignore keyword-args))
  (quail-error "Sorry, the new-value must be an instance of qr-decomposition.~&~
                Not: ~s." new-value))

(defmethod coef-of ((qrs qr-solution))
  "The least-squares solution to a linear system of equations Xb = y ~
   as found from a QR solution and then ~
   solving for b in QRb = y. These will be pivoted if the QR was."
  (with-slots
    (coef)
    qrs
    (when (not coef)
      (solve qrs (slot-value qrs 'y) :coef t))
    coef)
  )

(defmethod resid-of ((qrs qr-solution))
  "Least squares residuals -- the difference between y and its fitted value. ~
   As determined from the qrs-solution."
  (with-slots
    (resid)
    qrs
    (when (not resid)
      (solve qrs (slot-value qrs 'y) :resid t))
    resid)
  )

(defmethod pred-of ((qrs qr-solution))
  "The fitted or predicted values of y as determined by the qr-solution qrs."
  (with-slots
    (fitted-values)
    qrs
    (when (not fitted-values)
      (solve qrs (slot-value qrs 'y) :fit t))
    fitted-values)
  )

(defmethod qy-of ((qrs qr-solution))
  "QY is Q times y as determined by the qr-solution qrs."
  (with-slots
    (qy)
    qrs
    (when (not qy)
      (solve qrs (slot-value qrs 'y) :qy t))
    qy)
  )

(defmethod qty-of ((qrs qr-solution))
  "QY is transpose(Q) times y as determined by the qr-solution qrs."
  (with-slots
    (qty)
    qrs
    (when (not qty)
      (solve qrs (slot-value qrs 'y) :qty t))
    qty)
  )
          
(defmethod rank-of ((qrs qr-solution) &rest keyword-args
                    &key &allow-other-keys)
  "The numerical rank of the X matrix used in calculating ~
   this qr-solution. See the reader tolerance-of as well."
  (declare (ignore keyword-args))
  (slot-value qrs 'rank))

(defmethod tolerance-of ((qrs qr-solution))      
  "Tolerance used to determine the rank if the rank is NIL.  ~
   Numerical rank will be the number of diagonal elements of the ~
   triangular matrix R that are greater than tolerance * the maximum ~
   diagonal element."
  (slot-value qrs 'tolerance))
