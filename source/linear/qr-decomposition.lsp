;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                            qr-decomposition.lisp 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1994 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     Michael Lewis 1991.
;;;     Greg Anglin 1992.
;;;     R.W. Oldford 1993, 1994.
;;;
;;;
;;;--------------------------------------------------------------------------------
;;;

(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(qr-decomposition qrd-of qr-of qraux-of jpvt-of
          q-matrix-of
          ;;qy-of qty-of coef-of resid-of pred-of
          )))

;--------------------------------------------------------------------------
; CLASS: qr-decomposition
;--------------------------------------------------------------------------
  
(defclass qr-decomposition (matrix-decomposition)
  ((qr :initarg :qr
       :reader qr-of
       :initform '()
       :documentation 
       
       "QR contains in its upper triangle the upper triangular matrix R of
       the QR factorization.  Below its diagonal, x contains information
       from which the orthogonal part of the decomposition can be recovered.")
   
   (qraux :initarg :qraux
          :reader qraux-of
          :initform '()
          :documentation 
          
          "qraux is a singly subscripted array of dimension P that ~
           contains further informations required to recover the orthogonal part ~
           of the decomposition.")
   
   (jpvt :initarg :jpvt
         :reader jpvt-of
         :initform '()
         :documentation
         
         "jpvt(j) contains the index of the column that was moved into column ~
          j , provided pivoting has been requested.  Thus, on return, the ~
          QR decomposition is not of X, but of the matrix whose columns are ~
          X(jpvt(1)),X(jpvt(2)),...,X(jpvt(p)).")
   
   (pivot :initarg :pivot
          :reader pivoted-p
          :initform '()
          :documentation
          "Pivot is non-null if pivoting was requested, null otherwise.")
   
   (initial :initarg :initial
            :initform '()
            :reader initial-of
            :documentation
            "Initial identifies columns which were constrained to occupy leading ~
             positions during pivoting.")
   
   (final :initarg :final
          :initform '()
          :reader final-of
          :documentation
          
          "Final identifies columns which were constrained to occupy trailing ~
           positions during pivoting.")
   
   ;;(tolerance :initarg :tolerance
   ;;           :initform '()
   ;;           :documentation
   ;;           
   ;;           "Not used so commented out. Should indicate numerical tolerance ~
   ;;            below which linear dependencies are declared.")
   )
  
  (:documentation 
   "Return class for QR decomposition.  Slot documentation is mostly from ~
    the Linpack manual."))

;;; export slot names
;;;
(push-extension-class 'qr-decomposition)


;---------------------------------------------------------------------------
; GENERIC-FUNCTION: qrd-of
;---------------------------------------------------------------------------

(defgeneric qrd-of (x &rest keyword-args
                      &key pivot initial final
                      &allow-other-keys)
  (:documentation
   "Determines the QR decomposition of the matrix X.  ~
    That is, X is decomposed into the product of an orthogonal matrix Q ~
    and an upper triangular matrix R.  ~
    It is calculated using the LINPACK routine DQRDC which uses Householder ~
    transformations to achieve this end. ~
    Once calculated, the decomposition is cached on X so that it ~
    need not be recalculated on subsequent calls.  ~
    (:required X A matrix having only strictly numerical non-complex elements.)
    (:key ~
    (:arg pivot NIL ~
    Pivot is non-null if pivoting was requested, null otherwise.)~
    (:arg initial NIL ~
    If pivot is non-NIL, then initial can be a list of the indices ~
    of those columns which are to be constrained to occupy the leading ~
    positions of X during pivoting.If NIL, then there is no restriction ~
    on the initial columns.)~
    (:arg final NIL ~
    If pivot is non-NIL, then final can be a list of the indices ~
    of those columns which are to be constrained to occupy the trailing ~
    positions of X during pivoting.  If NIL, then there is no restriction ~
    on the last columns.)~
    )~
    (:rest (:arg keyword-args NIL For some methods there may be ~
    further keywords allowed.)~
    )~
    (:returns An instance of the class qr-decomposition.)~
    (:see-also (qr-decomposition :topic) (matrix-decomposition :topic)~
    (lsfit :generic-function) (dqrdc :function) (dqrsl :function))~
    (:examples (:files (QR decompositions ~
    q:examples;arrays;matrices;decompositions;qr.lisp) )~
    )~
    "))



(defmethod qrd-of ((x matrix) 
                   &rest keyword-args
                   &key 
                   (pivot nil pivot-supplied-p)
                   (initial nil initial-supplied-p)
                   (final nil final-supplied-p)
                   ;;(tolerance nil tolerance-supplied-p)
                   &allow-other-keys)
  "Determines the QR decomposition of the matrix X.  ~
    That is, X is decomposed into the product of an orthogonal matrix Q ~
    and an upper triangular matrix R.  ~
    It is calculated using the LINPACK routine DQRDC which uses Householder ~
    transformations to achieve this end. ~
    Once calculated, the decomposition is cached on X so that it ~
    need not be recalculated on subsequent calls.  ~
    (:required X A matrix having only strictly numerical non-complex elements.)
    (:key ~
    (:arg pivot NIL ~
    Pivot is non-null if pivoting was requested, null otherwise.)~
    (:arg initial NIL ~
    If pivot is non-NIL, then initial can be a list of the indices ~
    of those columns which are to be constrained to occupy the leading ~
    positions of X during pivoting.If NIL, then there is no restriction ~
    on the initial columns.)~
    (:arg final NIL ~
    If pivot is non-NIL, then final can be a list of the indices ~
    of those columns which are to be constrained to occupy the trailing ~
    positions of X during pivoting.  If NIL, then there is no restriction ~
    on the last columns.)~
    )~
    (:rest (:arg keyword-args NIL For some methods there may be ~
    further keywords allowed.)~
    )~
    (:returns An instance of the class qr-decomposition.)~
    (:see-also (qr-decomposition :topic) (qr-solution :topic) ~
   (matrix-decomposition :topic) (solve :generic-function)~
    (lsfit :generic-function) (dqrdc :function) (dqrsl :function))~
    (:examples (:files (QR decompositions ~
    q:examples;arrays;matrices;decompositions;qr.lisp) )~
    )~
    "
  ;; tolerance not supported above.  Would require change to LINPACK code. 
  ;; (:arg tolerance NIL ~
  ;; Tolerance for detecting linear dependence amongst columns columns ~
  ;; of X.  Smaller values indicate stricter limits before exact dependence ~
  ;; is inferred.)~
  (declare (ignore keyword-args))
  (let* ((table-key (cons 'qrd
                          (list :pivot (if pivot T)
                                       :initial initial
                                       :final final)))
         (current-decomposition
          (find-table-entry x 'decomposition table-key)))
    
    (if (and current-decomposition
             
             (with-slots ((qrd-pivot pivot)
                          (qrd-initial initial)
                          (qrd-final final)
                          ;;(qrd-tolerance tolerance)
                          )
                         current-decomposition
               
               (and
                (or (not pivot-supplied-p)
                    (eql pivot qrd-pivot))
                (or (not initial-supplied-p)
                    (eql initial qrd-initial))
                (or (not final-supplied-p)
                    (eql final qrd-final))
                ;;(or (not tolerance-supplied-p)
                ;;    (> tolerance qrd-tolerance))
                )))
      
      
      current-decomposition
      
      (let* ((qr (sel x))
             (n (first (dimensions-of qr)))
             (p (or (second (dimensions-of qr)) 1))
             (qraux (array 0
                           :dimensions (list p)))
             (jpvt (array 0
                          :dimensions (list p)))
             (work (array 0
                          :dimensions (list p)))
             job)
        
        (cond ((or pivot initial final)
               (progn
                 (setf pivot t)
                 (setf job 1)
                 (anchor-columns jpvt initial 1)
                 (anchor-columns jpvt final -1)))
              (t (setf job 0)))
        
        (dqrdc qr n p qraux jpvt work job)
        
        (add-table-entry x 'decomposition table-key
                         (make-instance 'qr-decomposition 
                           :qr qr :qraux qraux :jpvt jpvt :pivot pivot
                           :initial initial :final final
                           ;;:tolerance tolerance
                           )
                         )))))

;;;-------------------------------------------------------------------------

(defun anchor-columns (pivot-vector fixed-columns fixing-code)
  (if fixed-columns
    (loop for i in fixed-columns
          do (setf (eref pivot-vector i) fixing-code))))


;;;
;;;  upper-triangle-of
;;;

(defmethod upper-triangle-of ((qrd qr-decomposition))
  "Returns the upper triangular matrix R from the qr decomposition."
  (upper-triangle-of (qr-of qrd)))


(defgeneric q-matrix-of (thing)
  (:documentation "Returns the orthogonal Q matrix corresponding to a ~%~
                   QR (or Gram-Schmidt) orthogonalization of thing (a matrix ~%~
                   or a qr-decomposition). ~%~
                   Note: 1. contains only as many columns as the original ~%~
                   matrix, and 2. the column ordering of the Q matrix may not directly ~%~
                   correspond to that of the original thing due to possible ~%~
                   pivoting operations."))

(defmethod q-matrix-of ((x matrix))
  (q-matrix-of (qrd-of x)))

(defmethod q-matrix-of ((qrd qr-decomposition))
  (let ((x (original-matrix-of qrd))
        (R (upper-triangle-of qrd))
        )
    (when (pivoted-p qrd)
      (let* ((pivoted (sel x))
             (pivots (jpvt-of qrd))
             (cols (first (dimensions-of pivots))))
        (loop for i from 0 to (- cols 1)
              do (setf (ref-variable pivoted i)
                       (ref-variable x (- (eref pivots i) 1))))
        (setf x pivoted)))
    ;; Need to do the following inline more efficiently ... rwo
    (tp (solve (tp R) (tp x)))
    )
  )
