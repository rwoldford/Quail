;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                        cholesky-decomposition.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1992 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     Michael Lewis 1991.
;;;     Greg Anglin 1992.
;;;     R.W. Oldford 1993, 1994.
;;;
;;;
;;;-----------------------------------------------------------------------------
;;;

(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(cholesky-of cholesky-decomposition)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CLASS: Cholesky-decomposition
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass cholesky-decomposition (matrix-decomposition)
  
  ((a :initarg :a :accessor upper-triangle-of :initform '()
      :documentation 
      "A is an upper triangular matrix representing ~
       the Cholesky factor of the matrix ~
      as it has been permuted by pivoting.")
   
   (jpvt :initarg :jpvt :reader jpvt-of :initform '()
         :documentation
         "If pivoting was requested, JPVT(J) contains the index of the ~
          diagonal element of A that was moved into the J-th position.")
   (rcond :initarg :rcond :initform NIL
          :accessor rcond-of
          :documentation
          "This is the estimated inverse of the condition number of the original ~
           matrix. If RCOND is so small that the logical expression ~
           1.0 + RCOND .EQ. 1.0 is true, then the matrix can ~
           usually be regarded as singular to working precision.")
   (null-vector
    :initarg :null-vector :initform NIL
    :reader null-vector-of
    :documentation
    "If the original matrix, A, is close to a singular matrix, ~
     then this will contain an approximate null-vector, z, in the ~
     sense that ||Az|| = rcond*||A||*||z||.")
   
   (info :initarg :info :reader info-of :initform '()
         :documentation
         "Value depends upon whether pivoting was chosen or not. ~
          If pivoting, info contains the index of the last positive diagonal ~
          element of the Cholesky factor.  ~
          Otherwise, it contains the order of the leading sub-matrix found ~
          not to be positive definite."))
  
  (:documentation 
   
   "Return class for Cholesky decomposition.  Slot value ~
    documentation from the Linpack manual."))

;;;
;;;  export slots
;;;

(qk::push-extension-class 'cholesky-decomposition)

;---------------------------------------------------------------------------
; METHOD: cholesky-of
;---------------------------------------------------------------------------

(defgeneric cholesky-of (A &key pivot initial final prompt-if-fail?)
  (:documentation
   "If A is positive definite, A can ~
    be factored uniquely in the form A = tp(R)R, where R ~
    is an upper triangular matrix with positive diagonal ~
    elements.  This is the Cholesky decomposition of A, and ~
    R is its Cholesky factor (Linpack manual). ~
    Once calculated, the decomposition is cached on A so that it ~
    need not be recalculated on subsequent calls.  ~
    (:required A A positive definite symmetric square matrix having only ~
    strictly numerical non-complex elements.  No check is made for symmetry.)
    (:key ~
    (:arg pivot NIL ~
    Pivot is non-null if pivoting is requested, null otherwise.)~
    (:arg initial NIL ~
    If pivot is non-NIL, then initial can be a list of the indices ~
    of those diagonal elements which are to be moved the leading ~
    positions of A during pivoting.If NIL, then there is no restriction ~
    on the initial columns.)~
    (:arg final NIL ~
    If pivot is non-NIL, then final can be a list of the indices ~
    of those diagonal elements which are to be moved to the trailing ~
    positions of A during pivoting.  If NIL, then there is no restriction ~
    on the last columns.)~
    (:arg prompt-if-fail? T Only relevant if no pivoting is allowed. ~
    If the algorithm fails, should the ~
    user be offered the opportunity to continue? ~
    If NIL, it is assumed that continuation is preferred. ~
    Continuation will produce a ~
    cholesky-decomposition object containing only a partial decomposition ~
    together with the order of the leading submatrix found to not be ~
    positive definite and a vector z such that Az = 0 approx.  ~
    These are stored as the slots info and null-vector, respectively.)~
    )~
    (:returns An instance of the class cholesky-decomposition.)~
    (:see-also ~
    (cholesky-decomposition :topic) ~
    (upper-triangle-of :generic-function) ~
    (lu-decomposition :topic) ~
    (matrix-decomposition :topic)~
    (lsfit :generic-function)  ~
    (backsolve :function))~
    (:examples (:files (Cholesky decomposition ~
    q:examples;arrays;matrices;decompositions;cholesky.lisp) )~
    )~
    ")
  )


(defmethod cholesky-of ((A matrix)
                        &key
                        (pivot NIL)
                        initial
                        final
                        (prompt-if-fail? T)
                        )
  "If A is positive definite, A can ~
   be factored uniquely in the form A = tp(R)R, where R ~
   is an upper triangular matrix with positive diagonal ~
   elements.  This is the Cholesky decomposition of A, and ~
   R is its Cholesky factor (Linpack manual).  ~
   Once calculated, the decomposition is cached on A so that it ~
   need not be recalculated on subsequent calls.  ~
   (:required A A positive definite symmetric matrix having only ~
   strictly numerical non-complex elements.) ~
   (:key ~
   (:arg pivot NIL ~
   Pivot is non-null if pivoting is requested, null otherwise. ~
   If pivoting is requested LINPACK dchdc is used, otherwise dpoco.)~
   (:arg initial NIL ~
   If pivot is non-NIL, then initial can be a list of the indices ~
   of those diagonal elements which are to be moved the leading ~
   positions of A during pivoting.If NIL, then there is no restriction ~
   on the initial columns.)~
   (:arg final NIL ~
   If pivot is non-NIL, then final can be a list of the indices ~
   of those diagonal elements which are to be moved to the trailing ~
   positions of A during pivoting.  If NIL, then there is no restriction ~
   on the last columns.)~
   (:arg prompt-if-fail? T Only relevant if no pivoting is allowed. ~
   If the algorithm fails, should the ~
   user be offered the opportunity to continue? ~
   If NIL, it is assumed that continuation is preferred. ~
   Continuation will produce a ~
   cholesky-decomposition object containing only a partial decomposition ~
   together with the order of the leading submatrix found to not be ~
   positive definite and a vector z such that Az = 0 approx.  ~
   These are stored as the slots info and null-vector, respectively.) ~
   )~
   (:returns An instance of the class cholesky-decomposition.)~
   (:see-also (cholesky-decomposition :topic) ~
   (upper-triangle-of :generic-function) ~
   (lu-decomposition :topic) ~
   (matrix-decomposition :topic) ~
   (lsfit :generic-function) ~
   (backsolve :function) ~
   (q::dchdc :function) ~
   (q::dpoco :function))~
   (:examples (:files (Cholesky decomposition ~
   q:examples;arrays;matrices;decompositions;cholesky.lisp) )~
   )~
   "
  (let ((table-key (list 'cholesky
                         :pivot (if pivot T)
                         :initial initial
                         :final final)))
    (or (find-table-entry A 'decomposition table-key)
        (if pivot
          (let* ((n (nrows A))
                 (p (ncols A))
                 (aa (sel A))
                 (work (array 0
                              :dimensions (list p)))
                 (jpvt (array 0
                              :dimensions (list n)))
                 (job 1)
                 (info (array 0 :dimensions 'nil)))
            
            (if initial
              (loop for i in initial do (setf (eref jpvt i) 1)))
            (if final
              (loop for i in final do (setf (eref jpvt i) -1)))
            
            ;; call linpack
            (dchdc aa p work jpvt job info)
            
            ;;; Zero out the bottom of aa
            (loop
              for i from 0 below n
              do
              (loop
                for j from 0 below i
                do
                (setf (eref aa i j) 0)))

            (setf info (eref info 0))

            (add-table-entry A 'decomposition table-key
                             (make-instance
                               'cholesky-decomposition
                               :a aa
                               :jpvt jpvt
                               :info info)
                             ))
          (let* ((n (nrows A))
                 (aa (sel A))
                 (z (array 0 :dimensions (list n)))
                 (rcond (array 0 :dimensions 'nil))
                 (info (array 0 :dimensions 'nil)))
            
            (dpoco aa n rcond z info)
            
            ;;; Zero out the bottom of aa
            (loop
              for i from 0 below n
              do
              (loop
                for j from 0 below i
                do
                (setf (eref aa i j) 0)))
            
            (setf info (eref info 0))
            (setf rcond (eref rcond 0))
            (cond
             ((zerop info)
              (add-table-entry A 'decomposition table-key
                               (make-instance
                                 'cholesky-decomposition
                                 :a aa
                                 :rcond rcond
                                 :null-vector z
                                 :info info)
                               )
              )
             (prompt-if-fail?
              (unless
                (quail-yes-or-no-p
                 "Possibly non positive definite matrix of order ~s.  ~
                  Cholesky failed to converge. ~&~
                  Continue?"
                 (eref info 0))
                (quail-error "Cholesky failed to converge for ~s ~&~
                              Matrix not positive definite."
                             A))
              (if (quail-yes-or-no-p "Try pivoting?")
                (cholesky-of A :pivot T)
                (add-table-entry A 'decomposition table-key
                                 (make-instance
                                   'cholesky-decomposition
                                   :a aa
                                   :rcond rcond
                                   :null-vector z
                                   :info info)
                                 )))
             (T
              (warn  "Cholesky failed to converge for ~s ~&~
                      Matrix not positive definite. ~&~
                      Partial decomposition returned."
                     A)
              (add-table-entry A 'decomposition table-key
                               (make-instance
                                 'cholesky-decomposition
                                 :a aa
                                 :rcond rcond
                                 :null-vector z
                                 :info info)
                               ))
             )
            )
          )
        )
    )
  )

(defmethod cholesky-of :around ((A matrix)
                                &key
                                (pivot NIL)
                                initial
                                final
                                (prompt-if-fail? T)
                                )
  (declare (ignore pivot initial final prompt-if-fail?))
  (if (= (nrows A)
         (ncols A))
    (call-next-method)
    (quail-error "Cholesky-of: Matrix is not square!")))
