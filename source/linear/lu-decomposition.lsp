;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                        lu-decomposition.lisp                              
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;-----------------------------------------------------------------------------
;;;

(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(lud-of lu-of ipvt-of lu-decomposition rcond-of)))

(defgeneric lud-of (A)
  (:documentation
   "Determines the LU decomposition of the matrix A.  ~
    That is, A is decomposed into the product of a ~
    matrix L and an upper triangular matrix U.  ~
    L is the product of elementary lower triangular and permutation matrices. ~
    Both are calculated using the LINPACK routine DGECO.  ~
    DGECO uses Gaussian elimination with partial pivoting to compute ~
    the LU decomposition and estimate the condition number of A. ~
    Once calculated, the decompostion is cached on A so that it ~
    need not be recalculated on subsequent calls.  ~
    (:required A A square matrix having only strictly numerical non-complex elements.)
    (:returns An instance of the class lu-decomposition.)~
    (:see-also (lu-decomposition :topic) (matrix-decomposition :topic)~
    (lsfit :generic-function) (solve :generic-function) ~
    (dgeco :function) (dgedi :function) (dgefa :function) (dgesl :function))~
    (:examples (:files (LU decompositions ~
    q:examples;arrays;matrices;decompositions;lu.lisp) )~
    )~
    "))

(defgeneric lu-of (A)
  (:documentation
   "Returns the matrix LU whose upper triangular is the upper ~
    triangular matrix U of the LU-decomposition A.  Below diagonal ~
    elements contain the information necessary to construct the lower ~
    triangular matrix L.  ~
    (:see-also (lu-decomposition :topic) (matrix-decomposition :topic)~
    (solve :generic-function) ~
    (lud-of :generic-function))~
    (:examples (:files (LU decompositions ~
    q:examples;arrays;matrices;decompositions;lu.lisp) )~
    )~
    "))

;--------------------------------------------------------------------------
; CLASS: lu-decomposition
;--------------------------------------------------------------------------

(defclass lu-decomposition (matrix-decomposition)
  
  ((a
    :initarg :a :reader lu-of :initform '()
    :documentation 
    "A contains in its upper triangle an upper triangular matrix U ~ 
       and in its strict lower triangle the multipliers necessary ~
     to construct a matrix L so that A=LU.")
   
   (ipvt
    :initarg :ipvt :reader ipvt-of :initform '()
    :documentation
    "IPVT is a singly subscripted integer array of dimension N  ~
     which contains the pivot information necessary to construct  ~
     the permutations in L.  Specifically, IPVT(K) is the ~
     index of the K-th pivot row.")
   
   (rcond
    :initarg :rcond :reader rcond-of :initform '()
    :documentation
    "This is the estimated inverse of the condition number of the original ~
     matrix. If RCOND is so small that the logical expression ~
     1.0 + RCOND .EQ. 1.0 is true, then the matrix can ~
     usually be regarded as singular to working precision."))
  
  (:documentation
   "Return class for LU decomposition. Slot documentation is from ~
    the Linpack manual."))

;;;
;;; export slot names
;;;

(push-extension-class 'lu-decomposition)

;---------------------------------------------------------------------------
; METHOD: lud-of
;---------------------------------------------------------------------------


(defmethod lud-of ((x matrix))
  (or (find-table-entry x 'decomposition 'lu)
      (let* ((a (sel x))
             (n (first (dimensions-of a)))
             (ipvt (array 0
                          :dimensions (list n)))
             (rcond (array 0 :dimensions nil))
             (z (array 0 :dimensions (list n))))
        (dgeco a n ipvt rcond z)
        (add-table-entry
         x 'decomposition 'lu 
         (make-instance 'lu-decomposition :a a :ipvt ipvt :rcond (eref rcond 0))
         ))))

;---------------------------------------------------------------------------
; METHOD: lu-of
;---------------------------------------------------------------------------


(defmethod lu-of ((X matrix))
  (lu-of (lud-of X)))


;;;
;;;  upper-triangle-of
;;;

(defmethod upper-triangle-of ((lud lu-decomposition))
  "Returns the matrix U from the LU decomposition."
  (upper-triangle-of (lu-of lud)))
