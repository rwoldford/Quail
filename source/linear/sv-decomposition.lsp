;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                          sv-decomposition.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1994 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1994.
;;;
;;;
;;;-----------------------------------------------------------------------------
;;;

(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(sv-decomposition
          svd-of
          left-singular-vectors-of
          right-singular-vectors-of
          singular-values-of)))


(defgeneric left-singular-vectors-of (thing)
  (:documentation
   "Returns the left singular vectors of its argument. ~
    (:see-also (svd-of :generic-function) ~
    (sv-decomposition :topic) ) ~
    (:examples (:files (sv decompositions ~
    q:examples;arrays;matrices;decompositions;svd.lisp) ~
    (Other decompositions ~
    q:examples;arrays;matrices;decompositions;intro.lisp) )~
    )"
   )
  )

(defgeneric right-singular-vectors-of (thing)
  (:documentation "Returns the right singular vectors of its argument. ~
                   (:see-also (svd-of :generic-function) ~
                   (sv-decomposition :topic) ) ~
                   (:examples (:files (sv decompositions ~
                   q:examples;arrays;matrices;decompositions;svd.lisp) ~
                   (Other decompositions ~
                   q:examples;arrays;matrices;decompositions;intro.lisp) )~
                   )"
                  )
  )

(defgeneric singular-values-of (thing)
  (:documentation "Returns the singular values of its argument. ~
                   (:see-also (svd-of :generic-function) ~
                   (sv-decomposition :topic) ) ~
                   (:examples (:files (sv decompositions ~
                   q:examples;arrays;matrices;decompositions;svd.lisp) ~
                   (Other decompositions ~
                   q:examples;arrays;matrices;decompositions;intro.lisp) )~
                   )"
                  )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CLASS: sv-decomposition
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass sv-decomposition (matrix-decomposition)
  ((singular-values
    :initarg :singular-values
    :reader singular-values-of
    :initform NIL
    :documentation 
    "Contains the vector of singular values in descending order.")
   
   (left-singular-vectors
    :initarg :left
    :reader left-singular-vectors-of
    :initform NIL
    :documentation 
    "The matrix of left singular vectors.")
   
   (right-singular-vectors
    :initarg :right
    :reader right-singular-vectors-of
    :initform NIL
    :documentation
    "The matrix of right singular vectors.")
   
   (diagonal
    :initarg :diag
    :initform NIL
    :documentation 
    "If info is non-zero, then this contains the diagonal ~
     of a bidiagonal matrix having the same singular values as the original ~
     matrix.  The super diagonal of this matrix is stored as the ~
     slot super-diagonal."
    )
   
   (super-diagonal
    :initarg :super-diag
    :initform NIL
    :documentation 
    "If info is non-zero, then this contains the super-diagonal ~
     of a bidiagonal matrix having the same singular values as the original ~
     matrix.  The diagonal of this matrix is stored on the slot diagonal."
    )
   
   (start-index
    :initarg :start-index
    :initform 0
    :documentation 
    "This is the start index of the singular-values that have been ~
     successfully computed.  If non-zero, the diagonal and super-diagonal ~
     slots become of interest.")
   )
  
  (:documentation 
   "Return class for singular value decomposition.  ~
    Slot documentation is mostly from ~
    the Linpack manual."))

;;; export slot names
;;;
(push-extension-class 'sv-decomposition)


;---------------------------------------------------------------------------
; GENERIC-FUNCTION: svd-of
;---------------------------------------------------------------------------

(defgeneric svd-of (x &key prompt-if-fail?)
  (:documentation
   "Determines the singular value decomposition of the matrix X.  ~
   That is, X is decomposed into the product of an orthogonal matrix U, ~
   diagonal matrix D and the transpose of the orthogonal matrix V.  ~
   It is calculated using the LINPACK routine DSVDC which uses Householder ~
   transformations to achieve this end. ~
   Once calculated, the decomposition is cached on X so that it ~
   need not be recalculated on subsequent calls.  Note that the decomposition ~
   might be dangerously out of date if the elements of X are changed after ~
   the initial call (see clear-decompositions). ~
   (:required ~
   (:arg X A matrix having only strictly numerical non-complex elements.) ~
   )~
   (:key ~
   (:arg prompt-if-fail? T If the algorithm fails to converge, should the ~
   user be offered the opportunity to continue? Continuation will produce an ~
   sv-decomposition object containing some of the singular values ~
   together with the diagonal and super-diagonal of a bidiagonal matrix that ~
   has the same singular values as X. These are stored as the slots of the same ~
   name.  If NIL, it is assumed that continuation is preferred.) ~
   )~
   (:returns An instance of the class sv-decomposition.)~
   (:see-also (sv-decomposition :topic) (matrix-decomposition :topic) ~
   (dsvdc :function)(clear-decompositions :function))~
   (:examples (:files (sv decompositions ~
   q:examples;arrays;matrices;decompositions;svd.lisp~
   ) ~
   (Other decompositions ~
   q:examples;arrays;matrices;decompositions;intro.lisp~
   ) )~
   )~
   "
    ))



(defmethod svd-of ((x matrix) &key (prompt-if-fail? T))
  "Determines the singular value decomposition of the matrix X.  ~
   That is, X is decomposed into the product of an orthogonal matrix U, ~
   diagonal matrix D and the transpose of the orthogonal matrix V.  ~
   It is calculated using the LINPACK routine DSVDC which uses Householder ~
   transformations to achieve this end. ~
   Once calculated, the decomposition is cached on X so that it ~
   need not be recalculated on subsequent calls.  Note that the decomposition ~
   might be dangerously out of date if the elements of X are changed after ~
   the initial call (see clear-decompositions). ~
   (:required ~
   (:arg X A matrix having only strictly numerical non-complex elements.) ~
   )~
   (:key ~
   (:arg prompt-if-fail? T If the algorithm fails to converge, should the ~
   user be offered the opportunity to continue? Continuation will produce an ~
   sv-decomposition object containing some of the singular values ~
   together with the diagonal and super-diagonal of a bidiagonal matrix that ~
   has the same singular values as X. These are stored as the slots of the same ~
   name.  If NIL, it is assumed that continuation is preferred.) ~
   )~
   (:returns An instance of the class sv-decomposition.)~
   (:see-also (sv-decomposition :topic) (matrix-decomposition :topic) ~
   (dsvdc :function)(clear-decompositions :function))~
   (:examples (:files (sv decompositions ~
   q:examples;arrays;matrices;decompositions;svd.lisp~
   ) ~
   (Other decompositions ~
   q:examples;arrays;matrices;decompositions;intro.lisp~
   ) )~
   )~
   "
  (let ((key (list 'svd
                   :prompt-if-fail?
                   (if prompt-if-fail? T))))
    (or (find-table-entry x 'decomposition key)
        (let* ((x-copy (sel x))
               (n    (or (first (dimensions-of x)) 1))
               (p    (or (second (dimensions-of x)) 1))
               (m    (min n p))
               (s    (array 0 :dimensions (list (min (1+ n) p))))
               (e    (array 0 :dimensions (list p)))
               (u    (array 0 :dimensions (list n m)))
               (v    (array 0 :dimensions (list p m)))
               (work (array 0 :dimensions (list n)))
               ;; The following job number insures that the first m
               ;; left singular vectors are produced and that all the
               ;; right singular vectors are produced.
               (job  21)
               (info (array 0 :dimensions (list 1))))
          
          
          (dsvdc x-copy n p s e u v work job info)
          (let ((svs (array NaN :dimensions (list m)))
                (start-index (eref info 0)))
            (cond
             ((zerop start-index)
              (loop for i from 0 to (- m 1)
                    do (setf (eref svs i)
                             (eref s i)))
              (add-table-entry x 'decomposition
                               key
                               (make-instance 'sv-decomposition 
                                 :singular-values svs
                                 :left u
                                 :right v
                                 :start-index 0
                                 :diag NIL
                                 :super-diag NIL
                                 )
                               )
              )
             (T
              (unless
                (and prompt-if-fail?
                     (quail-yes-or-no-p
                      "Warning: SVD failed to converge. ~&~
                       ~s singular values were obtained. ~&~
                       Continue with the rest set to NaN?"
                      (max 0 (- m start-index)))
                     )
                (quail-error "SVD failed to converge for ~s ~&~
                              ~s singular values were obtained."
                             x (max 0 (- m start-index))))
              (loop for i from start-index
                    to (- m 1)
                    do (setf (eref svs i)
                             (eref s i)))
              (add-table-entry x 'decomposition
                               key
                               (make-instance 'sv-decomposition 
                                 :singular-values svs
                                 :left u
                                 :right v
                                 :start-index start-index
                                 :diag s
                                 :super-diag e
                                 )
                               )))
            )
          )
        )
    )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Methods for matrices
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod left-singular-vectors-of ((thing matrix))
  (left-singular-vectors-of (svd-of thing))
  )

(defmethod right-singular-vectors-of ((thing matrix))
  (right-singular-vectors-of (svd-of thing))
  )

(defmethod singular-values-of ((thing matrix))
  (singular-values-of (svd-of thing))
  )
