;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                          eigen-decomposition.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1994 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     Carsten Whimster 1997.
;;;
;;;
;;;-----------------------------------------------------------------------------
;;;

(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(eigen-vectors-of
          eigen-values-of
          eigen-decomposition
          eigen-of)))

(defgeneric eigen-vectors-of (thing)
  (:documentation
   "Returns the eigen vectors of its argument. ~
    (:see-also (eigen-of :generic-function) ~
    (eigen-decomposition :topic) ) ~
    (:examples (:files (eigen decompositions ~
    q:examples;arrays;matrices;decompositions;eigen.lisp) ~
    (Other decompositions ~
    q:examples;arrays;matrices;decompositions;intro.lisp) )~
    )"
   )
  )

(defgeneric eigen-values-of (thing)
  (:documentation "Returns the eigen values of its argument. ~
                   (:see-also (eigen-of :generic-function) ~
                   (eigen-decomposition :topic) ) ~
                   (:examples (:files (eigen decompositions ~
                   q:examples;arrays;matrices;decompositions;eigen.lisp) ~
                   (Other decompositions ~
                   q:examples;arrays;matrices;decompositions;intro.lisp) )~
                   )"
                  )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CLASS: eigen-decomposition
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass eigen-decomposition (matrix-decomposition)
  ((eigen-values
    :initarg :eigen-values
    :reader eigen-values-of
    :initform NIL
    :documentation 
    "Contains the vector of eigen values.")
   
   (eigen-vectors
    :initarg :eigen-vectors
    :reader eigen-vectors-of
    :initform NIL
    :documentation 
    "The matrix of eigen vectors.")
   )

  (:documentation 
   "Return class for eigen decomposition.  ~
    Source: Numerical Recipes in Fortran."))

;;; export slot names
;;;
(push-extension-class 'eigen-decomposition)

;---------------------------------------------------------------------------
; GENERIC-FUNCTION: eigen-of
;---------------------------------------------------------------------------

(defgeneric eigen-of (A &optional sort?)
  (:documentation
   "Determines the eigen decomposition of the real symmetric matrix A.  ~
    It is calculated using the Jacobi method. ~
    Once calculated, the decomposition is cached on A so that it ~
    need not be recalculated on subsequent calls.  Note that the decomposition ~
    might be dangerously out of date if the elements of X are changed after ~
    the initial call (see clear-decompositions). ~
    (:required ~
    (:arg A - Symmetric matrix having only real elements.) ~
    )~
    (:returns An instance of the class eigen-decomposition.)~
    (:see-also (eigen-decomposition :topic) (matrix-decomposition :topic) ~
    (clear-decompositions :function))~
    (:examples (:files (eigen decompositions ~
    q:examples;arrays;matrices;decompositions;eigen.lisp~
    ) ~
    (Other decompositions ~
    q:examples;arrays;matrices;decompositions;intro.lisp~
    ) )~
    )~
    "
    ))

(defmethod eigen-of ((A matrix) &optional (sort? T))
  "Determines the eigen decomposition of the matrix A.  ~
   It is calculated using the jacobi routine. ~
   Once calculated, the decomposition is cached on A so that it ~
   need not be recalculated on subsequent calls.  Note that the decomposition ~
   might be dangerously out of date if the elements of X are changed after ~
   the initial call (see clear-decompositions). ~
   (:required ~
   (:arg A - Symmetric matrix having only real elements.) ~
   (:arg sort? - Default T. Specifies whether or not to sort eigen values
    and vectors.) ~
   )~
   (:returns an instance of the class eigen-decomposition.)~
   (:see-also (eigen-decomposition :topic) (matrix-decomposition :topic) ~
   (clear-decompositions :function))~
   (:examples (:files (eigen decompositions ~
   q:examples;arrays;matrices;decompositions;eigen.lisp~
   ) ~
   (Other decompositions ~
   q:examples;arrays;matrices;decompositions;intro.lisp~
   ) )~
   )~
   "
  (let ((key (list 'eigen)))
    (or (find-table-entry A 'decomposition key)
        (let* ((A-copy (sel A))
               (rows (nrows A))
               (D (array 0.0 :dimensions (list rows)))
               (V (array 0.0 :dimensions (list rows rows))))
          (jacobi A-copy D V)
          (if sort?
            (let ((sort-pos (qk::sort-position D #'>)))
              (add-table-entry A 'decomposition
                               key
                               (make-instance 'eigen-decomposition 
                                 :eigen-values (ref D sort-pos)
                                 :eigen-vectors (ref V T sort-pos)
                                 )
                               )
              )
            (add-table-entry A 'decomposition
                             key
                             (make-instance 'eigen-decomposition 
                               :eigen-values D
                               :eigen-vectors V
                               )
                             )
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

(defmethod eigen-vectors-of ((thing matrix))
  (eigen-vectors-of (eigen-of thing))
  )

(defmethod eigen-values-of ((thing matrix))
  (eigen-values-of (eigen-of thing))
  )
