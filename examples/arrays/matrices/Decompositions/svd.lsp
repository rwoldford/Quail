;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               svd.lsp                              
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

(in-package :quail-user)

;;;
;;;  This is an example file illustrating the use of my favourite matrix
;;;  decomposition, the singular value decomposition or svd for short.
;;;
;;;  First there is some discussion of the decomposition itself and some
;;;  of its important properties and relations.
;;;  The remainder of the file gives example code illustrating the
;;;  use of the implementation and some of the properties described here.
;;;
;;;  To get to the example code search for the string **example**
;;;
;;;  As defined here, it is sometimes referred to as the singular value 
;;;  factorization (e.g. LINPACK), the term decomposition being reserved for
;;;  the case when U is required to be a complete n by n orthogonal matrix.
;;;
;;;  The svd is a decomposition of the n by p matrix X
;;;  into the product of three matrices, U D and V, where
;;;               T
;;;  1.  X = U D V
;;;
;;;  2.  U is an n by p matrix such
;;;       T
;;;      U U = Ip   ... the p by p identity matrix.
;;;
;;;      The columns of U are called the left singular vectors of X.
;;;
;;;  3.  V is a p by p orthogonal matrix.
;;;       T      T
;;;      V V = VV  = Ip   ... the p by p identity matrix.
;;;
;;;      The columns of V are called the right singular vectors of X.
;;;
;;;  4.  D is a p by p diagonal matrix whose non-negative diagonal elements are
;;;      the singular values of X arranged in order of decending magnitude.
;;;      
;;;
;;;  Note also that the following properties and relations hold:
;;;
;;;      T       2  T
;;;     X X = V D  V   And so the singular values of X are the square root
;;;            of the eigen-values of XTX.
;;;            The column vectors of V are the corresponding eigen-vectors.
;;;
;;;        T     2  T
;;;     X X = U D  U   And so the singular values of X are the square root
;;;            of the non-zero eigen-values of XXTX.
;;;            The column vectors of U are the corresponding eigen-vectors.
;;;
;;;        T
;;;     U U     is *not* an orthogonal matrix but
;;;             is an orthogonal projection matrix that will project any
;;;             n by 1 real vector onto the column space of the X matrix.
;;;             It is sometimes called the ``hat matrix'' in statistics
;;;             and its diagonal elements are measures of ``leverage''
;;;
;;;         T
;;;  I - U U    is also an orthogonal projection matrix projecting a vector
;;;             y onto the orthogonal complement of the column space of X.
;;;
;;;         -1 T
;;;  G = V D  U   is a generalized inverse of X; it is the Moore-Penrose
;;;               inverse and satisfies the following properties:
;;;               XGX = X ; GXG = G 
;;;               Again, for numerical reasons it is not a good idea to
;;;               work with g-inverses in calculations (at least those so
;;;               formed).  It's better to work with a QR or LU decomposition.
;;;
;;;       -1 T T
;;;  b = V D  U y    is the least-squares solution to the overdetermined linear
;;;                  system Xb=y or, equivalently, to the regression problem
;;;                  y = Xb + r where b and the residual r are unknown.
;;;                  Note that this is not the recommended means of numerically
;;;                  determining the solution; use the solve function instead.

;;;
;;;  All values are calculated using the LINPACK routine dsvdc.
;;;

;;;  The **example** begins
;;;
;;;  Let's get a 25 by 4 matrix X.
;;;

(<- x (array (iseq 100) :dimensions '(25 4)))

;;;
;;; Its svd is simply calculated as
;;;

(<- xsvd (svd-of X))

;;; xsvd is an instance of an sv-decomposition object which stores the
;;; relevant information.
;;; The singular values are stored as a vector. 

(singular-values-of xsvd)

;;; The matrix U:

(left-singular-vectors-of xsvd)

;;; The matrix V:

(right-singular-vectors-of xsvd)

;;;
;;; The decomposition has been cached on the original matrix X so that
;;; we can ask for these quantities from either the svd or directly from
;;; the matrix X.
;;; Had the svd not been calculated before, it would be calculated and
;;; cached as soon as X was queried for singular-value information.
;;; So we have

(eq (singular-values-of X) (singular-values-of xsvd))

;;; returning T because both calls return the same vector.
;;; The same holds for the other calls for right left singular
;;; values.
;;; NOTE:  Any stored decompositions can always be cleared with

(clear-decompositions X)

;;; Any further decomposition calls will require recalculation.
;;; And now xsvd calculated previously will no longer be eq to the
;;; new calculations

(eq (singular-values-of X) (singular-values-of xsvd))

;;; although element-wise the results are numerically equal.

(and (= (eref (singular-values-of X) 0) (eref (singular-values-of xsvd) 0))
     (= (eref (singular-values-of X) 1) (eref (singular-values-of xsvd) 1))
     (= (eref (singular-values-of X) 2) (eref (singular-values-of xsvd) 2))
     (= (eref (singular-values-of X) 3) (eref (singular-values-of xsvd) 3))
     )


;;; For numerical reasons the decomposition will rarely be exact.
;;; For example, let's multiply the pieces together and see how close
;;; the result is to the original matrix X.

(<- svs (singular-values-of X))

;;; which is a vector.  We use the function diagonal to get a diagonal
;;; matrix whose diagonal elements are those of the vector.

(<- d (diagonal svs))

;;; Now for the other matrices,

(<- u (left-singular-vectors-of X))
(<- v (right-singular-vectors-of X))

;;; and the mean element-wise absolute error in caclulating X from these
;;; components is

(mean (abs (- x (.* u d (tp v)))))

;;;  Similarily we might test the orthogonality of the singular-vectors
;;;

(mean (abs (- (.* (tp v) v) (identity-matrix 4))))
(mean (abs (- (.* v (tp v)) (identity-matrix 4))))

(mean (abs (- (.* (tp u) u) (identity-matrix 4))))


;;;
;;; Mathematically X is only of rank 2.  The second and third columns can be
;;; obtained as the sum of the first and an integer multiple of the difference
;;; between the second and first.
;;; The rank can be determined as the number of non-zero singular values.
;;; Numerically, however this will not be exactly 2.

(matrix-rank-of x)

;;; Should we decide that the last two singular values are mathematically zero,
;;; we can set them to zero exactly.

(<- (ref (singular-values-of x)
         '(2 3))                  ; indices 2 and 3 are the last two.
    '(0 0))

;;; Note that svs has changed as well; it was the same vector.

svs

;;;
;;; The rank will now be mathematically correct.

(matrix-rank-of x)


;;;  Of course there is no guarantee that the resulting svd will produce
;;;  more accurate answers for other calculations.

(<- dnew (diagonal svs))

(mean (abs (- x (.* u dnew (tp v)))))


;;;
;;;  Finally, here's how you might collect up the diagonal elements of the
;;;  hat matrix
;;;          T  -1  T      T
;;;      X (X X)   X  = U U 

(collect-slices (u-row u 0)
  ;; That is, collect slices where each slice will be called
  ;; u-row.  Each slice will be a slice of u, and
  ;; a slice of u will be defined to be all elements
  ;; having the same value of its 0'th index (i.e. in the same row).

  (sum (* u-row u-row))
  )

;;;
;;;  So we might define a function that puts it all together
;;;

(defun get-leverage-values (X)
  "Returns a list of the diagonal elements of the ~
   hat matrix formed from the matrix X."
  (collect-slices (u-row
                   (left-singular-vectors-of X)
                   0)
    (sum (* u-row u-row))
    )
  )

;;; Note that the svd will only ever be calculated once.
;;;

(get-leverage-values X)

;;; Here's a faster version that removes the overhead from
;;; sum and collect-slices; but requires a little more thinking.
;;; The basic idea is to avoid having generic functions look up
;;; the appropriate method more often than necessary.
;;; In this case, because all calculations are done with the result
;;; of an eref, we can count on that being a Common Lisp number
;;; and so may use ``sum'' in the loop macro and also
;;; the CL function * rather than the more general Quail versions
;;; of either of these.  The Quail macro ``with-CL-functions''
;;; identifies the list of functions to be replaced everywher in the
;;; body by the CL equivalent. 
;;;

(defun get-lev-vals (X)
  "Returns a list of the diagonal elements of the ~
   hat matrix formed from the matrix X."
  (with-CL-functions (*)
    (let ((n (or (first (dimensions-of X)) 1))
          (p (or (second (dimensions-of X)) 1))
          (U (left-singular-vectors-of X))
          )
      (loop for i from 0 below n
            collect
            (loop for j from 0 below p
                  as val = (eref U i j)
                  sum
                  (* val val)
                  )))))

(get-lev-vals X)

;;; To compare, use time.  The difference can be substantial.
;;; Again, to be fair, make sure that the svd is calculated
;;; before either call.  If you have executed the statements
;;; above in order this will be the case.
;;;

(time (get-leverage-values X))
