;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                          Matrix operations                           
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (C) 1994 Statistical Computing Laboratory, University Of Waterloo
;;;
;;;   
;;;  Authors:
;;;     R.W. Oldford 1994.
;;;
;;; Contents:
;;;
;;;    2. Matrix operations.
;;;           First consider selected operations that result from matrices
;;;           being special kinds of Quail ref-arrays.
;;;           These are the n-ary arithmetic operators, various
;;;           common mathematical unary and binary operations,
;;;           and numerical predicates.
;;;
;;;       2.1 Matrix specific operations.
;;;
;;;           2.1.1 Matrix multiplication.  .*
;;;           2.1.2 Solving systems of linear equations
;;;           2.1.3 Matrix inversion 
;;;
;;;       2.2 Numerical properties
;;;           ... rank-of, condition-number-of, determinant,
;;;               trace-of
;;;
;;;           2.2.1 Numerical ranks
;;;           2.2.2 Condition numbers
;;;           2.2.3 Determinant
;;;           2.2.4 Trace
;;;           2.2.5 Selected algebraic structure
;;;
;;;
;;;  To go directly to any of these, just search this file for the topic of
;;;  of interest or to go section by section search for \section \subsection
;;;  or \subsubsection.
;;;  However, please note that some results will depend on
;;;  having executed other forms earlier in the file.
;;;
;;;  To get to the matrix overview:
     (edit-file "eg:Arrays;Matrices;overview.lsp")

;;;  To get to the array overview:
     (edit-file "eg:Arrays;overview.lsp")

;;; First we need a matrix x.

(<- x (array '(1 2 3 4 5 6) :dimensions '(3 2)))
x

;;;
;;;  \Section 2. Matrix operations.
;;;
;;;  Here we consider selected operations that result from matrices
;;;  being special kinds of Quail ref-arrays.
;;;  These are the n-ary arithmetic operators, and various
;;;  common mathematical unary and binary operations.
;;;
;;;  All of the functions that can be used with arrays can also
;;;  be used with a matrix x.  Some examples typically expected
;;;  of a matrix are its' transpose

(tp x)

;;;  and element-wise arithmetic

(+ x x x)
(- x x x)
(/ x x x)
(* x x x)
(* 2 x)


;;; For these operations to work the dimensions of the arguments must conform.
;;; For example, (+ (tp x) x) will fail.
;;; 
;;; If however one of the arguments is a scalar or a 0-dimensional matrix,
;;; the operation will apply to all conformable arguments equally.

(+ x 999999 x)
(+ x (ref x 0 0) x)

;;; And so on for the other n-ary operators -, *, and /.
;;;
;;; This also holds for 1-dimensional matrices. Here are two.

(<- col-x (+ '(10000 20000 30000) (ref x T 0)))
col-x
(<- row-x (+ '(10 20) (ref x 0 T)))
row-x

;;; Adding col-x to x results in it being added to every column of x.

(+ x col-x)
(+ col-x x)

;;; Adding row-x to x results in it being added to every row of x.

(+ x row-x)
(+ row-x x)

;;; And this works for two-dimensional matrices when one of the dimensions
;;; is 1.

(+ (tp row-x) x)
(+ x (tp row-x))

;;; When one of the dimensions is 1 (nrows or ncols is 1), then some effort is
;;; made to match the remaining dimension with one of the dimensions of the
;;; other argument and operate over rows or columns accordingly.
;;;
;;; If we have ncols=nrows on the two dimensional argument then some ambiguity
;;; arises and we have arbitrarily selected column-major order.

(<- square-x (array '(1 2 3 4 5 6 7 8 9) :dimensions '(3 3)))
square-x
(+ square-x col-x)
(+ col-x square-x)
(+ (tp col-x) square-x)
(+ square-x (tp col-x))

;;; NOTE: When more than two arguments are given, arithmetic operations occur
;;; in binary fashion proceeding left to right.  As a consequence
;;; (aside from numerical considerations) the arithmetic operations are no
;;; longer associative when operating on matrices of which some are two
;;; dimensional and some are one dimensional.
;;; For example,

(+ x col-x row-x)

;;; and

(+ col-x x row-x)

;;; are equivalent but (+ col-x row-x x) will fail because (+ col-x row-x) will.
;;; If you are keen to do something different you can take matters into your
;;; own hands for element-wise operations by calling map-element directly.
;;; This will allow you to specify an order.
;;;
;;;
;;; All of Common Lisp's mathematical functions have been extended to operate
;;; on arrays in general.   These are described in more detail in

(edit-file "eg:Arrays;math-funs.lsp")

;;;  Some examples are

(* 2 x)
(* x x)
(/ 2 x)
(log x)
(log x 10)
;;; take care here not to have a base 1 log calculated.
(log x (+ x 10))
(exp x)
(cos x)
(expt x 3)
(expt x x)
(sqrt x)
(mod x 2)
(mod x x)

;;; and so on.
;;;
;;; 
;;; Similarly, numerical predicates have been extended for Quail arrays,
;;; and hence for matrices.

(= x x x)
(> x 3)
(>= (expt x 2) x)
(< x 3)
(<= (expt x 2) x)

;;; For more detail, see

(edit-file "eg:Arrays;num-preds.lsp")

;;; In fact, anything that operates on Quail arrays also operates on matrices.
;;; For the whole ball of wax, see 

(edit-file "eg:Arrays;overview.lsp")


;;; \subsection 2.1 Matrix specific operations.
;;;
;;;  Besides the usual mathematical operations inherited from numerical arrays,
;;;  there are a few that are peculiar to matrices.
;;;  Here we examine 
;;;           ... Dot product for matrix multiplication: .*
;;;           ... Solving linear systems of equations: solve
;;;           ... Matrix inverses
;;;
;;;   \subsubsection 2.1.1 Matrix multiplication.
;;;
;;;    In Quail the matrix multiplication operator is the ``dot-product''
;;;    operator .*
;;;    It takes arbitrarily many arguments.
;;;                                                  T
;;;    For example, we determine the matrix product X X  as

(<- xtx (.* (tp x) x))

;;;           T
;;;     and XX  as

(<- xxt (.* x (tp x)))

;;;   Again the number of arguments is arbitrary provided the dimensions conform

(.* x)
(.* xxt x xtx)
(.* xxt (seq 1 3))

;;;  Note that the conformability is strictly determined from the dimensions
;;;  of the arguments.
;;;  For example, because lists are treated as column vectors this will work

(.* xxt '(1 2 3))

;;;  but (.* '(1 2 3) x) would fail.  To get the dimensions right transpose the
;;;  list first

(.* (tp '(1 2 3)) x)

;;; It is important to remember that ref will collapse any empty dimensions
;;; and so will return a column vector when a row vector might be expected.

(<- x0 (ref x 0 T))
(matrix-dimensions-of x0)

;;; So (.* x0 '(100 200)) will fail but

(.* (tp x0) '(100 200))

;;; will not.  The better solution would be to have ref preserve the empty
;;; dimensions as in

(<- row-0 (ref x 0 T :shape T))
(.* row-0 '(100 200))

;;;
;;; Similarly scalars are treated as 1 by 1 matrices

(.* '(1 2 3)
    100
    (ref x 1 1))

;;;  \subsubsection 2.1.2 Solving systems of linear equations
;;;
;;;  Consider solving a system of linear equations, say the ``normal equations''
;;;  from a least-squares linear regression.
;;;  That is for fixed n by p matrix X and n by 1 vector Y, we solve the
;;;  following matrix equation for the unknown p by 1 vector b.
;;;
;;;         T       T
;;;        X X b = X Y
;;;
;;;          T
;;;  Here's X X

(<- xtx (.* (tp x) x))

;;;
;;;  And suppose y is in fact linearly related to X

(<- trueb (array '(5 7)))
(<- y (+ (.* X trueb)
         (random-gaussian :n 3 :scale .1)))

;;;                          T
;;; The final ingredient is X Y
;;;

(<- xty (.* (tp x) y))

;;;
;;;  And to solve for b
;;;

(solve xtx xty)

;;; which should have values close to the original vector
;;; used above to create y.
;;;
;;; Another way of solving this system would be to make use of any of a
;;; variety of matrix decompositions.  First decompose X (e.g. via QR) or
;;; XTX  (e.g. via Cholesky) and call solve on the resulting decomposition
;;; with second argument y or XTy respectively.
;;; Solve will work with most decompositions.
;;;
;;; In keeping with many statistical systems, the second argument to
;;; solve is optional.
;;; If the second argument is NIL or an identity-matrix, 
;;; solve returns the matrix inverse of its' first argument.

(<- xtx-inv (solve xtx NIL))

;;; or

(solve xtx (identity-matrix (nrows xtx)))

;;; In either case, this is done by calling the function inverse directly
;;; on xtx.
;;; Then the normal equations could be solved equivalently (in Quail, see
;;; following section on inverse) as
;;;

(.* xtx-inv xty)

;;;    \subsubsection 2.1.3 Matrix inversion 
;;;
;;; To invert a square matrix the inverse function is used as in

(<- i-xtx (inverse xtx))

;;; Note that the inverse is not calculated explicitly, until needed.
;;; Instead inverse is a generic function which when called on an square
;;; matrix returns an instance of ``inverse-matrix''.  This instance maintains
;;; a pointer to the original matrix so that calling inverse again will
;;; return the original matrix as in

(eq xtx (inverse i-xtx))

;;; and so we also have information to determine that

(.* i-xtx xtx)

;;; must yield an identity matrix exactly! 
;;;
;;; But note that the inverse is not cached on xtx

(eq (inverse xtx) i-xtx)

;;; This way if xtx changes, and the inverse is asked for again, the correct
;;; result will be obtained.
;;;
;;; Only if the user actually wants to see or use the elements of the inverse
;;; matrix is it ever calculated explicitly and stored; even then, these values
;;; will never be used in a matrix multiplication.  Instead, matrix multiplication
;;; of an inverse-matrix and a second matrix will call the solve function on the
;;; original matrix and the second matrix.
;;;
;;; Should it at any time be desirable to have the inverse explicitly, one need
;;; only get a copy in the usual way using sel

(<- explicit-inv (sel i-xtx))

;;; 
;;; Quail employs the general strategy that where possible any operation performed
;;; on an inverse matrix which is mathematically equivalent to performing another
;;; operation on the original matrix and then inverting the result, the latter
;;; is chosen as the computational strategy.
;;;
;;; To illustrate these points, consider two square full-rank matrices A and B.

(<- a (array (random-gaussian :n 4) :dimensions '(2 2)))
(<- b (array (random-gaussian :n 4) :dimensions '(2 2)))

;;;                     -1      -1
;;; and their inverses A   and B   respectively.

(<- ia (inverse a))
(<- ib (inverse b))

;;;
;;; Then mathematically we have:
;;;
;;;          -1 -1      -1
;;;         A  B  = (BA)
;;;

(- (.* ia ib) (inverse (.* b a)))

;;; which is exact because the operations are now computationally equivalent.
;;;
;;; This was possible because the inverse function caches the matrix it inverted.
;;; The calculation also benefits by suppressing the matrix inverse operation
;;; until last.  The left hand side involves 2 calls to the solve
;;; function and one call to a matrix multiply whereas the right hand side involves
;;; only a single call to the solve function plus the single matrix multiply.
;;;
;;;               
;;; 
;;; Note on numerical stability:
;;;
;;; In most systems, the following route to the solution of the linear system
;;; is to be strictly avoided because of numerical problems.

(<- b-hat (.* (inverse xtx) xty))

;;; This is because explicitly producing a matrix inverse and then multiplying
;;; it by the vector x to get the solution is numerically unreliable for an
;;; ill-conditioned system of equations.
;;;
;;; Because the inverse is calculated only implicitly, the problem does not arise.
;;; When it comes time to do some matrix multiplication with it,
;;; the solve function is used as demanded for numerical stability.
;;;
;;; So the above solution to the normal equations will yield a solution
;;; numerically identical to that obtained earlier by (solve xtx xty).
;;; Compare this to the numerically undesirable approach

(.* explicit-inv xty)

;;; which will in general be different from b-hat.
;;;
;;; It should also be noted that some small differences due to the order of
;;; operations will be obtained if we calculate b-hat directly as the mathematics
;;; would have us do

(.* (inverse (.* (tp x) x)) (tp x) y)

;;; These differences are here primarily because .* has called the function solve
;;; on i-xtx and (tp X) rather than on i-xtx and xty.
;;; Like most operations, mathematically associative
;;; operators like .* are not computationally associative.
;;;
;;; When solve is given a null second argument it simply calls inverse
;;; on its first argument, as in

(<- i-xtx (inverse xtx))

;;;
;;; \subsection 2.2 Numerical properties
;;;
;;;     Here we describe some functions that determine numerical
;;;     properties of matrices.
;;;     These are rank-of, condition-number-of, determinant, and trace-of.
;;;     Some of these are defined only for square matrices so in addition to
;;;     the original matrix X, we will use some square ones defined in the previous
;;;     section.
;;;
;;;
;;;  \subsubsection 2.2.1 Numerical ranks
;;;
;;;  The numerical rank of a matrix is an assessment of the number of
;;;  linearly independent rows or columns in that matrix.
;;;  It is defined to be the number of non-zero singular values in the matrix,
;;;  or more precisely, the number of values above some non-negative threshold.
;;;
;;;  Our original matrix x has dimensions

(dimensions-of x)

;;;  and full column rank.  That is, its' rank is the same as its' number of
;;;  columns.

(rank-of x)

;;;        T
;;;  Now XX  will have the same mathematical rank as X but possibly not the
;;;  same numerical rank

(rank-of xxt)

;;;  Any difference will be due to the floating point errors involved
;;;  in calculation (either of xxt, none here, or of its rank).
;;;  If we examine the singular values of xxt 

(singular-values-of xxt)

;;;  we see that one is extremely small.  Mathematically, it must be zero.
;;;  A tolerance for zero can be given as a keyword argument to rank-of, as
;;;  in 

(rank-of xxt :tolerance 1E-10)

;;;  which in this case should produce the mathematically correct rank.
;;;
;;;
;;;   \subsubsection 2.2.2 Condition numbers.
;;;
;;;   Any n by p matrix that has rank less than the min(n,p) is said
;;;   to be singular and at least one of its' singular values will mathematically
;;;   be exactly zero.
;;;   A numerical measure of that singularity is the ratio of the largest
;;;   to the smallest singular values.  This is called the condition number
;;;   of the matrix and for non-zero real matrices lies between 0 and infinity.
;;;
;;;   Many bounds on the error of floating point calculations are proportional
;;;   to the condition number so it is desirable that it be small.  For some
;;;   applications, this might mean less than 30 for a column scaled matrix.
;;;
;;;   In Quail, the condition number is given as

(condition-number-of x)
(condition-number-of xxt)

;;;   And here we see that x is not too badly ``conditioned'' but that
;;;   xxt is extremely ``ill-conditioned'' and hence xxt is numerically
;;;   near singular.
;;;
;;;   For further discussion of the condition number, see
;;;   Golub and Van Loan ``Matrix Computations'' for numerical properties
;;;   and Belsley  ``Conditioning Diagnostics'' for statistical diagnostic uses.
;;;
;;;   \subsubsection 2.2.3 Determinant
;;;
;;;   The determinant of a square matrix is

(determinant xtx)

(determinant xxt)

;;;
;;;   \subsubsection 2.2.4 Trace
;;;
;;;    The trace of a square matrix is simply the sum of its diagonal elements.

(trace-of xtx)

;;;  Because it is mathematically invariant to cyclic permutations of the
;;;  arguments of a matrix product, up to rounding error, the following should
;;;  produce the same result: 

(trace-of xxt)

;;;  A more general trace operator might be written as

(defun my-trace-of (x)
  "A general trace that operates on any rectangular matrix x."
  (sum (diagonal-of x)))

(my-trace-of x)
(my-trace-of xxt)

;;;
;;;  \subsubsection 2.2.5 Selected algebraic structure.
;;;
;;;    We have already seen the computation of singular values.
;;;    Other functions that are applicable to the decomposition of the matrix
;;;    are sometimes directly available from the matrix.  Singular-values-of
;;;    is one such function. Other functions available on a matrix from its
;;;    singular value decomposition
;;;                      T
;;;             X = U D V
;;;
;;;     are the

(left-singular-vectors-of x)

;;;   or the matrix U, and

(right-singular-vectors-of x)

;;;   or the matrix V.
;;;
;;;   To check, X can be put back together from this structure as

(<- x2 (.* (left-singular-vectors-of x)
           (diagonal (singular-values-of x))
           (tp (right-singular-vectors-of x))))
;;; 
;;;  whose mean absolute element-wise error is quite small, namely

(mean (abs (- x x2)))

;;;  Similarly some functions defined on other matrix decompositions
;;;  also operate directly on the matrix argument.
;;;
