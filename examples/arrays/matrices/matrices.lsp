;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                          Matrices                             
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (C) 1994 Statistical Computing Laboratory, University Of Waterloo
;;;
;;; 
;;;  
;;;  Authors:
;;;     R.W. Oldford 1994.
;;;
;;;
;;;
;;; Matrices in Quail are just Quail arrays having 2 or fewer dimensions
;;; and numerical elements.  As such any function that is applicable to
;;; a general array is also applicable to a matrix.
;;;
;;; Contents:
;;;
;;;    1. Matrix introduction.
;;;       1.1 Matrix dimensions.
;;;           1.1.2 One dimensional matrices.
;;;           1.1.2 Zero dimensional matrices.
;;;
;;;    2. Matrix operations.
;;;           First consider selected operations that the result of matrices
;;;           being special kinds of Quail ref-arrays.
;;;       2.1 Matrix specific unary operations.
;;;           ... rank-of, inverse, trace-of, determinant, 
;;;               condition-number-of,
;;;               diagonal-of, upper-triangle-of,
;;;               singular-values-of,
;;;               left-singular-vectors-of, right-singular-vectors-of
;;;       2.2 Matrix specific n-ary operations.
;;;           ... Dot product for matrix multiplication: .*
;;;               Solving linear systems of equations: solve
;;;       2.3 Special Matrices.
;;;           ... identity-matrix, diagonal
;;;               inverse-matrix
;;;
;;;    3. Matrix decompositions.
;;;       3.1  Singular Value Decomposition.
;;;       3.2  QR Decomposition.
;;;       3.2  LU Decomposition.
;;;       3.2  Cholesky Decomposition.
;;;
;;;    4. Operations inherited from array.
;;;           ... log, sqrt, expt, cos, ...
;;;           ... +, -, *, /
;;;           ... tp
;;;           ... ref, eref, sel
;;;           ... slice and element iteration macros
;;;           ... min, max, ...
;;;
;;;    5. Common Lisp types as matrices.
;;;            CL lists, vectors, and arrays.
;;;
;;;  To go directly to any of these, just search this file for the topic of
;;;  of interest or to go section by section search for \section \subsection
;;;  or \subsubsection.
;;;  However, please note that some results will depend on
;;;  having executed other forms earlier in the file.
;;;


;;;  \section 1. Matrix introduction.
;;;
;;; First we need a matrix x.

(<- x (array '(1 2 3 4 5 6) :dimensions '(3 2)))
x

;;; Note that x is filled by row.  If column-wise order was desired one
;;; need only pass array :column as the value of its keyword argument :fill.

(<- x (array '(1 2 3 4 5 6) :dimensions '(3 2) :fill :column))
x

;;; Either way its dimensions are

(dimensions-of x)

;;; which means a matrix having

(nrows x)

;;; rows and

(ncols x)

;;; columns and

(number-of-elements x)

;;; elements.
;;;
;;; \subsection 1.1 Matrix dimensions.
;;;
;;; Matrices can have 0, 1, or 2 dimensions representing scalars, column vectors,
;;; and general matrices respectively.

(number-of-dimensions x)

;;; Each of these are printed slightly differently from each other and from more
;;; general arrays.
;;;
;;; The original matrix x is an example of a two dimensional matrix.
;;; Any reference of it that preserves shape will also be two dimensional:

(ref x '(0 1) T)
(ref x 0 T :shape T)
(ref x T 0 :shape T)
(ref x 0 0 :shape T)

;;;
;;; \subsubsection 1.1.2 One dimensional matrices.
;;;

;;; For an example of a 1 dimensional matrix, consider column 0 of x

(<- 1d-x (ref x T 0))
(number-of-dimensions 1d-x)

;;; Its' single dimension is reflected in its printed representation.

1d-x
(dimensions-of 1d-x)
(nrows 1d-x)
(ncols 1d-x)
(matrix-dimensions-of 1d-x)

;;; NOTE: All single dimensioned matrices are assumed to be column vectors.
;;; So referencing row 0 of x will also return a column vector!

(<- 1d-x (ref x 0 T))
1d-x
(number-of-dimensions 1d-x)
(dimensions-of 1d-x)
(nrows 1d-x)
(ncols 1d-x)
(matrix-dimensions-of 1d-x)

;;; Had we wanted to preserve the row shape, the call to ref would
;;; include :shape T.
;;; Of course the transpose of an n by 1 column vector will always be
;;; a 1 by n matrix.

(tp 1d-x)

;;;
;;; \subsubsection 1.1.3 Zero dimensional matrices.
;;;
;;; Occasionally a zero dimensional matrix is constructed.
;;; This usually happens by referencing a single element of a matrix
;;; via ref or sel.  For our example here, we will consider using sel
;;; so that we obtain a copy of a piece of x rather than a reference to it.
;;; (Sel is exactly like ref except that sel makes copies.)
;;; Then when we later set the value, we will do so without disrupting the
;;; original x or any other reference to it.
;;;
;;; Here's a 0-dimensional matrix,

(<- 0d-x (sel x 0 0))
(number-of-dimensions 0d-x)
(dimensions-of 0d-x)
(nrows 0d-x)
(ncols 0d-x)
(matrix-dimensions-of 0d-x)

;;; Conceptually the result is a scalar and so has zero (i.e. NIL) dimensions.
;;; However it is distinct from the true scalar obtained by directly accessing
;;; the 0 0 element of x through eref.

(<- x00 (eref x 0 0))

;;; An important consequence of this is that 0d-x can be regarded as a container
;;; whose contents can be changed whereas x00 cannot.
;;; For example, any of

(<- (eref 0d-x)     100)   ;; as a zero dimensional matrix,
(<- (eref 0d-x 0)   100)   ;; as a one dimensional matrix,
(<- (eref 0d-x 0 0) 100)   ;; or as a two dimensional matrix,

;;; will work but the same forms with x00 in place of 0d-x will result in error.
;;; Note that by so doing we have also changed the corresponding element in x
;;; because 0d-x is an refernce to part of x as opposed to a copy of it.
;;;
;;; The distinction between is important when for example a function is called that
;;; needs to return a different value as the contents of 0d-x.  This is often
;;; the case when foreign functions that call Fortran or C are used as these
;;; often return information as side-effects on the contents of input arrays.
;;; 
;;; A simpler illustration of the difference is the case where some structure
;;; maintains a pointer to the value of both x00 and 0d-x.
;;; Suppose we have a list l that points to both values, as in

(<- l (list x00 0d-x))

;;; 0d-x would normally be changed by

(<- (eref 0d-x) 222222222)

;;; and the true scalar x00 by

(<- x00 111111111)

;;; Only the second element of l will contain the updated info

l

;;; and so will be different from

(list x00 0d-x)

;;;  \Section 2. Matrix operations.
;;;
;;;  All of the functions that can be used with arrays can also
;;;  be used with a matrix x.  Some examples typically expected
;;;  of a matrix are its' transpose

(tp x)

;;;  and element-wise arithmetic,

(+ x x x)
(- x x x)
(/ x x x)
(* x x x)

;;; For these operations to work the dimensions of the arguments must conform.
;;; For example, (+ (tp x) x) will fail.
;;; 
;;; If however one of the arguments is a scalar or a 0-dimensional matrix,
;;; the operation will apply to all conformable arguments equally.

(+ x 999999 x)
(+ x 0d-x x)

;;; And so on for the other operators.
;;;
;;; This is also the case for 1-dimensional matrices. Here are two.

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

;;; This also works for two-dimensional matrices when one of the dimensions
;;; is 1.
(+ (tp row-x) x)
(+ x (tp row-x))

;;; When one of the dimensions is 1 (nrows or ncols is 1), then some effort is
;;; made to match the remaining dimension with one of the dimensions of the
;;; other argument and operate over rows or columns accordingly.
;;;
;;; If we have ncols=nrows on the two dimensional argument then some ambiguity
;;; arises and we have arbitrarily selected row-major order.

(<- square-x (array '(1 2 3 4 5 6 7 8 9) :dimensions '(3 3)))
square-x
(+ square-x col-x)
(+ col-x square-x)
(+ (tp col-x) square-x)
(+ square-x (tp col-x))

;;; NOTE: When more than two arguments are given, arithmetic operations occur
;;; in binary fashion proceeding left to right.  As a consequence
;;; (aside from numerical considerations) the arithmetic operations are no
;;; longer associative when operating on matrices some of which are two dimensional
;;; and some of which are one dimensional.
;;; For example
(+ x col-x row-x)

;;; and
(+ col-x x row-x)

;;; are equivalent but (+ col-x row-x x) will fail because (+ col-x row-x) will.
;;; If you are keen to do something different you can take matters into your
;;; own hands for element-wise operations by calling map-element directly.
;;; This will allow you to specify an order.
;;;
;;;
;;; There are a host of Common Lisp mathematical functions that have been
;;; extended to operate on arrays in general.   These are described in more
;;; detail in

(edit-file "eg:Arrays;math-funs.lsp")

;;; Similarly, anything that operates on arrays also operates on matrices.
;;; For the whole ball of wax, see 

(edit-file "eg:Arrays;overview.lsp")

;;;       2.1 Matrix specific unary operations.
;;;           ... rank-of, inverse, trace-of, determinant, 
;;;               condition-number-of,
;;;               diagonal-of, upper-triangle-of,
;;;               singular-values-of,
;;;               left-singular-vectors-of, right-singular-vectors-of

(rank-of x)
(<- sym-x (.* (tp x) x))
(inverse sym-x)
;;;       2.2 Matrix specific n-ary operations.
;;;           ... Dot product for matrix multiplication: .*
;;;               Solving linear systems of equations: solve
;;;  All of the functions that can be used with arrays can also
;;;  be used with a matrix x.  Some examples are

(* 2 x)
(log x)
(sqrt x)
(tp x)
(dimensions-of x)
(eref x 0 1)
(ref x '(:c 0) )
(ref x '(:c 0) 1)
(ref x T 1)


;;;
;;;  There are also some functions that are peculiar to matrices.
;;;
;;;      nrows              ... the number of rows in the matrix.
;;;      ncols              ... the number of columns in the matrix.
;;;
;;;  Most are functions that are related to matrix algebra.
;;;  They include:
;;;      tp                   ... the transpose function,
;;;      .*                   ... the dot product function,
;;;      determinant          ... the determinant of a square matrix,
;;;      trace-of             ... the trace of a matrix,
;;;      rank-of              ... the rank of a matrix,
;;;      condition-number-of  ... the condition number of a matrix,
;;;      solve                ... solves systems of linear equations,
;;;      inverse              ... returns the matrix inverse of a square matrix,
;;;                               or the Moore-Penrose generalized inverse of
;;;                               a rectangular matrix.
;;;  
;;;  as well as several matrix decompositions
;;;      svd-of             ... returns the singular-value decompostion of a
;;;                             matrix,
;;;      qrd-of             ... returns the QR decompostion of a matrix,
;;;      lud-of             ... returns the LU (lower-upper) decomposition,
;;;      cholesky-of        ... returns the Cholesky decomposition.
;;;
;;;  We'll illustrate each of these in turn considering the problem of
;;;  solving a system of linear equations, say the ``normal equations''
;;;  from a least-squares linear regression.
;;;  That is for fixed n by p matrix X and n by 1 vector Y, we solve the
;;;  following matrix equation for the unknown p by 1 vector b.
;;;
;;;         T       T
;;;        X X b = X Y
;;;

(rank-of x)

;;;          T
;;;  Here's X X

(<- xtx (.* (tp x) x))
(trace-of xtx)
(determinant xtx)

;;;
;;;  And suppose y is in fact linearly related to X

(<- trueb (array '(5 7)))
(<- y (+ (.* X trueb)
         (random-gaussian :n 3 :scale .1)))

;;;
;;;  Note that here

(nrows y)
(ncols y)

;;;
;;; but the dimensions of y are
;;;

(dimensions-of y)

;;; To get a row vector it will be necessary to take the transpose of y.
;;; This will be a 2 dimensional matrix having a single row.

(dimensions-of (tp y))

;;;
;;; One way to force the preservation of the 2 dimensional nature of y is to
;;; take its transpose twice.

(dimensions-of (tp (tp y)))

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
;;; In keeping with many statistical systems, the second argument to
;;; solve is optional.
;;; If the second argument is NIL or an identity-matrix, 
;;; solve returns the matrix inverse of its' first argument.

(<- xtx-inv (solve xtx NIL))

;;; or

(solve xtx (identity-matrix (nrows xtx)))


;;;
;;; Then the normal equations could be solved equivalently (in Quail) as
;;;

(.* xtx-inv xty)

;;; Note on numerical stability:
;;;
;;; In most systems, this route to the solution of the linear system
;;; is to be strictly avoided because of numerical problems.
;;; This is because explicitly producing a matrix inverse and then multiplying
;;; it by the vector x to get the solution is numerically unreliable for an
;;; ill-conditioned system of equations.
;;;
;;; In Quail, the problem does not arise because the inverse is calculated only
;;; implicitly.  When it comes time to do some matrix multiplication with it,
;;; the solve function is used as demanded for numerical stability.
;;; Only if the user actually wants to see or use the elements of the inverse
;;; matrix is it ever calculated explicitly and stored; even then, these values
;;; will never be used in a matrix multiplication.
;;;
;;; When solve is given a null second argument it simply calls inverse
;;; on its first argument, as in

(<- i-xtx (inverse xtx))

;;; So we could write down the solution to the normal equations as
;;;

(<- b-hat (.* (inverse xtx) xty))

;;; which would yield a solution numerically identical to that obtained earlier
;;; by (solve xtx xty).
;;;
;;; Should it at any time be desirable to have the inverse explicitly, one need
;;; only get a copy in the usual way using sel

(<- explicit-inv (sel i-xtx))

;;; Using this one could follow the numerically undesirable approach to get
;;;

(.* explicit-inv xty)

;;; which will in general be different from b-hat.

;;;
;;; It should also be noted that some small differences do to the order of
;;; operations will be obtained if we caclulate b-hat directly as the mathematics
;;; would have us do

(.* (inverse (.* (tp x) x)) (tp x) y)

;;; These differences are here primarily because .* has called the function solve
;;; on i-xtx and (tp X) rather than on i-xtx and xty.
;;; Like most operations, mathematically associative
;;; operators like .* are not computationally associative.
;;;
;;; Again because the inverse is only implicit, we can make some effort to have the
;;; operators take note of this and produce calculations that are more nearly correct
;;; We go far along this route by having the inverse remember it is the inverse of
;;; an original object.
;;; For example, xtx is exactly the same object as (inverse i-xtx)

(eq xtx (inverse i-xtx))

;;; and so we also have information to determine that

(.* (inverse xtx) xtx)

;;; must yield an identity matrix exactly!  Compare this to

(.* explicit-inv xtx)
;;; 
;;; Quail employs the general strategy that where possible any operation performed
;;; on an inverse matrix which is mathematically equivalent to performing another
;;; operation on the original matrix and then inverting the result, the latter
;;; is chosen as the computational strategy.
;;;
;;; To make these points consider to square full-rank matrices A and B.

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
;;; The caclulation also benefits by suppressing the matrix inverse operation
;;; until last.  The left hand side involves 2 calls to the solve
;;; function and one call to a matrix multiply whereas the right hand side involves
;;; only a single call to the solve function plus the single matrix multiply.
;;;
;;; Another way of solving this system would be to make use of any of a
;;; variety of matrix decompositions.  Solve will work with most of them.


;;; \section Common Lisp types as matrices.
;;; Any list or Common Lisp vector will be treated as a one dimensional matrix
;;; and may be used as such.  Matrix operations on these will typically return
;;; matrices.
;;; Some examples are

(<- l (list 1 2 3 4 5 6))
l
(dimensions-of l)
(nrows l)
(ncols l)
(tp l)

(<- v (vector 1 2 3 4 5 6))
v
(dimensions-of v)
(nrows v)
(ncols v)
(tp v)

;;; Note that deeper list structure is ignored.
;;;
;;; Common Lisp arrays that have two dimensions and numerical elements are
;;; treated as if they were one or two dimensional Quail matrices.
;;; Often Quail matrix operations on Common Lisp arrays preserve the
;;; CL array data type.  For example ref does not.

(<- cla-1 (make-array 6 :initial-contents '(1 2 3 4 5 6)))
cla-1
(dimensions-of cla-1)
(nrows cla-1)
(ncols cla-1)
(tp cla-1)

(<- cla-2 (make-array '(3 2) :initial-contents '((1 2) (3 4) (5 6))))
cla-2
(dimensions-of cla-2)
(nrows cla-2)
(ncols cla-2)
(tp cla-2)
(.* (tp cla-2) cla-2)
