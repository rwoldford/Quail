;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                          cholesky.lsp                              
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
;;;  This is an example file illustrating the use of the Cholesky decomposition.
;;;
;;;  First there is some discussion of the decomposition itself.
;;;  The remainder of the file gives example code illustrating the
;;;  use of the implementation.
;;;  For general information about matrix decompositions in Quail
;;;  or to see the other decompositions, you might check out

     (edit-file "eg:Arrays;Matrices;Decompositions;overview.lsp")

;;;
;;;  The Cholesky decomposition of the p by p symmetric matrix A
;;;  decomposes A into the product of p by p upper triangular matrix R and
;;;  its transpose, a p by p lower triangular matrix.
;;;       T
;;;  A = R R
;;;
;;;  This is often used to solve a square linear system of equations
;;;
;;;  A x = c.  
;;;
;;;  By decomposing A into the product of triangular matrices,
;;;  the solution x can be found by first solving the triangular system
;;;
;;;   T
;;;  R z = c
;;;
;;;  for the vector z, and then solving the triangular system
;;;
;;;  R x = a 
;;;
;;;  for x.
;;;
;;;  Each of these equations can be solved simply using the backsolve
;;;  operator.
;;;
;;;  For example, consider the following matrix

(<- A (array (random-uniform :n 16) :dimensions '(4 4)))

;;;  which can be turned into a symmetric matrix

(<- A (.* (tp A) A))

;;;  We'll also need a vector

(<- c (array '(1 2 3 4)))

;;;
;;;  First get the Cholesky decomposition of A
;;;

(<- chol (cholesky-of A))

;;;
;;;  The solution to A x = c can be had directly from the decomposition
;;;  as in

(<- x (solve chol c))

;;;
;;;  This is possibly quite different from the result that is returned when
;;;  solve is called on A itself

(solve A c)

;;;  especially if the matrix A is rather ill-conditioned, as approximated
;;;  by the

(condition-number-of chol)

;;;  
;;;  This is because the Cholesky called for here did not permit pivoting
;;;  whereas the LU decomposition underlying the solve function has pivoting
;;;  as the default.
;;;
;;;  If pivoting is desired, the Cholesky permits substantial control on this.
;;;  The generic function cholesky-of takes four keyword arguments: pivot 
;;;  initial final and prompt-if-fail?.
;;;  Default of pivot is NIL.  Pivot is non-null if pivoting is requested,
;;;  null otherwise.
;;;  Default of initial is NIL.  If pivot is non-NIL, then initial can be a list
;;;  of the indices of those diagonal elements which are to be moved the leading
;;;  positions of A during pivoting.If NIL, then there is no restriction
;;;  on the initial columns.
;;;  Default of final is NIL.  If pivot is non-NIL, then final can be a list
;;;  of the indices of those diagonal elements which are to be moved to the
;;;  trailing positions of A during pivoting.  If NIL, then there is no
;;;  restriction on the last columns.
;;;  Default of prompt-if-fail? T Only relevant if no pivoting is allowed.
;;;  If the algorithm fails, should the user be offered the opportunity to
;;;  continue? If NIL, it is assumed that continuation is preferred.
;;;  Continuation will produce a cholesky-decomposition object containing only
;;;  a partial decomposition together with the order of the leading submatrix
;;;  found to not be positive definite and a vector z such that Az = 0 approx.
;;;  These are stored as the slots info and null-vector, respectively.
;;;
;;;
;;;
;;;  Other quantities that can be calculated from the Cholesky decomposition are
;;;  the determinant, (approximate) condition number, the matrix rank,
;;;  and the explicitly calculated inverse
;;;    -1  -T
;;;   R   R
;;;

(determinant chol)
(condition-number-of chol)
(rank-of chol)
(rank-of chol :tolerance 1.0E-10)
(inverse chol)

;;;  
;;;  By looking at the components of the Cholesky decomposition, much information
;;;  can be had ``by hand'', but with some care.  Before, undertaking this
;;;  careful examination of the LINPACK manual would be wise.
;;;  All the information stored on the Cholesky is that provided by the LINPACK
;;;  routines DCHDC (pivoting) and DPOCO (without pivoting).
;;;
;;;  The Cholesky-decomposition has four components.
;;;  The first is a square matrix whose upper triangle is the matrix R.
;;;  Below the diagonal are the multipliers necessary to construct tp(R).
;;;  This square matrix is had as follows:

(upper-triangle-of chol)

;;;
;;;  The second important piece of information is the vector of pivoting
;;;  information.  This is stored in 

(jpvt-of lud)

;;;  and, if pivoting was requested, JPVT(J) contains the index of the
;;;  diagonal element of A that was moved into the J-th position.
;;;
;;;  The original matrix's numerical conditioning is determined
;;;  by the 

(rcond-of chol)

;;;  If 

(= (+ 1.0 (rcond-of chol))
   1.0)

;;;  then for all intents and purposes, the original matrix is numerically
;;;  singular.
;;;
;;;  If the original matrix, A, is close to a singular matrix,
;;;  then there will be an approximate null-vector, z, in the
;;;  sense that ||Az|| = rcond*||A||*||z||.
;;;  This vector is stored as

(null-vector-of chol)

;;;
;;;  Value depends upon whether pivoting was chosen or not.
;;;  If pivoting, info contains the index of the last positive diagonal
;;;  element of the Cholesky factor.
;;;  Otherwise, it contains the order of the leading sub-matrix found
;;;  not to be positive definite.

(info-of chol)

;;;
;;;
;;;  Again, for most purposes this kind of detail is unnecessary; it is
;;;  generally recommended that the generic functions solve, determinant
;;;  and so on be used.

