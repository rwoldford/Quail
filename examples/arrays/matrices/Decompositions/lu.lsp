;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               lu.lsp                              
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
;;;  This is an example file illustrating the use of the LU decomposition.
;;;
;;;  First there is some discussion of the decomposition itself.
;;;  The remainder of the file gives example code illustrating the
;;;  use of the implementation.
;;;  For general information about matrix decompositions in Quail
;;;  or to see the other decompositions, you might check out

     (edit-file "eg:Arrays;Matrices;Decompositions;overview.lsp")

;;;
;;;  The LU decomposition of the p by p matrix A decomposes A
;;;  into the product of p by p lower triangular matrix L and
;;;  and a p by p upper triangular matrix U.
;;;
;;;  A = L U
;;;
;;;  This is often used to solve a square linear system of equations
;;;
;;;  A x = c.  
;;;
;;;  By decomposing A into the product of triangular matrices LU,
;;;  the solution x can be found by first solving
;;;
;;;  L z = c
;;;
;;;  for the vector z, and then solving the system
;;;
;;;  U x = a 
;;;
;;;  for x.
;;;
;;;  Each of these equations can be solved simply using the backsolve
;;;  operator.
;;;
;;;  For example, consider the following matrix

(<- A (array (random-uniform :n 16) :dimensions '(4 4)))

;;;  and vector

(<- c (array '(1 2 3 4)))

;;;
;;;  First get the LU decomposition of A
;;;

(<- lud (lud-of A))

;;;
;;;  The solution to A x = c can be had directly from the lu decomposition
;;;  as in

(<- x (solve lud c))

;;;
;;;  As it turns out this is exactly the result that is returned when
;;;  solve is called on A itself

(solve A c)

;;;  That's because solve on a matrix argument uses the LU decomposition 
;;;  of that matrix to get the result.
;;;
;;;  Other quantities that can be calculated from the LU decomposition are
;;;  the determinant, (approximate) condition number, the matrix rank,
;;;  and the explicitly calculated inverse
;;;    -1  -1
;;;   U   L
;;;

(determinant lud)
(condition-number-of lud)
(rank-of lud)
(rank-of lud :tolerance 1.0E-10)
(inverse lud)

;;;  
;;;  By looking at the components of the lu decomposition, much information
;;;  can be had ``by hand'', but with some care.  Before, undertaking this
;;;  careful examination of the LINPACK manual would be wise.
;;;  All the information stored on the lud is that provided by the LINPACK
;;;  routine DGECO.
;;;
;;;  The lu-decomposition has three components.  The first is a square matrix
;;;  whose upper triangle is the matrix U.
;;;  Below the diagonal are the multipliers necessary to construct L.
;;;  This square matrix is had as follows:

(lu-of lud)

;;; or equivalently

(lu-of A)

;;;
;;;  And so L can be had as
;;;

(upper-triangle-of lud)

;;;  or equivalently

(upper-triangle-of (lu-of A))

;;;
;;;  The second important piece of information is the vector of pivoting
;;;  information.  This is stored in 

(ipvt-of lud)

;;;
;;;  Finally, the original matrix's numerical conditioning is determined
;;;  by the 

(rcond-of lud)

;;;  If 

(= (+ 1.0 (rcond-of lud))
   1.0)

;;;  then for all intents and purposes, the original matrix is numerically
;;;  singular.
;;;
;;;  Again, for most purposes this kind of detail is unnecessary; it is
;;;  generally recommended that the generic functions solve, determinant
;;;  and so on be used.
