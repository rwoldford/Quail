;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                            overview.lsp                              
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

;;;  In this and subsequent files we discuss matrix decompositions that
;;;  are currently available in Quail.
;;;
;;;  Contents:
;;;
;;;    3.0 Matrix decompositions
;;;       3.1  LU Decomposition.
;;;       3.2  Cholesky Decomposition.
;;;       3.3  Singular Value Decomposition.
;;;       3.4  QR Decomposition.
;;;
;;;  To go directly to any of these, just search this file for the topic of
;;;  of interest or to go section by section search for \section \subsection
;;;  or \subsubsection.
;;;  However, please note that some results will depend on
;;;  having executed other forms earlier in the file.
;;;
;;;  See also the matrix overview:
     (edit-file "eg:Arrays;Matrices;overview.lsp")

;;;  See also to the array overview:
     (edit-file "eg:Arrays;overview.lsp")

;;;
;;;  \section 3.0 Matrix decompositions
;;;
;;;  Matrix decompositions are useful mathematically and computationally.
;;;  By decomposition, we mean here that a collection of matrices are
;;;  found such that their product will yield the original matrix.
;;;  The individual matrices are chosen to have certain properties
;;;  (e.g. orthonormal columns, diagonal, or triangular structure)
;;;  which are either mathematically or computationally convenient.
;;;
;;;  For example the LU decomposition decomposes a square matrix A
;;;  into the product of a lower triangular matrix L and an upper
;;;  triangular matrix U so that A = LU.
;;;
;;;  In Quail every kind of decomposition is a subclass of the class
;;;  ``matrix-decomposition.''  For example, the subclass 
;;;  lu-decomposition represents the LU decomposition and an instance
;;;  of this class represents the lu decomposition of a particular matrix.
;;;  The general matrix-decomposition class is pretty much abstract
;;;  and is intended to define the common behaviours of its sublasses.
;;;
;;;  The following table lists the decompositions and the uses that they have 
;;;  in common.  The actual uses are illustrated in the subsections which follow.
;;;  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;     The decompositions and their common use
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; decomposition:         Singular-     LU   QR   Cholesky
;;;                        value (svd)
;;; -----------------------------------------------------
;;; Methods
;;;
;;; condition-number-of     Yes         Yes   No     Yes  
;;; determinant             Yes         Yes   No     Yes  
;;; inverse                 No          Yes   No     Yes
;;; rank-of                 Yes         Yes   Yes    Yes
;;; solve                   No          Yes   Yes    No
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;;  In general, if the matrix decomposition of interest is ``foo'', say,
;;;  then the foo decomposition of the matrix A is had by calling (foo-of  A).
;;;  This calculates and returns an instance of ``foo-decomposition.''
;;;  This instance can then be used as an argument to the functions
;;;  listed in the above table and to functions that are of peculiar interest
;;;  to a foo decomposition.
;;;
;;;  Some of these functions can be called directly on a matrix, not just on
;;;  a matrix decomposition.
;;;  In this case, Quail chooses whichever decomposition seems most appropriate
;;;  for the situation.
;;;
;;;  For example, calling ``condition-number-of'' on a matrix
;;;  A will cause the singular values of A to be determined and the ratio of the
;;;  largest to smallest returned.
;;;  But calling it on a specific ``foo-decomposition'' instance, will return 
;;;  an estimate of this ratio as determined from the information directly 
;;;  available on that decomposition instance and not necessarily from the
;;;  singular values themselves.
;;;
;;;  With this control over which decomposition to use in a particular situation
;;;  one can, for example, choose not to incur the additional cost of a singular 
;;;  value decomposition given that one has an lu-decomposition in hand.
;;;  The approximation available from the LU may good enough for the purpose
;;;  at hand.
;;;  
;;;  Another means of saving calculation costs is by caching decompositions
;;;  directly on the matrix.
;;;  So in addition to calculating the ``foo-decomposition'' for a matrix A,
;;;  (foo-of A) will also cache the returned instance on A.
;;;
;;;  This caching facility has been achieved by mixing the class
;;;
;;;       decomposition-mixin
;;;
;;;  into the class matrix.  That is the class matrix is a subclass
;;;  of the class decomposition-mixin (as well as other classes).
;;;
;;;  Decomposition-mixins maintain a table of cached decompositions.
;;;  When a decomposition is requested, the table is first consulted.
;;;  If the decomposition has already been stored, it is returned.
;;;  If not, it is calculated and stored before being returned.
;;;
;;;  This saves repeated calculation but it does have the potentially
;;;  *dangerous* consequence that if the matrix elements are changed
;;;  *after* the decomposition has been calculated, then the decomposition
;;;  will no longer be that of the matrix!
;;;  In this case, the decompositions should be cleared after the matrix
;;;  has been changed.
;;;  The function which does this is clear-decompositions.
;;;
;;;  For example, suppose we have X

(<- X (array (iseq 16) :dimensions '(4 4)))

;;;  and calculate its' LU decomposition.

(<- lud (lud-of X))

;;;  Now we change the (0 0) element of X from

(eref X 0 0)

;;;  to 30.

(<- (eref X 0 0) 30)

;;;  If we ask for the lu decomposition of X we will get the old one which will
;;;  now be incorrect.

(eq lud (lud-of X))

;;;  So we first

(clear-decompositions X)

;;;  and now

(eq lud (lud-of X))

;;;  is false.  Future requests for decompositions of X will be correct.
;;;
;;;  In the following sections we only briefly illustrate the particular
;;;  decompositions; more detailed information on each is given in separate
;;;  files.
;;;  To illustrate them, we will need the following matrices:
;;;  

(<- rect-X       (array (iseq 40) :dimensions '(10 4)))
(<- square-X     (array (iseq 16) :dimensions '(4 4)))
(<- sym-X        (.* (tp rect-X) rect-X))
(<- random-X     (array (random-uniform :n 16) :dimensions '(4 4)))
(<- rand-sym-X   (.* (tp random-X) random-X))

;;;  And some y vectors for solving equations.

(<- y10   (random-gaussian :n 10))
(<- y4    (random-gaussian :n 4))
;;;
;;;   \subsection 3.1 LU Decomposition.
;;;
;;;  The lu (lower upper triangular) decomposition of square real matrix.
;;;  This decomposition is discussed in greater detail in 

     (edit-file "eg:Arrays;Matrices;Decompositions;lu.lsp")

;;;  Here we only illustrate the functions described in the introductory
;;;  section.

(lud-of square-X)
(lud-of sym-X)
(lud-of random-X)

;;;  And you can save it.

(<- lud (lud-of square-X))

;;; Now square-X is not a full rank matrix.  This is reflected in
;;; the rank-of the lud

(rank-of lud)
(rank-of lud :tolerance 1E-10)
(condition-number-of lud)
(determinant lud)

;;; Calculating the inverse would result in error.
;;; Instead, let's work with the full rank matrix random-X.

(<- lud1 (lud-of random-X))
(rank-of lud1)
(rank-of lud1 :tolerance 1E-10)
(condition-number-of lud1)
(determinant lud1)

;;; And so the inverse can be calculated
(inverse lud1)

;;; and used.
(.* (inverse lud1) random-X)

;;; Note that this the explicit inverse and so not as reliable as calculating
;;; the inverse of random-X.
(.* (inverse random-X) random-X)

;;;
;;; And we can force the solution of the equation Ab=c
;;; to be via the LU decomposition

(solve lud1 y4)

;;; which returns b where A is random-X and c is y4.
;;;

;;;  \subsection 3.2  Cholesky Decomposition.
;;;
;;;  The Cholesky (triangular) decomposition of square symmetric real matrix.
;;;  This is mathematically equivalent to the LU when L = tp(U).
;;;  This decomposition is discussed in greater detail in 

     (edit-file "eg:Arrays;Matrices;Decompositions;cholesky.lsp")

;;;  Here we only illustrate the functions described in the introductory
;;;  section.
;;;
;;; First we need a full rank symmetric matrix.

(<- full-rank-sym-x (.* (tp random-X) random-X))

;;; and its' Cholesky decomposition.

(setf chol (cholesky-of full-rank-sym-x))

;;; 

(rank-of chol)
(condition-number-of chol)
(determinant chol)
(inverse chol)
(.* full-rank-sym-x (inverse chol))

;;; For less than full rank matrices, pivoting is an option

(setf c-sym-piv (cholesky-of sym-x :pivot T))
(rank-of c-sym-piv :tolerance 1E-5)
(condition-number-of c-sym-piv)
(determinant c-sym-piv)

;;;
;;;   \subsection 3.3  Singular Value Decomposition.
;;;
;;;  The singular value decomposition of an arbitrary real matrix.
;;;  This decomposition is discussed in greater detail in 

     (edit-file "eg:Arrays;Matrices;Decompositions;svd.lsp")

;;;  Here we only illustrate the functions described in the introductory
;;;  section.
;;;

(svd-of rect-X)
(svd-of square-X)
(svd-of sym-X)

(<- svd (svd-of rect-X))

;;; The applicable functions

(condition-number-of svd)
(rank-of svd)
(rank-of svd :tolerance 1E-10)

;;;  Important note:
;;;    svd is the decomposition used for the matrix versions of
;;;    these functions.

(condition-number-of rect-X)
(rank-of rect-X :tolerance 1E-10)

;;;  \subsection 3.4  QR Decomposition.
;;;
;;;  This decomposition is discussed in greater detail in 

     (edit-file "eg:Arrays;Matrices;Decompositions;qr.lsp")

;;;  Here we only illustrate the functions described in the introductory
;;;  section.
;;;
;;;  The QR decomposition of an arbitrary real matrix.

(qrd-of rect-X)
(qrd-of square-X)
(qrd-of sym-X)

(<- qrd (qrd-of rect-X))

;;; Rank

(rank-of qrd)
(rank-of qrd :tolerance 1.0E-10)

;;; And solving Ab = c when (nrows A) > (ncols A) will produce
;;; the least-squares solution of b

(solve qrd y10)

;;; Note that in this instance solve returns two values.  The first is the
;;; least-squares solution vector; the second is an instance of a qr-solution
;;; object which contains more complete information on the least-squares solution.
;;;
