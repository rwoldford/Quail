;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                          Matrix introduction                           
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
;;;    1. Matrix introduction.
;;;       1.1 Matrix constructors
;;;            - array, diagonal, identity-matrix, ones, seq
;;;       1.2 Matrix selectors      
;;;            - eref, ref, and sel.
;;;            - diagonal-of, upper-triangle-of
;;;       1.3 Matrix dimensions.      
;;;           - dimensions-of, nrows, ncols, matrix-dimensions-of,
;;;             number-of-elements
;;;           1.3.1 One dimensional matrices.
;;;           1.3.2 Zero dimensional matrices.
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
;;;  \section 1. Matrix introduction.
;;;
;;; Matrices in Quail are just Quail arrays having 2 or fewer dimensions
;;; and numerical elements.  As such any function that is applicable to
;;; a general array is also applicable to a matrix.
;;; They are implemented as a class matrix.
;;; 
;;; In this section, we give a brief introduction to the basic use of matrices.
;;;
;;; \subsection 1.1  Matrix constructors.
;;;
;;; There are many functions that can be used to construct a matrix.
;;; The most common and most general is the array function.
;;; It can be used to construct arrays of arbitrary dimensions and contents and
;;; is discussed at greater length in the introduction to arrays contained in

(edit-file "eg:Arrays;intro.lsp")

;;;  Here we illustrate only rather basic usage.
;;;  For example here is a 3 by 2 matrix x

(<- x (array '(1 2 3 4 5 6) :dimensions '(3 2)))

;;;  whose contents you can see by evaluating

x

;;; The first argument to array is the contents argument.
;;; This is a list or other ref'able structure whose
;;; elements are to be stored as the contents of the array.
;;; The dimensions of the array are determined by the value of the keyword
;;; argument of that name.  If missing, the default is determined from the
;;; structure of the contents argument.  So the matrix x could also have
;;; been constructed as

(<- x (array '((1 2)
               (3 4)
               (5 6)) 
             ))

;;; A number of other keywords exist for the array function.
;;;
;;; Here we note only that the order by which the elements of x are filled
;;; can be changed through the value of the keyword argument fill.
;;; By default x is filled by row.  If column-wise order was desired one
;;; need only pass array :column as the value of its keyword argument :fill.

(<- x (array '(1 2 3 4 5 6) :dimensions '(3 2) :fill :column))

;;; with the result

x

;;; A number of matrices and matrix types crop up frequently enough that it
;;; is convenient to have special constructors to produce them.
;;; A general treatment of these is given in 

(edit-file "eg:Arrays;handy-arrays")

;;; Here we quickly illustrate their use in constructing matrices.
;;;
;;; The first is a diagonal matrix (i.e. having zero above- and below-diagonal
;;; elements).  For this we have the constructor diagonal, as in

(diagonal '(1 2 3 4))

;;; or 

(diagonal '(1 1 1 1))

;;; This last case is really a 4 by 4 identity matrix and could be constructed
;;; instead using

(identity-matrix 4)

;;; In this case however, it returns a genuine identity-matrix object rather
;;; than an instance of a matrix whose contents are numerically equal to those
;;; of an identity matrix.
;;; The distinction allows storage to be minimized for large identity-matrices
;;; and also makes possible methods which are specialized to work with identity
;;; matrices.
;;;
;;; Similarly, a 3 by 2 matrix of ones can be produced as

(ones 3 2)

;;; Despite its printed representation, this returns an instance of the class
;;; ones-matrix.

(<- ones (ones 3 2))
(class-of ones)

;;;
;;;  Matrices whose contents are to be a regular sequence of numbers can
;;;  be constructed using the function seq.  As in

(seq 1 5)
(seq -10 -5)
(seq -10 -15)
(seq 1 2 .1)

;;;
;;; Finally, array is clever enough to use pretty much any ref'able object
;;; as its' initial contents.

(array (seq 1 24) :dimensions '(8 3))
(array (identity-matrix 4) :dimensions '(16 1))
(array (random-uniform :n 16) :dimensions '(4 4))


;;; \subsection 1.2 Matrix selectors      
;;;            - eref, ref, and sel.
;;;            - diagonal-of, upper-triangle-of
;;;
;;;  Matrix elements are selected as with any Quail array, by using eref
;;;  (as a mnemonic device, consider it short for ``element-reference'')
;;;  All indexing is zero-based.  So for x, we have (eref x i j)
;;;  selecting the (i,j)'th element of x.  As in

(eref x 0 0)
(eref x 0 1)
(eref x 1 0)
(eref x 1 1)
(eref x 2 0)
(eref x 2 1)


;;;  Similarly, referencing and copying whole blocks of a matrix use
;;;  the generic array functions ref (for ``reference'') and sel (for ``select'')
;;;  as referencing the top 2 by 2 block of X in any of the followinf equivalent
;;;  ways:

(ref x '(0 1) '(0 1))
(ref x '(0 1) T)
(ref x '(0 1) )
(ref x '(:c 2) '(0 1))
(ref x '(:c 2) T)
(ref x '(:c 2) )

;;; Rows, columns, can be repeated as in

(ref x '(0 0 0) T)
(ref x '(0 1 0) '(0 0 1))

;;; Replacing ``ref'' by ``sel'' in any of these statements produces a copy
;;; of the block structure but which contains the identical elements.
;;; The principal difference is that changing the contents of a cell
;;; in a reference causes the contents of the corresponding cell of the
;;; original object to change to the same thing.
;;; If the change is made to a cell in a copy of the matrix, then the change does
;;; not get reflected in the original.
;;;
;;; For greater discussion, see

(edit-file "eg:Arrays;intro.lsp")

;;; and

(edit-file "eg:Arrays;ref.lsp")

;;;  Besides the element extraction function, eref, and the whole block
;;;  extraction functions, ref and sel,
;;;  one often wants the upper triangle of a matrix, as an upper triangular matrix,
;;;  or perhaps just its diagonal elements.
;;;  So for convenience, Quail also provides the functions upper-triangle-of,
;;;  and diagonal-of as shown below.

(upper-triangle-of x)

;;;  Note the zero below the diagonal.
;;;  The diagonal is extracted as

(diagonal-of x)

;;; and is sometimes used in conjunction with the ``diagonal'' constructor
;;; to produce a diagonal matrix from the diagonal of another matrix as in

(diagonal (diagonal-of x))

;;;  Note that both upper-triangle-of and diagonal-of operates on 
;;;  matrices of arbitrary shape.
;;;
;;;
;;; \subsection 1.3 Matrix dimensions.
;;;
;;;  There are a number of functions for asking about the size of the matrix.
;;;  All of them relate ultimately to the dimensions of the matrix.
;;;  For our matrix x, we have

(dimensions-of x)

;;; which means a matrix having

(nrows x)

;;; rows and

(ncols x)

;;; columns and

(number-of-elements x)

;;; elements.
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
;;; \subsubsection 1.3.1 One dimensional matrices.
;;;

;;; For an example of a 1 dimensional matrix, consider column 0 of x

(<- 1d-x (ref x T 0))
(number-of-dimensions 1d-x)

;;; Its' single dimension is reflected in its printed representation.

1d-x
(dimensions-of 1d-x)
(nrows 1d-x)
(ncols 1d-x)

;;; The following always treats the argument as if it were a matrix
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
;;; \subsubsection 1.3.2 Zero dimensional matrices.
;;;
;;; Occasionally a zero dimensional matrix is constructed.
;;; This usually happens by referencing a single element of a matrix
;;; via ref or sel.  For our example here, we will consider using sel
;;; so that we obtain a copy of a piece of x rather than a reference to it.
;;; \footnote{Sel is exactly like ref except that sel makes a copy and drops
;;; the pointer to the original data structure.}
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
x00

;;; They are different objects

(eq x00 0d-x)

;;; but because zero-dimensional objects are treated as scalars

(= x00 0d-x)
(> x00 0d-x)
(< x00 0d-x)
(>= x00 0d-x)
(<= x00 0d-x)

;;; An important consequence of this difference is that 0d-x can be regarded as a
;;; container whose contents can be changed whereas x00 cannot.
;;; For example, any of

(<- (eref 0d-x)     100)   ;; as a zero dimensional matrix,
(<- (eref 0d-x 0)   100)   ;; as a one dimensional matrix,
(<- (eref 0d-x 0 0) 100)   ;; or as a two dimensional matrix,

;;; will work but the same forms with x00 in place of 0d-x will result in error.
;;; Note that had we used ref instead of sel in constructing x00, these assignments
;;; would have changed the corresponding element in x as well.
;;; This is why we chose to make 0d-x a copy of part of x rather than a reference to
;;; it.
;;;
;;; Some functions (particularly foreign functions that call Fortran or C) return the
;;; results by changing the contents of arrays passed as arguments.
;;; In such cases, passing the element x00 would fail whereas passing the ``container''
;;; 0d-x would be just right.
;;; 
;;; A simpler illustration of the difference is the case where some structure
;;; maintains a pointer to the value of both x00 and 0d-x.
;;; Suppose we have a list l that points to both values, as in

(<- some-list (list x00 0d-x))
some-list

;;; 0d-x would normally be changed by

(<- (eref 0d-x) 222222222)

;;; and the true scalar x00 by

(<- x00 111111111)

;;; Only the second element of l will contain the updated info

some-list

;;; and so will be different from

