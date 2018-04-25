;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                          Matrix overview                         
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
;;; For an introduction to arrays in Quail see
;;;
         (edit-file "eg:Arrays;overview.lsp")
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

         (edit-file "eg:Arrays;Matrices;intro.lsp")

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

         (edit-file "eg:Arrays;Matrices;operations.lsp")
;;;
;;;    3. Matrix decompositions.

         (edit-file "eg:Arrays;Matrices;Decompositions;overview.lsp")

;;;       3.1  LU Decomposition.
             (edit-file "eg:Arrays;Matrices;Decompositions;lu.lsp")
;;;       3.2  Cholesky Decomposition.
             (edit-file "eg:Arrays;Matrices;Decompositions;cholesky.lsp")
;;;       3.3  Singular Value Decomposition.
             (edit-file "eg:Arrays;Matrices;Decompositions;svd.lsp")
;;;       3.4  QR Decomposition.
             (edit-file "eg:Arrays;Matrices;Decompositions;qr.lsp")
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

      (edit-file "eg:Arrays;Matrices;cl-types.lsp")
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Matrix specific n-ary operations.
;;;           ... Dot product for matrix multiplication: .*
;;;               Solving linear systems of equations: solve
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
