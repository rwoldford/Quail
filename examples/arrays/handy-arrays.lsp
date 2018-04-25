;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                     Some handy array creation functions                           
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (C) 1994 Statistical Computing Laboratory, University Of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1994.

(in-package q-user)

;;;  Some arrays are created frequently so it is useful to have
;;; a few functions that will create them automatically.
;;;
;;; Here are some that we find useful and so offer to you. Search for
;;; section or the topic that interests you.
;;;
;;;  Contents
;;;    1. Array of ones.
;;;    2. Sequences.
;;;        seq, iseq
;;;    3. Diagonals of matrices and diagonal matrices.
;;;    4. Identity-matrix.
;;;    5. Upper triangular matrices.
;;;
;;;
;;; \section 1.0 Array of ones.
;;;
;;;  The function ones-array creates a numerical array (num-array)
;;; of 1s of the dimensions given.

(ones-array 25)
(ones-array 2 3 5)

;;; \section 2.0 Sequences.
;;;
;;;  There are two functions, seq and iseq, that create arrays and
;;; lists of sequences.
;;;
;;;  Create a sequence in a Quail array

(seq 1 10)
(seq -10 10)
(seq 10 100 10)
(seq 10 -10 -1)
(seq 1.2 1.5 .05)

;;;
;;; Note end points are firm
;;;

(seq 1.2 1.5 .06)

;;; To create a sequenced array of arbitrary dimension use array and seq
;;; together.

(array (seq 1 10) :dimensions '(2 5))

;;;
;;; A list of integers
;;;

(iseq -10 10)
(iseq 10 -10)
(iseq -30 -20)


;;; \section 3.0 Diagonals of matrices and diagonal matrices.
;;;
;;;  Occasionally it is convenient to extract only the diagonal elements
;;; of a square matrix.  These are had using the function diagonal-of
;;;

(<- square (array (iseq 16) :dimensions '(4 4)))
(diagonal-of square)

;;;  Similarly, it is sometimes convenient to construct a diagonal matrix
;;; from the contents of some other matrix.

(diagonal '(1 2 3 4))
(diagonal (diagonal-of square))
(diagonal square)

;;; \section 4.0 Identity-matrix.
;;;
;;;  Identity matrices are very handy and easily constructed.  The
;;;  4 by 4 identity matrix 

(<- I4 (identity-matrix 4))

;;; is an instance of the class identity-matrix.  This has advantages
;;; for storage as

(<- I100000000 (identity-matrix 100000000))

;;; takes as much space as I4.
;;; It also has an advantage at matrix multiply time,

(.* I4 square)

;;; \section 5.0 Upper triangular matrices.
;;;
;;;  As with diagonal-of, we also have upper-triangle-of to create
;;;  an upper triangular matrix whose upper triangle has the same elements
;;;  as the upper triangle of its matrix argument.

