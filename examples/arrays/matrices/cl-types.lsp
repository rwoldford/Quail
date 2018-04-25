;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                    Common Lisp types as matrices                          
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
;;; For an introduction to matrices in Quail see
;;;
         (edit-file "eg:Matrices;overview.lsp")
;;;
;;;
;;; \section Common Lisp types as matrices.
;;;
;;; Any list or Common Lisp vector will be treated as a one dimensional matrix
;;; (i.e. column vector) and may be used as such.
;;; Matrix operations on these will typically return matrices.
;;; Some examples are

(<- l (list 1 2 3 4 5 6))
l
(dimensions-of l)
(matrix-dimensions-of l)
(nrows l)
(ncols l)
(tp l)

(<- v (vector 1 2 3 4 5 6))
v
(dimensions-of v)
(matrix-dimensions-of v)
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
(matrix-dimensions-of cla-1)
(nrows cla-1)
(ncols cla-1)
(tp cla-1)

(<- cla-2 (make-array '(3 2) :initial-contents '((1 2) (3 4) (5 6))))
cla-2
(dimensions-of cla-2)
(matrix-dimensions-of cla-2)
(nrows cla-2)
(ncols cla-2)
(tp cla-2)
(.* (tp cla-2) cla-2)
