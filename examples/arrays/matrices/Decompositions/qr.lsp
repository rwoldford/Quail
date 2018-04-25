;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                          qr.lsp                              
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1995 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1995.
;;;
;;;
;;;-----------------------------------------------------------------------------
;;;

(in-package :quail-user)

;;;
;;;  This is an example file illustrating the use of the QR decomposition.
;;;
;;;  First there is some discussion of the decomposition itself.
;;;  The remainder of the file gives example code illustrating the
;;;  use of the implementation.
;;;  For general information about matrix decompositions in Quail
;;;  or to see the other decompositions, you might check out

     (edit-file "eg:Arrays;Matrices;Decompositions;overview.lsp")

;;;
;;;  The QR decomposition of the n by p matrix X
;;;  decomposes X into the product of an n by p matrix Q and an upper triangular
;;;  matrix R  
;;;  
;;;  X = Q R
;;;
;;;             T
;;;  such that Q Q = Ip
;;;  This is often used to produce the least squares solution to 
;;;
;;;  y = Xb + e 
;;;                                                              T         2
;;;  That is, it is used to find the solution b which minimizes e e = sum ei
;;;  The least-squares solution is
;;;           T  -1 T      -1  T
;;;     b = (X X)  X y  = R   Q y
;;;  
;;;
;;;  For example, consider the following matrix

(<- x (array '(1 2 3 4 5 6)
             :dimensions '(3 2)))
(<- x1 (array '(1 2 2 4 3 6)
              :dimensions '(3 2)))
(<- qrd (qrd-of x))
(<- qrd-sx (qrd-of (sel x) :pivot t))
(<- qrd1 (qrd-of x1))


(rank-of qrd)

(upper-triangle-of qrd)

