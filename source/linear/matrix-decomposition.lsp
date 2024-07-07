;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                           matrix-decomposition.lisp                              
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1992 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     Michael Lewis 1991.
;;;     Greg Anglin 1992.
;;;     R.W. Oldford 1993, 1994.
;;;
;;;
;;;-----------------------------------------------------------------------------
;;;

(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(matrix-decomposition decomposition-mixin clear-decompositions
          qr-decomposition lu-decomposition cholesky-decomposition
          singular-value-decomposition)))


;;;
;;; Mixin separates out matrix-decompositions.       .... rwo
;;;-------------------------------------------------------------------------

(defclass matrix-decomposition (quail-object)
  ((original-matrix :initarg :original-matrix
       :reader original-matrix-of
       :initform '()
       :documentation 
       "Contains a pointer to the original matrix that was decomposed."))
  (:documentation   
   "A class that defines the decomposition of a matrix. Purely an ~
    organizational class."))


;---------------------------------------------------------------------------
;Mixin adds decompositions to matrix
;---------------------------------------------------------------------------

(defclass decomposition-mixin (hash-table-mixin) 
  ((decomposition :initarg :decomposition 
                  :accessor decomposition-of 
                  :initform nil
                  :documentation                 
                  "Hash table of decompositions, or nil"))  
  (:documentation   
   "Mixin for slot to store decompositions of a matrix."))


(defun clear-decompositions (thing)
  "Clears all decompositions cached on this thing. ~
   This is important if the contents of thing have been changed after ~
   decompositions of it were calculated."
  (clear-table thing 'decomposition))

;---------------------------------------------------------------------------
;redefinition of matrix class
;---------------------------------------------------------------------------

(defclass matrix (decomposition-mixin num-array)
  ())

#+:aclpc(acl-mop::finalize-inheritance (find-class 'qk::matrix))

