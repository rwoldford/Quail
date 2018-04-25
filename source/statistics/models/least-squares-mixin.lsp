;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               least-squares-mixin.lisp                              
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1990 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     Michael Lewis 1991.
;;;     Greg Anglin 1992.
;;;
;;;
;;;--------------------------------------------------------------------------------
;;;

(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(least-squares-mixin)))

;--------------------------------------------------------------------------
; CLASS: least-squares-mixin
;--------------------------------------------------------------------------

(defclass least-squares-mixin ()  
  ((qrd :initarg :qrd :initform nil :accessor qrd
        :documentation        
        "QR decomposition used in calculations.")   
   (qy :initarg :qy :initform nil :accessor qy
       :documentation       
       "QY is the vector: Q times y, where y is the response matrix")   
   (qty :initarg :qty :initform nil :accessor qty
        :documentation        
        "QTY is the vector: transpose(Q) times y, ~
         where y is the response matrix") )
  (:documentation   
   "Least squares fits have this class as a superclass"))
