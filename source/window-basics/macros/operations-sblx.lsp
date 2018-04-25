;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                          operations-sblx.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;  This file is part of the Window-Basics package intended to provide a uniform
;;;  window environment for Common Lisp programming.
;;;  
;;;  For copyright information and history, see window-basics-copyright.lisp
;;;  Authors:
;;;     G.W. Bennett 1995 2017
;;;     H.A. Chipman 1991
;;;     C.B. Hurley 1989-1991
;;;     J.A. McDonald 1988-89
;;;     R.W. Oldford 1989-1991
;;;     J.O. Pedersen 1988-89
;;;     
;;;----------------------------------------------------------------------------------

(in-package :wb)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(boole-to-op)
        ))

#|
(defmacro boole-to-op (value)
   (declare (special cg::po-replace cg::po-paint cg::po-invert cg::po-erase))
  `(case ,value
     (:default       cg::po-replace)
     (,boole-1       cg::po-replace)
     ;(,boole-c1      (+ cg::d~st cg::d~s~t cg::~d~st cg::~d~s~t))
     (,boole-ior     cg::po-paint)
     ;(,boole-orc1    (+ cg::d~st cg::~d~st cg::dst cg::d~s~t cg::~d~s~t cg::ds~t))
     (,boole-xor     cg::po-invert)
     ;(,boole-eqv     (+ cg::dst cg::~d~st cg::ds~t cg::~d~s~t))
     (,boole-andc1   cg::po-erase)
     ;;(,boole-and     cg::dst)
     ;(,boole-and     (+ cg::dst cg::ds~t))
     (otherwise NIL)))

(defmacro boole-to-op (value)
   (declare (special cg::po-replace cg::po-paint cg::po-invert cg::po-erase))
  `(cond
     ((eq ,value :default)       cg::po-replace)
     ((eq ,value :boole-1)      cg::po-replace)
     ((eq ,value :boole-c1)      (+ cg::po-d~st cg::po-d~s~t cg::po-~d~st cg::po-~d~s~t))
     ((eq ,value :boole-ior)    cg::po-paint)
     ((eq ,value :boole-orc1)    (+ cg::po-d~st cg::po-~d~st cg::po-dst cg::po-d~s~t cg::po-~d~s~t cg::po-ds~t))
     ((eq ,value :boole-xor)     cg::po-invert)
     ((eq ,value :boole-eqv)     (+ cg::po-dst cg::po-~d~st cg::po-ds~t cg::po-d~s~t))
     ((eq ,value :boole-andc1)   cg::po-erase)
     ((eq ,value :boole-and)     cg::po-dst)
     ((eq ,value :boole-and)     (+ cg::po-dst cg::po-ds~t))
     (T cg::po-replace)))
|#
;;; Until I know better what to do here, take the results from operations-clx.lsp
(defmacro boole-to-op (value)
  `(case ,value
     (:default       boole-1)
     (:boole-1       boole-1)
     (:boole-c1      boole-c1)
     (:boole-ior     boole-ior)
     (:boole-orc1    boole-orc1)
     (:boole-xor     boole-xor)
     (:boole-eqv     boole-eqv)
     (:boole-andc1   boole-andc1)
     (:boole-and     boole-and)
     (otherwise NIL)))
