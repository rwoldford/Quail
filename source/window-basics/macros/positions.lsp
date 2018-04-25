;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               positions.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;
;;;  This file is part of the Window-Basics package intended to provide a uniform
;;;  window environment for Common Lisp programming.
;;;  
;;;  For copyright information and history, see window-basics-copyright.lisp
;;;
;;;  Authors:
;;;     C.B. Hurley 1989-1991
;;;     J.A. McDonald 1988-89
;;;     R.W. Oldford 1989-1991
;;;     J.O. Pedersen 1988-89
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :wb)

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(make-position copy-position position-p position-x
           position-y)))

;;;============================================================
;;; Positions
;;;============================================================
;;; changed representation of positions from cons to vectors, jam, 11-17-88.
;;; --makes it easier to extend to 3d
;;; --makes it possible to implement some graphics operations
;;; on the CM, because you can have array valued pvars,
;;; but not cons valued pvars.

(defmacro make-position (&optional (x 0) (y 0)) `(vector ,x ,y))

(defmacro copy-position (position)
  (let ((sym (gensym)))
    `(let ((,sym ,position))
       (vector (aref ,sym 0) (aref ,sym 1)))))

(defmacro position-p (object)
  (let ((sym (gensym)))
    `(let ((,sym ,object))
       (and (vectorp ,sym)
	    (= (length ,sym) 2)))))

(defmacro position-x (object) `(aref ,object 0))
(defmacro position-y (object) `(aref ,object 1))
(defsetf position-x (position) (new-value)
  `(setf (aref ,position 0) ,new-value))
(defsetf position-y (position) (new-value)
  `(setf (aref ,position 1) ,new-value))
