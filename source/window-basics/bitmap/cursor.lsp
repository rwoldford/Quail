;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               cursor.lisp
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
;;;  Authors:
;;;     R.W. Oldford 1991
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :wb)

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(*box-cursor* *circle-cursor*
           restore-circle-cursor restore-box-cursor)))

(defvar *box-cursor* NIL)

(defun restore-box-cursor ()
  "Restores the value of *box-cursor*"
  (declare (special *box-cursor*))
  (setf *box-cursor*
        (make-cursor
         (make-box-bitmap 16 16)
         :mask (make-blank-bitmap 16 16))))

(eval-when (load) (restore-box-cursor))

(defvar *circle-cursor* NIL)

(defun restore-circle-cursor ()
  "Restores the value of *circle-cursor*"
  (declare (special *circle-cursor*))
  (setf *circle-cursor*
        (make-cursor
         (make-circle-bitmap 16)
         :mask (make-blank-bitmap 16 16)
         :hotspot (h-draw:make-point 8 8))))

(eval-when (load) (restore-circle-cursor))

(add-restore-lisp-functions #'restore-box-cursor #'restore-circle-cursor)
