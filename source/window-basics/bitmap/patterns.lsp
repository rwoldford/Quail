;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               patterns.lisp
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
;;;     H.A. Chipman 1991
;;;     C.B. Hurley 1989-1991
;;;     J.A. McDonald 1988-89
;;;     R.W. Oldford 1989-1992
;;;     J.O. Pedersen 1988-89
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :wb)

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(*patterns* 
           eq-patterns order-patterns 
           *dash*
)))

;;;============================================================
;;; Dashing Patterns
;;;============================================================


(defparameter *dash* '#(5 2))


;;;============================================================
;;; Texture Patterns
;;;============================================================


(defvar *patterns*)

(defun setup-patterns ()
  (declare (special *black-shade*
                    *white-shade*
                    *light-gray-shade*
                    *dark-gray-shade*
                    *gray-shade*
                    *patterns*))
  (setf *patterns* (list *white-shade*  *light-gray-shade* 
                         *gray-shade* *dark-gray-shade* *black-shade*)))

(add-restore-lisp-functions #'setup-patterns)

(eval-when (:load-toplevel) (setup-patterns))

(defun eq-patterns (pattern-1 pattern-2)
  "Test whether two patterns are equal.~
   Patterns may be nil here"
  (cond
   ((and (null pattern-1) (null pattern-2))
    t)
   ((null pattern-1) nil)
   ((null pattern-2) nil)
   (t (eq pattern-1 pattern-2))))




(defun order-patterns (patterns)
  "Orders the patterns in the list given.~
   WARNING: The list *patterns* is used to order the~
   patterns, patterns not in *patterns* will be at the end~
   of the orderd list in any order"
  (declare (special *patterns*))
  (loop for p in *patterns*
        when (member p patterns) 
        collect p into result
        finally 
        (return
         (loop for p in patterns 
               do (pushnew p result)
               finally (return (nreverse result))))))

