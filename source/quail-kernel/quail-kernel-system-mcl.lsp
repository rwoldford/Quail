;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               quail-kernel-system-mcl.lisp                              
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1990 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     Greg Anglin 1990, 1991.
;;;
;;;
;;;--------------------------------------------------------------------------------

(in-package :quail-kernel)

(defun system-get-pointer (x)
  #+:ccl-2
  (ccl::%address-of x)
  #-:ccl-2
  (ccl:%ptr-to-int (ccl:%get-ptr (list x))))


