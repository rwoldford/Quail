;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               grapher-var.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;
;;;
;;; some global variables must be customised to the machine
;;;
;;;  Authors:
;;;     G. Desvignes  1988-89
;;;     R.W. Oldford  1988-1992
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------




(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(*max-height-graph*
          *max-width-graph*
          *min-height-graph*
          *min-width-graph*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; global variable definitions
;;;

(defvar *graph-cache-node-labels*           nil)
(defvar *graph-default-node-border*         nil)
(defvar *graph-default-node-font*           wb::*small-graphics-font*
  "Default font used in displaying graphs (i.e. networks).")
(defvar *graph-default-node-label-shade*    nil)

(defvar *max-height-graph*     NIL)
(defun max-height-graph () 
  (if *max-height-graph*
    *max-height-graph*
    (setf *max-height-graph* (- (wb:screen-height) 30))))
(defvar *max-width-graph*      NIL)
(defun max-width-graph () 
  (if *max-width-graph*
    *max-width-graph*
    (setf *max-width-graph* (- (wb:screen-width) 30))))
(defvar *min-height-graph*     200)
(defvar *min-width-graph*      200)


