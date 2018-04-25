;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               graph-var.lisp
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
;;;     R.W. Oldford  1988-1991
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------




(in-package :graph)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(*max-height-graph*
          *max-width-graph*
          *min-height-graph*
          *min-width-graph*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; global variable definitions
;;;

(defvar *graph-cache-node-labels*           nil)
(defvar *graph-default-font*                '("helvetica" 10))
(defvar *graph-default-node-border*         nil)
(defvar *graph-default-node-font*           nil)
(defvar *graph-default-node-label-shade*    nil)

(defvar *max-height-graph*     (/ wb:*screen-height* 2))
(defvar *max-width-graph*      (/ wb:*screen-width* 2))
(defvar *min-height-graph*     30)
(defvar *min-width-graph*      100)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; constant definition
;;;

(unless (boundp '*graph-origin*)
  (defconstant *graph-origin*  (make-position 0 0)))

