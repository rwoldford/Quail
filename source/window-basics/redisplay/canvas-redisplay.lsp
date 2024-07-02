;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                          canvas-redisplay.lisp
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
;;;     R.W. Oldford 1989-1992
;;;     H.A. Chipman 1991
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :wb)


(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(canvas-redisplay-mixin set-redisplay-fn set-redisplay-test
           redisplay  redisplay-p redisplay-fn-of redisplay-p-of display-of
           canvas-display)))


;;; Give canvas a hook to the thing it's displaying. This is 
;;; necessary to get the Mac windows to refresh themselves
;;; properly.
;;;
;;; NOTE: the redisplay function needs to be defined for any particular
;;; application.  Here, we just have a useless dummy definition.

(defun default-redisplay-fn (canvas) (declare (ignore canvas)))
(defun default-redisplay-p (canvas)
	"This dummy function returns NIL. ~
	 It is here in case some canvas types wish to have ~
         redisplay tests peculiar to the individual canvas."
	 (declare (ignore canvas)) 
	NIL)

(defclass canvas-redisplay-mixin ()
  ((display :initarg :display
            :initform nil
            :accessor display-of
            :documentation "The data structure to be displayed in ~
                            this canvas.")
   (redisplay-fn :initarg :redisplay-fn
                 :initform #'default-redisplay-fn
                 :accessor redisplay-fn-of
                 :documentation "A function applied to the canvas ~
                                 when redisplay-p yields true when ~
                                 applied to the canvas.")
   (redisplay-p :initarg :redisplay-p
                :initform #'default-redisplay-p
                :accessor redisplay-p-of
                :documentation "A test function applied to the canvas ~
                                which yields true when ~
                                the canvas contents should be redisplayed.")
   )
  (:documentation
   "Mixin to canvas so that it will redraw it's display")
  )

(defun redisplay (canvas)
  "Redisplays the canvas by applying the canvas's redisplay-fn ~
   to the canvas."
  (funcall (redisplay-fn-of canvas) canvas))

(defun redisplay-p (canvas )
  "Yields true when ~
   the canvas contents should be redisplayed."
  (or (and (canvas-visible-p canvas)
           (not (at-top-p canvas)))
      (and (at-top-p canvas)
           (just-selected-p canvas))
      (funcall (redisplay-p-of canvas) canvas))
  )

(defun set-redisplay-fn (canvas new-fn)
  "Sets the redisplay function of the canvas to new-fn."
  (setf (redisplay-fn-of canvas) new-fn))

(defun set-redisplay-test (canvas new-fn)
  "Sets the redisplay test function of the canvas to new-fn."
  (setf (redisplay-p-of canvas) new-fn))

;;; An additional fun to grab the display ... rwo

(defun canvas-display (canvas)
  (display-of canvas))

(defun just-selected-p (canvas)
  (declare (special *just-selected*))
  (eq canvas *just-selected*))

(defvar *just-selected* NIL)
