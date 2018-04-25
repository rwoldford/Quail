;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               host-menu-canvas-mcl.lisp
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
;;;     R.W. Oldford 1989-1991
;;;     
;;;
;;;
;;;-------------------------------------------------------------------
;;;

(in-package :wb)

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(*last-menubar* *system-default-menubar*)))

(defclass host-menu-canvas ()
  ()
  (:documentation "A class to define how title menus should appear."))


(defvar *system-default-menubar*
  NIL
  "The default menubar of the system.")

(defun set-system-default-menubar (&optional (menubar ccl::*default-menubar*))
  "Sets the *system-default-menubar* to be the value of the optional ~
   argument menubar (defaults to the Macintosh's *default-menubar*)."
  (declare (special ccl::*default-menubar*))
  (setf *system-default-menubar* menubar))

(eval-when (load)
  (set-system-default-menubar))

(defvar *last-menubar* *system-default-menubar*
  "The list of elements of the last menubar.  To be updated ~
   and downdated as canvases are activated and deactivated.")

(defun put-title-menus-on-menubar (canvas)
  (declare
   (special *current-canvas*
            *system-default-menubar*
            *last-menubar*))
  (setf *current-canvas* canvas)
  (let ((title-menus (title-menus-of canvas))
        title-menubar)
    (when
      title-menus
      (setf title-menus
            (sort (loop for m in title-menus collect m)
                  #'(lambda (k1 k2)
                      (string< (string k1) (string k2)))
                  :key #'car))
      (setf title-menubar
            (append *system-default-menubar*
                    (loop for m in title-menus collect (cdr m))))
      (setf *last-menubar* (menubar))
      (set-menubar title-menubar))
    )
  )



(defmethod initialize-instance :after ((self host-menu-canvas)
                                       &rest initargs)
  (declare (ignore initargs))
  (if (title-menus-of self)
    (put-title-menus-on-menubar self))
  )

(defmethod ccl::view-activate-event-handler  :before ((c host-menu-canvas))
  (if (title-menus-of c)
    (put-title-menus-on-menubar c))
  )

(defmethod ccl::view-deactivate-event-handler :before ((c host-menu-canvas))
  (declare (special *last-menubar*))
  (if (title-menus-of c)
    (set-menubar *last-menubar*))
  )

(defmethod window-close :after ((c host-menu-canvas))
  "Deactivates the menubar and re-enstalls the last one."
  (declare (special *last-menubar*))
  (if (title-menus-of c)
    (set-menubar *last-menubar*))
  )
