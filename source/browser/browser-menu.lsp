;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               browser-menu.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1988-1992 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     G. Desvignes 1988,1989
;;;     R.W. Oldford 1988-1992
;;;
;;;
;;;----------------------------------------------------------------------------------
;;;
;;;  
;;; it contains the functions peculiar to menus created by browsers
;;;

(in-package :quail)


;;; the following is intimately related to the design of menus in window-basics
;;; and to using a global variable for the *current-canvas* that is updated
;;; for canvases

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(browser-when-held-fn
          browser-title-when-selected-fn
          browser-body-when-selected-fn
          )))



(defun browser-when-held-fn (item menu key)
  "What to do when the menu item is held."
  (declare (ignore menu key))
  (when (listp item)
    (quail-print (third item))))

(defun browser-body-when-selected-fn (item menu mouse-button)
  "Special selection function that calls the function in the menu ~
   on the value of *current-canvas*."
  (wb::default-when-selected-fn item menu mouse-button))


(defun browser-title-when-selected-fn (item menu mouse-button)
  "Special selection function that calls the function in the menu ~
   on the value of *current-canvas*."
  (wb::default-when-selected-fn item menu mouse-button))

(defun return-item-when-selected-fn (item menu mouse-button)
  "Special selection function that returns the item if it's not a list ~
   or the second element in item if it is a list."
  (declare (ignore menu mouse-button))
  (if (and (typep item 'sequence)
           (>= (length item) 2))
    (second item)
    item))
