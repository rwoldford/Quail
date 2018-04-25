;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;;                         re-enable-menus.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;
;;;  Authors:
;;;     R.W. Oldford 1996
;;;
;;;
;;;

(in-package :q-user)

(setf wb::*all-menus*
      (append wb::*all-menus*
              (list
               (q::quail-menu)
               (q::quail-plot-menu))))
