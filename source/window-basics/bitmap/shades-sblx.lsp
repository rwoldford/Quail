;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;              shades-sblx.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;  This file is part of the Window-Basics package intended to provide a uniform
;;;  window environment for Common Lisp programming.
;;;  
;;;  For copyright information and history, see window-basics-copyright.lisp
;;;  Authors:
;;;     H.A. Chipman 1991
;;;     C.B. Hurley 1989-1991
;;;     J.A. McDonald 1988-89
;;;     R.W. Oldford 1989-1992
;;;     J.O. Pedersen 1988-89
;;;     G.W. Bennett 1996
;;;     
;;;----------------------------------------------------------------------------------
(in-package :wb)
(eval-when (:compile-toplevel :load-toplevel :execute)
 (export  '(*black-shade* *white-shade* *light-gray-shade* *dark-gray-shade*
           *gray-shade* *default-fill-shade*)))
;;;============================================================
;;; Shades
;;;============================================================
(defvar *black-shade*)

(defvar *white-shade*)

(defvar *light-gray-shade*)

(defvar *dark-gray-shade*)

(defvar *gray-shade*)

(defvar *default-fill-shade*)

(defun setup-shades ()
  (declare (special *black-shade* *black-color*
                    *white-shade* *white-color*
                    *light-gray-shade* *light-gray-color*
                    *dark-gray-shade* *dark-gray-color*
                    *gray-shade*  *gray-color*
                    *default-fill-shade* *colors*))
  (setf *black-shade* *black-color*)
  (setf *white-shade* *white-color*)
  (setf *light-gray-shade* *light-gray-color*)
  (setf *dark-gray-shade* *dark-gray-color*)
  (setf *gray-shade* *gray-color*)
  #|(setf *gray-shade*  (make-record :color 
                                   :b0 170
                                   :b1 85
                                   :b2 170
                                   :b3 85
                                   :b4 170
                                   :b5 85
                                   :b6 170
                                   :b7 85))|#
  (setf *default-fill-shade* *gray-color*)
   )

(add-restore-lisp-functions #'setup-shades)

(eval-when (:load-toplevel) (setup-shades))
