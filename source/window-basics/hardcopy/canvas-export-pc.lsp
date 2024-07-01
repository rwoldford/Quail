;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               canvas-export-pc.lsp
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :wb)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(canvas-export)))

(defmethod canvas-export ((self canvas) &key left top width height filetype )
   "Exports the canvas contents to a file of type filetype"
   (declare (special the-array the-tex) ;28JUL2023
            (ignore left top width height)) ; 07AUG2023
   (unless filetype
      (setf filetype (pick-one (list :postscript :bitmap))))
   (case 
     filetype
     (:postscript
      (canvas-to-ps self))
     (:bitmap
      (let ((image-loc
             (cg::ask-user-for-new-pathname
              "Choose location for BITMAP image file:"
              :initial-name "canvas.bmp"))
            )
         (cond (image-loc
                (multiple-value-setq (the-array the-tex)
                  (cg::get-pixels self (cg::visible-box self)))
                (apply #'cg::save-texture (list the-array the-tex image-loc))
                )
               (T
                (format *terminal-io* "~&~%No bitmap image saved.~%"))
               )))))
