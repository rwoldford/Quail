;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                          canvas-redisplay-sblx.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;  This file is part of the Window-Basics package intended to provide a uniform
;;;  window environment for Common Lisp programming.
;;;  
;;;  For copyright information and history, see window-basics-copyright.lisp
;;;  Authors:
;;;     G.W. Bennett 1996
;;;     R.W. Oldford 1989-1992
;;;     
;;;----------------------------------------------------------------------------------
(in-package :wb)
(eval-when (:compile-toplevel :load-toplevel :execute) (export  '()))

(defmethod resize-sheet ((self  canvas-redisplay-mixin) width height)
  (let ((result))
    (cond
      ((eql (frame-state self) :shrunk)
      (setf result (call-next-method))
      )
      (T (setf result (call-next-method))
        (unless (eq (frame-state self) :shrunk)
        (when (redisplay-p self)
          (redisplay self)))))
    result))
   ;(what-goes-here-? :force T))
#|
(defmethod cg::resize-window ((self canvas-redisplay-mixin) position)
     (let (result )
      (cond
       ((eq :icon (cg::state self))
        (setf result (call-next-method))
       )
       (T (setf result (call-next-method))
         (unless (eq :icon (cg::state self))
            (when (redisplay-p self)
              (redisplay self)
      ))))
   result))
|#