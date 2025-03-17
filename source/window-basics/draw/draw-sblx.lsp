;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                       draw-sblx.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;  This file is part of the Window-Basics package intended to provide a uniform
;;;  window environment for Common Lisp programming.
;;;  
;;;  For copyright information and history, see window-basics-copyright.lisp
;;;  Authors:
;;;     N.G. Bennett 1992
;;;     H.A. Chipman 1991
;;;     C.B. Hurley 1989-1992
;;;     M.E. Lewis 1991
;;;     J.A. McDonald 1988-89
;;;     R.W. Oldford 1989-1992
;;;     J.O. Pedersen 1988-89
;;;     G.W. Bennett 1996
;;;     
;;;----------------------------------------------------------------------------------

(in-package :wb)
(eval-when (:compile-toplevel :load-toplevel :execute) (export '(canvas-highlight-rectangle canvas-highlight-arc)))


(defun canvas-highlight-rectangle (canvas left top right bottom ;top
                                                        &key
                                                        color
                                                        (operation :boole-xor)
                                                        )
     "Highlights a rectangle of canvas using color"
     ;(let ((pane-of-canvas (get-frame-pane canvas 'host-pane)))
     (if (eql operation :boole-xor)
                 (canvas-draw-filled-rectangle canvas left top right bottom ;top 
                  :color color)
         (canvas-draw-filled-rectangle canvas left top right bottom ;top 
          :color color :operation operation)
              )
        ;)
     )

(defun canvas-highlight-arc (canvas
                                              start-angle arc-angle
                                              x-centre y-centre
                                              x-radius y-radius
                                              &key
                                              color
                                              (operation :boole-xor)
                                              )
     "Highlights an arc of canvas using color"
     (let ((pane-of-canvas (get-frame-pane canvas 'host-pane)))
     (if (eql operation :boole-xor)
        (progn     ; for highlight mode on color
           (with-focused-canvas canvas
            (let ((old-op (pen-operation-of canvas)))
                (with-drawing-options (pane-of-canvas :ink color)
                 (canvas-draw-filled-arc canvas
                  start-angle arc-angle
                  x-centre y-centre
                  x-radius y-radius
                  :color color   ))
                (h-draw::set-pen-mode canvas (boole-to-op old-op)))))
                (with-drawing-options (pane-of-canvas :ink color)
         (canvas-draw-filled-arc canvas 
          start-angle arc-angle
          x-centre y-centre
          x-radius y-radius
          :color color :operation operation))
        )))
