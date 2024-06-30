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

;;; Highlight forms exist in cg:: but I need explanation from
;;; Franz about their functioning
;;; They turned out to be stubs to old stuff !!
;;; Thus highlight forms for drawing don't exist
#| original acl version
(defun canvas-highlight-rectangle (canvas left right bottom top
                                                        &key
                                                        color
                                                        (operation :boole-xor)
                                                        )
     "Highlights a rectangle of canvas using color"
     (declare (special cg::po-paint cg::po-replace cg::po-invert cg::po-erase))
     (if (eql operation :boole-xor)
        (progn     ; for highlight mode on color
           (with-focused-canvas canvas
            (let ((old-op (pen-operation-of canvas)))
                (cg::with-foreground-color (canvas color)
                 (canvas-draw-filled-rectangle canvas left right bottom top 
                  :color color   ))
;               (format "~%[h-d:c-h-rect:1] new-op to go to s-p-mode is ~s " new-op)
                (h-draw::set-pen-mode canvas (boole-to-op old-op)))))
        (cg::with-foreground-color (canvas color)
         (canvas-draw-filled-rectangle canvas left right bottom top 
          :color color :operation operation))))
|#          

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

#| original acl version
(defun canvas-highlight-arc (canvas
                                              start-angle arc-angle
                                              x-centre y-centre
                                              x-radius y-radius
                                              &key
                                              color
                                              (operation :boole-xor)
                                              )
     "Highlights an arc of canvas using color"
     (declare (special cg::po-invert cg::po-erase cg::po-paint cg::po-replace))
     (if (eql operation :boole-xor)
        (progn     ; for highlight mode on color
           (with-focused-canvas canvas
            (let ((old-op (pen-operation-of canvas)))
                (cg::with-foreground-color (canvas color)
                 (canvas-draw-filled-arc canvas
                  start-angle arc-angle
                  x-centre y-centre
                  x-radius y-radius
                  :color color   ))
;           (format "~%[h-d:c-h-arc:1] new-op to go to s-p-mode is ~s " new-op)
                (h-draw::set-pen-mode canvas (boole-to-op old-op)))))
        (cg::with-foreground-color (canvas color)   
         (canvas-draw-filled-arc canvas 
          start-angle arc-angle
          x-centre y-centre
          x-radius y-radius
          :color color :operation operation))
        ))
|#

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
;           (format "~%[h-d:c-h-arc:1] new-op to go to s-p-mode is ~s " new-op)
                (h-draw::set-pen-mode canvas (boole-to-op old-op)))))
                (with-drawing-options (pane-of-canvas :ink color)
         (canvas-draw-filled-arc canvas 
          start-angle arc-angle
          x-centre y-centre
          x-radius y-radius
          :color color :operation operation))
        )))
