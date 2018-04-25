;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This file contains the functions and data structures which
;;; originally defined in the file having the same name without the
;;; "obs" (for obsolete) extension.  All code contained here has been
;;; superseded by other code.
;;;
;;; RWO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Functionality replaced by canvas-draw-rectangle

(defun draw-area-box (left bottom width height border window &optional texture)
  "Draw the border of the region defined by LEFT, BOTTOM, WIDTH, HEIGHT."
  
       (fill-region window left bottom border height texture)
                                               ; draw Left edge
       (fill-region window                     ; draw Top edge
              (+ left border)
              (- (+ bottom height)
                 border)
              (- width border border)
              border texture)
       (fill-region window                     ; draw Bottom edge
              (+ left border)
              bottom
              (- width border border)
              border texture)
       (fill-region window                     ; draw Right edge
              (- (+ left width)
                 border)
              bottom border height texture))


(defun draw-graph-node-border (border left bottom width height stream)
  "Interprets and draws the node border."

       (cond ((null border))
                                               ; 
             ((eq border t)
              (draw-area-box left bottom width height 1 stream))
             ((integerp border)
              (or (<= border 0)
                  (draw-area-box left bottom width height border stream)))
             ((listp border)
              (draw-area-box left bottom width height (first border)
                     stream
                     (second border)))
             (t (quail-error "Illegal border : ~S" border))))
