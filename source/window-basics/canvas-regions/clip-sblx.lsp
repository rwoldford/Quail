;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                               clip-sblx.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1988-1992 Statistical Computing Laboratory, University of Waterloo
;;;  Authors:
;;;     G. Desvignes 1988,1989
;;;     R.W. Oldford 1988-1992
;;;     G.W. Benentt 1996
;;;----------------------------------------------------------------------------------
(in-package :window-basics)
(eval-when (:compile-toplevel :load-toplevel :execute) (export '(clipping-region-of)))

(defun clipping-region-of (canvas)
  "Returns the content region of the canvas which I take to ~
  means the interior of its window in (Q) coords."
  (let* ((its-pane (get-frame-pane canvas 'host-pane))
    (its-rectangle (pane-viewport-region its-pane))
    (clip-left (bounding-rectangle-min-x its-rectangle))
    (clip-bottom (host-to-screen-y (bounding-rectangle-max-y its-rectangle)))
    (clip-width (bounding-rectangle-width its-rectangle))
    (clip-height (bounding-rectangle-height its-rectangle)))
  (make-region clip-left clip-bottom clip-width clip-height)
  ))

#|          
(defun clipping-region-of (canvas)
  "Returns the content region of the canvas which I take to ~
   mean the interior of its window in (Q)screen coords."
  (let ((clip-left (cg::box-left 
                    ;(cg::window-interior canvas) 28oct05
                    (cg::interior canvas) ;28oct05
                                 ))
          (clip-bottom (host-to-screen-y (cg::box-bottom
                                          ;(cg::window-interior canvas) 28oct05
                                          (cg::interior canvas) ;28oct05
                                          )))
        (clip-width 
         ;(cg::window-interior-width canvas) 28oct05
         (cg::width (cg::interior canvas)) ;28oct05
                    )
        (clip-height 
         ;(cg::window-interior-height canvas) 28oct05
         (cg::height (cg::interior canvas)) ;28oct05
                     ))
      (make-region clip-left clip-bottom clip-width clip-height)
      )
)
|#