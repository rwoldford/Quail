;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                               clip-pc.lisp
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
