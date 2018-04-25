;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                        host-system-pc.lsp                          
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1991 Statistical Computing Laboratory, University of Waterloo
;;;  Authors:
;;;     R.W. Oldford 1991.
;;;--------------------------------------------------------------------------------
(in-package :wb)
 (eval-when (:compile-toplevel :load-toplevel :execute)
 (shadow '(MAKE-POSITION COPY-POSITION POSITION-X POSITION-Y
 MOVE-TO LINE-TO DRAW-LINE DRAW-ELLIPSE DRAW-POLYGON) ))
;; (use-package :common-graphics)
