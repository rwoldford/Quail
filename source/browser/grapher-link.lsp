;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               grapher-link.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;
;;;
;;;  Authors:
;;;     G. Desvignes  1988-89
;;;     R.W. Oldford  1988-1992
;;;     J.R. MacPhail 1995
;;;
;;;
;;;----------------------------------------------------------------------------------

;;; This file, split off from grapher.lisp, defines operations on links.
;;; Links logically use nodes, and grapher depends on both.



(in-package :quail)


(defun display-link (from-nodes to-nodes  window g
                                &optional line-width operation)
  "Draws a link from FROM-NODES to TO-NODES."
  (wb:with-focused-canvas window
    (if (graph-side-flg g)
      (cond (
             ;; in a horizontal case of LATTICE, always draw from right
             ;; to left
             (or (graph-directed-flg g)
                 (> (gn-left to-nodes)
                    (gn-right from-nodes)))
             (display-link-rl from-nodes to-nodes line-width operation 
                              window))
            ((> (gn-left from-nodes)
                (gn-right to-nodes))
             (display-link-lr  from-nodes to-nodes line-width operation 
                               window))
            ((> (gn-bottom from-nodes)
                (gn-top to-nodes))
             (display-link-bt  from-nodes to-nodes line-width operation 
                               window))
            ((> (gn-bottom to-nodes)
                (gn-top from-nodes))
             (display-link-tb  from-nodes to-nodes line-width operation 
                               window))
            (t nil)
            ; if on top of each other don't
            ; draw
            )
      (cond ((or (graph-directed-flg g)
                 (> (gn-bottom from-nodes)
                    (gn-top to-nodes)))
             (display-link-bt  from-nodes to-nodes line-width operation 
                               window))
            ((> (gn-bottom to-nodes)
                (gn-top from-nodes))
             (display-link-tb  from-nodes to-nodes line-width operation 
                               window))
            ((> (gn-left to-nodes)
                (gn-right from-nodes))
             (display-link-rl  from-nodes to-nodes line-width operation 
                               window))
            ((> (gn-left from-nodes)
                (gn-right to-nodes))
             (display-link-lr  from-nodes to-nodes line-width operation 
                               window))
            (t nil)
            ; if on top of each other don't
            ; draw
            ))))

(defun erase-link (from-nodes to-nodes  window g
                              &optional line-width operation)
  "Erases a link from from-nodes to to-nodes."
  (wb:with-focused-canvas window
    (if (graph-side-flg g)
      (cond (
             ;; in a horizontal case of LATTICE, always draw from right
             ;; to left
             (or (graph-directed-flg g)
                 (> (gn-left to-nodes)
                    (gn-right from-nodes)))
             (erase-link-rl  from-nodes to-nodes line-width operation 
                             window))
            ((> (gn-left from-nodes)
                (gn-right to-nodes))
             (erase-link-lr  from-nodes to-nodes line-width operation 
                             window))
            ((> (gn-bottom from-nodes)
                (gn-top to-nodes))
             (erase-link-bt  from-nodes to-nodes line-width operation 
                             window))
            ((> (gn-bottom to-nodes)
                (gn-top from-nodes))
             (erase-link-tb  from-nodes to-nodes line-width operation 
                             window))
            (t nil)
            ; if on top of each other don't
            ; draw
            )
      (cond ((or (graph-directed-flg g)
                 (> (gn-bottom from-nodes)
                    (gn-top to-nodes)))
             (erase-link-bt  from-nodes to-nodes line-width operation 
                             window))
            ((> (gn-bottom to-nodes)
                (gn-top from-nodes))
             (erase-link-tb  from-nodes to-nodes line-width operation 
                             window))
            ((> (gn-left to-nodes)
                (gn-right from-nodes))
             (erase-link-rl  from-nodes to-nodes line-width operation 
                             window))
            ((> (gn-left from-nodes)
                (gn-right to-nodes))
             (erase-link-lr  from-nodes to-nodes line-width operation 
                             window))
            (t nil)
            ; if on top of each other don't
            ; draw
            ))))

(defun display-link-bt (gnb gnt width operation window)
  "Draws a line from the bottom edge of GNB to the top edge of GNT."
  
  (wb:canvas-draw-line
   window
   (wb:position-x (graph-node-position gnb))
   (- (gn-bottom gnb) 1)
   (wb:position-x (graph-node-position gnt))
   (+ (gn-top gnt) 1)
   :width width
   :operation operation))

(defun erase-link-bt (gnb gnt width operation window)
  "Erases a line from the bottom edge of GNB to the top edge of GNT translated ~
   by TRANS."
  
  (wb:canvas-erase-line
   window
   (wb:position-x (graph-node-position gnb))
   (- (gn-bottom gnb) 1)
   (wb:position-x (graph-node-position gnt))
   (+ (gn-top gnt) 1)
   :width width
   :operation operation))

(defun display-link-lr (gnl gnr width operation window)
  "Draws a line from the left edge of GNL to the right edge of GNR."
  
  (wb:canvas-draw-line
   window
   (- (gn-left gnl) 1)
   (wb:position-y (graph-node-position gnl))
   (+ (gn-right gnr) 1)
   (wb:position-y (graph-node-position gnr))
   :width width
   :operation operation))

(defun erase-link-lr (gnl gnr width operation window)
   window
  "Erase a line from the left edge of GNL to the right edge of GNR."
  (wb:canvas-erase-line
   window
   (- (gn-left gnl) 1)
   (wb:position-y (graph-node-position gnl))
   (+ (gn-right gnr) 1)
   (wb:position-y (graph-node-position gnr))
   :width width
   :operation operation))

(defun display-link-rl (gnr gnl width operation window)
   window
  "Draws a link from the right edge of GNR to the left edge of GNL."
  (wb:canvas-draw-line
   window
   (+ (gn-right gnr) 1)
   (wb:position-y (graph-node-position gnr))
   (- (gn-left gnl) 1)
   (wb:position-y (graph-node-position gnl))
   :width width
   :operation operation))

(defun erase-link-rl (gnr gnl width operation window)
   window
  "Erase a link from the right edge of GNR to the left edge of GNL."
  (wb:canvas-erase-line
   window
   (+ (gn-right gnr) 1)
   (wb:position-y (graph-node-position gnr))
   (- (gn-left gnl) 1)
   (wb:position-y (graph-node-position gnl))
   :width width
   :operation operation))

(defun display-link-tb (gnt gnb width operation window)
  "Draws a line from the top edge of GNT to the bottom edge of GNR."
  
  (wb:canvas-draw-line
   window
   (wb:position-x (graph-node-position gnt))
   (+ (gn-top gnt) 1)
   (wb:position-x (graph-node-position gnb))
   (- (gn-bottom gnb) 1)
   :width width
   :operation operation))

(defun erase-link-tb (gnt gnb width operation window)
  "Erase a line from the top edge of GNT to the bottom edge of GNR."
  
  (wb:canvas-erase-line
   window
   (wb:position-x (graph-node-position gnt))
   (+ (gn-top gnt) 1)
   (wb:position-x (graph-node-position gnb))
   (- (gn-bottom gnb) 1)
   :width width
   :operation operation))

(defun display-node-links (node window g
                                &optional to-s-only line-width operation)
  "Displays  node links. If to-s-only is not NIL , draws only the to links."
  
  (wb:with-focused-canvas window
    (prog ((node-lst (graph-node-lst g)))
      (dolist (to-node-id (to-links node))
        (display-link node (get-node-from-id to-node-id node-lst)
                      window g line-width operation))
      (or to-s-only (dolist (from-node-id (from-links node))
                      (display-link (get-node-from-id from-node-id 
                                                      node-lst)
                                    node window g line-width operation)))))
  )

(defun erase-node-links (node window g
                              &optional to-s-only line-width operation)
  "Erase node links. If to-s-only is not NIL, erases only the to links."
  
  (wb:with-focused-canvas window
    (prog ((node-lst (graph-node-lst g)))
      (dolist (to-node-id (to-links node))
        (erase-link node (get-node-from-id to-node-id node-lst)
                    window g line-width operation))
      (or to-s-only (dolist (from-node-id (from-links node))
                      (erase-link (get-node-from-id from-node-id 
                                                    node-lst)
                                  node window g line-width operation)))))
  )

