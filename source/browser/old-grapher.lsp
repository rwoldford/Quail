;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               grapher.lisp
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
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(graph-node-height
          graph-node-width
          graph-node-font
          graph-node-label
          graph-node-to-nodes
          graph-node-id
          graph-node-lst
          find-node
          flip-node
          get-node-from-id
          gn-bottom
          gn-left
          graph-region
          layout-graph
          make-graph-node
          reverse
          show-graph
          to-links
          move-node)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Structures definitions :
;;;

(defstruct graph-node
             id
             position
             label-bitmap
             width
             height
             to-nodes
             from-nodes
             label
             (label-shade *graph-default-node-label-shade*)
             (font *graph-default-node-font*)
             (border *graph-default-node-border*))

(defstruct graph
             node-lst 
             side-flg 
             directed-flg)


(defmacro half (x)
  "Divides by 2 ."
  `(and ,x (ash ,x -1)))

(defun find-node (canvas position)
  "Finds the closest node to position in canvas and returns it. ~
   The node is returned if position is inside the region it ~
   occupies in the canvas otherwise NIL is returned."
  
  (let* ((graph (wb::display-of canvas))
         (node-lst (if graph
                     (graph-node-lst graph))))
    (if node-lst
      (node-lst-as-menu node-lst position)
      NIL)))

(defun brh-layout (n x y mom-lst gn)
  "X and Y are the lower left corner of the box that will surround the tree ~
   headed by the browsenode N. MOM-LST isa the mother node inside a cons ~
   cell. GN is the graphnode for nodeid N. It is crucial that the ~
   node position be set before recursion because this marks that the node has ~
   been (is being) laid out already. BRH-OFFSET is used to raise the ~
   daughters in those rare cases where the label is bigger than the ~
   daughters."
 

       (declare (special mother-d personal-d the-node-lst))
       (prog ((ds (graph-node-to-nodes gn))
              (width (graph-node-width gn))
              (y-height (+ personal-d (graph-node-height gn)))
              d-height)
             (setf (graph-node-from-nodes gn)
                   mom-lst)
             (setf (graph-node-position gn)
                   (wb::make-position (+ x (half width)) nil))
             (cond ((null ds))
                   ((> y-height (setq d-height (brh-layout-daughters
                                                ds
                                                (+ x width mother-d)
                                                y
                                                (list n))))
                    (brh-offset ds (half (- y-height d-height))))
                   (t (setq y-height d-height)))
             (setf (wb::position-y (graph-node-position gn))
                   (+ y (half y-height)))
             (return y-height)))

(defun brh-layout-daughters (ds x y mom-lst)
  "DS are the daughters of (CAR MOM-LST). X is where the left edge of their ~
   label will be, and Y is the bottom of the mother's box. Returns the height ~
   of the mother's box. Tests to see if a node has been layout already. If ~
   so, side effecting the graph-node structure."

       (declare (special the-node-lst))
       (let ((the-floor y))
            (dolist (d ds)
                (setq the-floor (+ the-floor (brh-layout d x the-floor mom-lst
                                                    (get-node-from-id d 
                                                           the-node-lst)))))
            (- the-floor y)))

(defun brh-offset (node-ids y-inc)
       

;;; 
;;; 

       (declare (special the-node-lst))
       (dolist (n node-ids)
           (setq n (get-node-from-id n the-node-lst))
           (setf (wb::position-y (graph-node-position n))
                 (+ (wb::position-y (graph-node-position n))
                    y-inc))
           (brh-offset (graph-node-to-nodes n)
                  y-inc)))

(defun brhc-intertree-space (ttc btc)
  "Given the top transition chain of the old daughter and the bottom ~
   transition chain of the new daughter, where BTC is sitting on the bottom ~
   of the box, calculate how much the bottom must be raised so that it just ~
   clears the TTC. OP is the top left corner of some label. NP is the bottom ~
   left corner."

       (let ((raise -1000)
             (np (pop btc))
             (op (pop ttc))
             dist)
            (loop (setq dist (- (wb::position-y op)
                                (wb::position-y np)))
                  (and (> dist raise)
                       (setq raise dist))
                  (cond ((null btc)
                         (return raise))
                        ((null ttc)
                         (return raise))
                        ((eq (wb::position-x (first btc))
                             (wb::position-x (first ttc)))
                         (setq np (pop btc))
                         (setq op (pop ttc)))
                        ((< (wb::position-x (first btc))
                            (wb::position-x (first ttc)))
                         (setq np (pop btc)))
                        (t (setq op (pop ttc)))))))

(defun brhc-layout (n x mom-lst gn)
  "See documentation on BRH-LAYOUT. Instead of keeping only the graphnode in layed ~
   out node's position field, keep the offset as well. The offset is how much ~
   this nodes box must be raised relative to the enclosing box. Uses two free ~
   variables to return transition chains. RETURN-TTC is the top left corner ~
   of all the labels. RETURN-BTC is the bottom left corners."

       (declare (special personal-d return-ttc return-btc))
       (prog ((daughters (graph-node-to-nodes gn))
              (w (graph-node-width gn))
              (h (graph-node-height gn))
              y-center x-sw h/2)
             (setq h/2 (half h))
             (setq x-sw (+ x w))
             (setf (graph-node-from-nodes gn)
                   mom-lst)
             (setf (graph-node-position gn)
                   (list 0))
             (setq y-center (if daughters
                                (brhc-layout-daughters daughters x-sw
                                       (list n))
                                (brhc-layout-terminal gn x-sw)))
             (setf (graph-node-position gn)
                   (wb::make-position (+ x (half w)) y-center))
             (push (wb::make-position x (+ personal-d (- y-center h/2) h))
                   return-ttc)
             (push (wb::make-position x (- y-center h/2))
                   return-btc)
             (return y-center)))

(defun brhc-layout-daughters (ds x-sw mom-lst)
  "See documentation on BRH-LAYOUT-DAUGHTERS. First daughter is always laid out on ~
   the bottom of the box. Subsequent daughters have the amount that they are ~
   to be raised calculated by comparing the top edge of the old daughter (in ~
   TTC) with the bottom edge of the new daughter (in RETURN-BTC). TTC is ~
   update by adding the new daughter's transition chain to the front, because ~
   the new daughter's front is guaranteed to be higher than the old ~
   daughter's front. Conversely, BTC is updated by adding the new daughter's ~
   transition chain to the back, because the old daughter's front is ~
   guaranteed to be lower."

       (declare (special mother-d family-d the-node-lst return-ttc return-btc))
       (let (gn btc ttc first-d-y-center last-d-y-center (offset 0)
                (x (+ x-sw mother-d)))
            (dolist (d ds)
                (setq gn (get-node-from-id d the-node-lst))
                (setq last-d-y-center (brhc-layout d x mom-lst gn))
                (cond ((null ttc)
                                               ; first daughter
                       (setq first-d-y-center last-d-y-center)
                       (setq ttc return-ttc)
                       (setq btc return-btc))
                      (t (setq offset (brhc-intertree-space ttc return-btc))
                         (brhc-offset d offset)
                         (setq ttc (extend-transition-chain (
                                                         raise-transition-chain
                                                             return-ttc offset)
                                          ttc))
                         (setq btc (extend-transition-chain btc
                                          (raise-transition-chain return-btc 
                                                 offset))))))
            
            ;; 
            ;; finally, add a mythical top left corner at the height of the
            ;; highest daughter because diagonal links are getting clobbered.
            ;; Move lowest daughter's bottom left corner to the left for the
            ;; same reason.
            (setq return-ttc (cons (wb::make-position x-sw (wb::position-y (first ttc)))
                                   ttc))
            (setf (wb::position-x (first btc))
                  x-sw)
            (setf (wb::position-y (first ttc))
                  (+ (wb::position-y (first ttc))
                     family-d))
            (setq return-btc btc)
            
            ;; 
            ;; Center of mother is half way between first and last daughter's
            ;; label centers using fact that offset of first daughter is zero
            ;; and last daughter's offset is OFFSET
            (half (+ first-d-y-center last-d-y-center offset))))

(defun brhc-layout-terminal (gn x-sw)
  "Initializes the transition chains to the right edge of the node label, and ~
   returns the label's center."

       (declare (special return-ttc return-btc))
       (setq return-ttc (list (wb::make-position x-sw 0)))
       (setq return-btc (list (wb::make-position x-sw (graph-node-height gn))))
       (half (graph-node-height gn)))

(defun brhc-offset (n absy)
  "Adds  in all the offsets. see comment on BHRC-LAYOUT-DAUGHTERS."

       (declare (special the-node-lst))
       (prog ((gn (get-node-from-id n the-node-lst)))
             (setf (wb::position-y (graph-node-position gn))
                   (+ absy (wb::position-y (graph-node-position gn))))
             (dolist (d (graph-node-to-nodes gn))
                 (brhc-offset d absy))))

(defun brhl-layout (n x y mom-lst gn)
  "X and Y are the lower left corner of the box that will surround the tree ~
   headed by the browse node N. MOM-LST is the mother node inside a cons ~
   cell. GN is the graphnode for the nodeid N. It is crucial that the ~
   nodeposition be set before recursion because this marks that the node has ~
   been laid out already. If in addition, the YCOORD is NIL, the the node is ~
   still in the process of being laid out. BRHL-LAYOUT-DAUGHTERS uses this ~
   fact to break loops by inserting boxed nodes."

       (declare (special mother-d personal-d the-node-lst))
       (if (graph-node-position gn)
           
           ;; this case only occures if the node has been put in the roots
           ;; list, and has already been visited by recursion. Value won't be
           ;; used
           0
                                               ; 
           (prog ((ds (graph-node-to-nodes gn))
                  (width (graph-node-width gn))
                  (y-height (+ personal-d (graph-node-height gn))))
                 (setf (graph-node-from-nodes gn)
                       mom-lst)
                                               ; this is first time for
                                               ; layout, so set FROM-NODES
                 (setf (graph-node-position gn)
                       (wb::make-position (+ x (half width)) (list n)))
                 (and ds (setq y-height (max (brhl-layout-daughters
                                              ds
                                              (+ x width mother-d)
                                              y
                                              (list n))
                                             y-height)))
                 (setf (wb::position-y (graph-node-position gn))
                       (+ y (half y-height)))
                 (return y-height))))

(defun brhl-layout-daughters (ds x y mom-lst)
  "DS are the daughters of (CAR MOM-LST). X is where the left edge of their ~
   label will be, and y is the bottom of the mother's box. Returns the height ~
   of the mother's box. Tests to see if a node has been laid out already. if ~
   so, it  sees if the node is far enought to the right; if not it moves the ~
   node and its daughters."

       (declare (special the-node-lst y-height))
       (let (d gn np (the-floor y))
            (do ((d-tail ds (cdr d-tail)))
                ((null d-tail))
              (setq gn (get-node-from-id (setq d (first d-tail))
                              the-node-lst))
              (cond ((setq np (graph-node-position gn))
                     (cond ((null (wb::position-y np))
                            (setq gn (new-instance-of-graph-node gn))
                            (setf (first d-tail)
                                  (graph-node-id gn))
                            (setq the-floor (+ the-floor
                                               (brhl-layout (graph-node-id
                                                             gn)
                                                      x the-floor mom-lst gn)))
                            )
                           (t (brhl-move-right gn x nil)
                              (push (first mom-lst)
                                               ; Add its mother to the
                                               ; from-links
                                    (graph-node-from-nodes gn)))))
                    (t (setq the-floor (+ the-floor
                                          (brhl-layout d x the-floor mom-lst gn
                                                 ))))))
            (- the-floor y)))

(defun brhl-move-right (gn x stack)
  "Move this node and its children right."

       (declare (special mother-d the-node-lst))
       (prog ((width (graph-node-width gn))
              (np (graph-node-position gn)))
             (and (member gn stack)
                  (quail-error "Loop caught in BRHL-MOVE-RIGHT at ~S" (
                                                               graph-node-label
                                                                 gn)))
             (when (< x (- (wb::position-x np)
                           (half width)))
                   (return))
             (let ((new-x (+ x width mother-d))
                   (new-stack (cons gn stack)))
                  (dolist (d (to-links gn))
                      (brhl-move-right (get-node-from-id d the-node-lst)
                             new-x new-stack)))
             (setf (wb::position-x np)
                   (+ x (half width)))))

(defun browse-layout-horiz (root-ids)
  "Each subtree is given a box centered vertically on its label. Sister boxes ~
   a but do not intrude as they do in the compacting version."

       (declare (special the-node-lst))
       (let ((y 0))
            (dolist (n root-ids)
                (setq y (+ y (brh-layout n 0 y nil (get-node-from-id n 
                                                          the-node-lst)))))
            (make-graph :node-lst the-node-lst :side-flg t :directed-flg nil)))

(defun browse-layout-horiz-compactly (roots)
  "See documentation on BRH-LAYOUT and BRH-LAYOUT-DAUGHTERS first. This differs in ~
   that it keeps on the stack a representation of the shape of the tree that ~
   fills the node's box. The representation is a list of positions. If one ~
   starts  drawing a line from left to right starting at the CAR, each point ~
   is a step in the line, and the point begins a new plateau (or valley). The ~
   last point is where the line would turn around and head back to the left. ~
   Builds dummy top node for ROOTS if necessary, and adjusts the horizontal ~
   distance accordingly."
  
       (declare (special the-node-lst mother-d))
       (prog (return-ttc return-btc)
             (declare (special return-ttc return-btc))
             (cond ((not (listp roots))
                    (brhc-layout roots 0 nil (get-node-from-id roots 
                                                    the-node-lst))
                    (brhc-offset roots 0))
                   ((null (cdr roots))
                    (brhc-layout (first roots)
                           0 nil (get-node-from-id (first roots)
                                        the-node-lst))
                    (brhc-offset (first roots)
                           0))
                   (t (prog ((gn (make-graph-node :label (make-symbol "")
                                        :id
                                        (cons nil nil)
                                        :to-nodes roots :width 0 :height 0))
                             top-node)
                            (push gn the-node-lst)
                            (setq top-node (graph-node-id gn))
                            (brhc-layout top-node (- mother-d)
                                   nil gn)
                            (brhc-offset top-node 0)
                            (let (rn)
                                 (dolist (n roots)
                                     (setq rn (get-node-from-id n the-node-lst)
                                           )
                                     (setf (graph-node-from-nodes rn)
                                           (remove top-node (
                                                          graph-node-from-nodes
                                                             rn)))))
                            (setq the-node-lst (remove gn the-node-lst))))))
       (make-graph :node-lst the-node-lst :side-flg t :directed-flg nil))

(defun browse-layout-lattice (node-lst)
  "Almost the same as BROWSE-LAYOUT-HORIZ, except that ot doesn't box nodes ~
   unless there are cycles. Instead, a single node is placed at the rightmost ~
   of the positions that would be laid out by for all of its (boxed) ~
   occurences by BROWSE-LAYOUT-HORIZ."

       (declare (special the-node-lst))
       (let ((y 0))
            (dolist (n node-lst)
                (setq y (+ y (brhl-layout n 0 y nil (get-node-from-id n 
                                                           the-node-lst)))))
            (make-graph :node-lst the-node-lst :side-flg t :directed-flg nil)))

(defun display-link (from-nodes to-nodes  window g
                                &optional line-width operation)
  "Draws a link from FROM-NODES to TO-NODES."
  
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
                 )))

(defun erase-link (from-nodes to-nodes  window g
                                &optional line-width operation)
  "Erases a link from from-nodes to to-nodes."
  
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
                 )))

(defun display-link-bt (gnb gnt width operation window)
  "Draws a line from the bottom edge of GNB to the top edge of GNT."
  
  (wb::canvas-draw-line
   (wb::position-x (graph-node-position gnb))
   (- (gn-bottom gnb) 1)
   (wb::position-x (graph-node-position gnt))
   (+ (gn-top gnt) 1)
   window
   :width width
   :operation operation))

(defun erase-link-bt (gnb gnt width operation window)
  "Erases a line from the bottom edge of GNB to the top edge of GNT translated ~
   by TRANS."
  
  (wb::canvas-erase-line
   (wb::position-x (graph-node-position gnb))
   (- (gn-bottom gnb) 1)
   (wb::position-x (graph-node-position gnt))
   (+ (gn-top gnt) 1)
   window
   :width width
   :operation operation))

(defun display-link-lr (gnl gnr width operation window)
  "Draws a line from the left edge of GNL to the right edge of GNR."
  
  (wb::canvas-draw-line
   (- (gn-left gnl) 1)
   (wb::position-y (graph-node-position gnl))
   (+ (gn-right gnr) 1)
   (wb::position-y (graph-node-position gnr))
   window
   :width width
   :operation operation))

(defun erase-link-lr (gnl gnr width operation window)
  "Erase a line from the left edge of GNL to the right edge of GNR."
  (wb::canvas-erase-line
   (- (gn-left gnl) 1)
   (wb::position-y (graph-node-position gnl))
   (+ (gn-right gnr) 1)
   (wb::position-y (graph-node-position gnr))
   window
   :width width
   :operation operation))

(defun display-link-rl (gnr gnl width operation window)
  "Draws a link from the right edge of GNR to the left edge of GNL."
  (wb::canvas-draw-line
   (+ (gn-right gnr) 1)
   (wb::position-y (graph-node-position gnr))
   (- (gn-left gnl) 1)
   (wb::position-y (graph-node-position gnl))
   window
   :width width
   :operation operation))

(defun erase-link-rl (gnr gnl width operation window)
  "Erase a link from the right edge of GNR to the left edge of GNL."
  (wb::canvas-erase-line
   (+ (gn-right gnr) 1)
   (wb::position-y (graph-node-position gnr))
   (- (gn-left gnl) 1)
   (wb::position-y (graph-node-position gnl))
   window
   :width width
   :operation operation))

(defun display-link-tb (gnt gnb width operation window)
  "Draws a line from the top edge of GNT to the bottom edge of GNR."
  
  (wb::canvas-draw-line
   (wb::position-x (graph-node-position gnt))
   (+ (gn-top gnt) 1)
   (wb::position-x (graph-node-position gnb))
   (- (gn-bottom gnb) 1)
   window
   :width width
   :operation operation))

(defun erase-link-tb (gnt gnb width operation window)
  "Erase a line from the top edge of GNT to the bottom edge of GNR."
  
  (wb::canvas-erase-line
   (wb::position-x (graph-node-position gnt))
   (+ (gn-top gnt) 1)
   (wb::position-x (graph-node-position gnb))
   (- (gn-bottom gnb) 1)
   window
   :width width
   :operation operation))

(defun display-node-links (node window g
                                &optional to-s-only line-width operation)
  "Displays  node links. If to-s-only is not NIL , draws only the to links."
  
  (prog ((node-lst (graph-node-lst g)))
    (dolist (to-node-id (to-links node))
      (display-link node (get-node-from-id to-node-id node-lst)
                    window g line-width operation))
    (or to-s-only (dolist (from-node-id (from-links node))
                    (display-link (get-node-from-id from-node-id 
                                                    node-lst)
                                  node window g line-width operation)))))

(defun erase-node-links (node window g
                              &optional to-s-only line-width operation)
  "Erase node links. If to-s-only is not NIL, erases only the to links."
  
  (prog ((node-lst (graph-node-lst g)))
    (dolist (to-node-id (to-links node))
      (erase-link node (get-node-from-id to-node-id node-lst)
                  window g line-width operation))
    (or to-s-only (dolist (from-node-id (from-links node))
                    (erase-link (get-node-from-id from-node-id 
                                                  node-lst)
                                node window g line-width operation)))))


(defun display-graph (graph window clipping-region &optional line-width)
  "Displays graph on WINDOW. ~
   Draws links first, then labels so that lattices don't have lines through ~
   the labels."
  (dolist (n (graph-node-lst graph))
    (display-node-links n window graph t line-width))
  (dolist (n (graph-node-lst graph))
    (print-display-node n window clipping-region))
  (let ((nodes (selected-nodes-of window)))
    (if nodes
      (dolist (node nodes)
        (flip-node node window)))))


(defun draw-graph-node-border (border left bottom width height stream
                                      &key color
                                      operation
                                      dashing)
  "Interprets and draws the node border."

  (let ((x1 left) (y1 bottom)
        (x2 (+ left width)) (y2 (+ bottom height))
        (border-width
          (cond
            ((eq border t) 1)
            ((and (integerp border) (> border 0)) border)
            ((listp border)
             (setf dashing (second border))
             (first border))
            (t (quail-error "Illegal border : ~S" border)))))
    
    (when border
      (wb::canvas-draw-rectangle x1 x2 y1 y2 stream
                             :width border-width
                             :color color
                             :dashing dashing
                             :operation operation))))

(defmacro eq-or-member (item atom-or-list)
  "Returns T if item EQ Atom-or-list or if item is a member of Atom-or-list."

       `(or (eq ,item ,atom-or-list)
            (and (listp ,atom-or-list)
                 (member ,item ,atom-or-list :test #'eq))))

(defun extend-transition-chain (ltc rtc)
  "Extends the left transition chain by appending the part of the right ~
   transition chain that is to the right of the end of the left transition ~
   chain. End point of left transition chain is changed to intersect right ~
   transition chain."

       (let ((l-tail ltc)
             (r-tail rtc)
             lx rx)
            (loop (cond ((null (cdr r-tail))
                         (setf (wb::position-y (first (last l-tail)))
                               (wb::position-y (first r-tail)))
                         (return ltc))
                        ((null (cdr l-tail))
                         (rplacd l-tail (cdr r-tail))
                         (setf (wb::position-y (first l-tail))
                               (wb::position-y (first r-tail)))
                         (return ltc))
                        ((eq (setq lx (wb::position-x (second l-tail)))
                             (setq rx (wb::position-x (second r-tail))))
                         (setq l-tail (cdr l-tail))
                         (setq r-tail (cdr r-tail)))
                        ((< lx rx)
                         (setq l-tail (cdr l-tail)))
                        (t (setq r-tail (cdr r-tail)))))))

(defun fill-graph-node-label
       (shade left bottom width height border window)
  (declare (ignore shade))
  ;; 
  ;;  border must be subtracted from the node's region
  ;;
  (wb::canvas-invert window                                ;<----- Better as a draw-filled?
                 :canvas-left (+ left border)          ;       ... rwo
                 :canvas-bottom (+ bottom border)
                 :width (- width border border)
                 :height (- height border border)))

(defun flip-node (node window)
  "Flip the region around the node."
  
  (wb::canvas-invert window
                 :canvas-left
                 (+ (gn-left node) -1)
                 :canvas-bottom
                 (+ (gn-bottom node) -1)
                 :width (+ (graph-node-width node) 2)
                 :height (+ (graph-node-height node) 2)))

(defun from-links (node) (graph-node-from-nodes node))

(defun forest-break-cycles (node)
  "Breaks any cycle by inserting new nodes and boxing."

       (declare (special the-node-lst))
       (setf (graph-node-position node)
             t)
       (let (dn)
            (do ((d-tail (graph-node-to-nodes node)
                        (cdr d-tail)))
                ((null d-tail))
                                               ; 
              (setq dn (get-node-from-id (first d-tail)
                              the-node-lst))
              (cond ((graph-node-position dn)
                                               ; we've seen this before
                     (setq dn (new-instance-of-graph-node dn))
                     (setf (first d-tail)
                           (graph-node-id dn)))
                    (t (forest-break-cycles dn))))))

(defun get-node-from-id (id node-lst &optional no-err-flg)
  "Returns node associated to ID."

       (or (dolist (node node-lst nil)
               (when (eq id (graph-node-id node))
                     (return node)))
           (and id (listp id)
                                               ; the nodes created when we
                                               ; break cycles have the same
                                               ; name than the original node
                                               ; in a list, so both node name
                                               ; should return the same graph
                                               ; node
                (get-node-from-id (first id)
                       node-lst))
           (if no-err-flg
               nil
               (quail-error "No graphnode for ID : ~S" id))))

(defun gn-bottom (node &aux (pos (graph-node-position node)))
  "Determines the bottom position of a Graph-Node."
   
     (if pos
       (- (or (wb::position-y pos) 0)
          (half (or (graph-node-height node) 0)))
       0))

(defun gn-left (node &aux (pos (graph-node-position node)))
  "Determines the left position of a Graph-Node."
 
       (if pos
           (- (or (wb::position-x pos) 0)
              (half (or (graph-node-width node) 0)))
           0))

(defun gn-right (node &aux (pos (graph-node-position node)))
  "Determines the right position of a Graph-Node."

       (if pos
           (+ (or (wb::position-x pos) 0)
              (- (half (+ 1 (or (graph-node-width node) 0)))
                 1))
           0))

(defun gn-top (node &aux (pos (graph-node-position node)))
  "Determines the top position of a Graph-Node."

       (if pos
          (+ (or (wb::position-y pos) 0)
             (- (half (+ 1 (or (graph-node-height node) 0)))
                1))
          0))

(defun graph-node-border-width (border)
  "Returns a non-negative integer giving the border width."

       (cond ((null border)
              0)
             ((eq border t)
              1)
             ((integerp border)
              (abs border))
             ((and (listp border)
                   (integerp (first border))
                   (>= (first border)
                       0))
              (first border))
             (t (quail-error "Illegal border : ~S" border))))


(defun graph-region (graph)
  "Return the minimum region containing the graph."

       (if graph
           (prog (left-offset bottom-offset (node-lst (graph-node-lst graph)))
                 (return (cond (node-lst
                                  (dolist (n node-lst)
                                          (measure-graph-node n))
                                  (list (setq left-offset (min-left node-lst))
                                        (setq bottom-offset (min-bottom 
                                                                   node-lst))
                                        (+ 1 (- (max-right node-lst)
                                                left-offset))
                                        (+ 1 (- (max-top node-lst)
                                                bottom-offset))))
                                (t (list 0 0 0 0)))))
           (list 0 0 0 0)))



(defun init-nodes-for-layout (ns format root-ids font)
       

;;; 

       (declare (special the-node-lst))
       (dolist (gn ns)
           (setf (graph-node-position gn)
                 
                 ;; T indicate prior visitation. Roots are already visited
                 (not (not (member (graph-node-id gn)
                                  root-ids :test #'eq))))
           (or (graph-node-font gn)
               (setf (graph-node-font gn)
                     font)))
       (dolist (r root-ids)
           (if (eq-or-member 'lattice format)
               (lattice-break-cycles (get-node-from-id r the-node-lst)
                      nil)
               (forest-break-cycles (get-node-from-id r the-node-lst))))
       (dolist (gn the-node-lst)
           (setf (graph-node-position gn)
                 nil)
           (set-label-size gn)))

(defun interpret-mark-format (format)
  "Sets special variables for new-instance-of-graph-node and mark-graph-node."

       (declare (special box-both-flg box-leaves-flg border-for-marking 
                       label-shade-for-marking))
       (prog (pl)
             (and (eq-or-member 'copies-only format)
                  (setq box-both-flg nil))
             (and (eq-or-member 'not-leaves format)
                  (setq box-leaves-flg nil))
             (cond ((not (listp format))
                    (return))
                   ((eq (car format)
                        'mark)
                    (setq pl (cdr format)))
                   (t (return)))
             (if (member 'border pl :test #'eq)
                 (setq border-for-marking (getf pl 'border))
                 (setq border-for-marking 'dont))
             (if (member 'label-shade pl :test #'eq)
                 (setq label-shade-for-marking (getf pl 'label-shade))
                 (setq label-shade-for-marking 'dont))))

(defun intersect-regionp-lbwh (left bottom width height reg)
  "Like intersect regions, but without requiring the consing."

       (not (or (> (wb::region-bottom reg)
                   (+ bottom height))
                (< (+ (wb::region-left reg)
                      (wb::region-width reg))
                   left)
                (> (wb::region-left reg)
                   (+ left width))
                (< (+ (wb::region-bottom reg)
                      (wb::region-height reg))
                   bottom))))

(defun lattice-break-cycles (node stack)
       

;;; 
;;; 

       (declare (special the-node-lst))
       (setf (graph-node-position node)
             t)
       (let (d gn)
            (do ((d-tail (graph-node-to-nodes node)
                        (cdr d-tail)))
                ((null d-tail))
                                               ; 
              (setq gn (get-node-from-id (setq d (first d-tail))
                              the-node-lst))
              (cond ((member d stack)
                     (setq gn (new-instance-of-graph-node gn))
                     (setf (first d-tail)
                           (graph-node-id gn)))
                    ((null (graph-node-position gn))
                     (lattice-break-cycles gn (cons d stack)))))))

(defun layout (graph)
       

;;; 
;;; Layout in lattice for tests
;;; 

       (let (roots)
            (dolist (node (graph-node-lst graph))
                (unless (dolist (link (graph-node-from-nodes node))
                            (when (get-node-from-id link (graph-node-lst graph)
                                         )
                                  (return t)))
                    (push (graph-node-id node)
                          roots)))
            (show-graph
             (layout-graph (graph-node-lst graph) roots 'lattice nil nil nil nil))))

(defun layout-graph (the-node-lst root-ids format font &optional mother-d 
                                  personal-d family-d)
  "Takes a list of Graph-node records and a list node ids for the top level ~
   nodes, where the graph-nodes have only the NODE-ID, NODE-LABEL and ~
   TO-NODES fields filled in. It fills in the other fields approprietly ~
   according the format switch and the boxing switch so that the graph ~
   becomes a forest. If there are loops in the graph they are snapped and the ~
   NODE-LST is extended with push this function returns a graph record with ~
   the display slots filled in appropriately."
  
  (declare (special the-node-lst mother-d personal-d family-d))
  (prog ((box-both-flg t)
         (box-leaves-flg t)
         (border-for-marking t)
         (label-shade-for-marking 'dont)
         g)
    (declare (special box-both-flg box-leaves-flg border-for-marking 
                      label-shade-for-marking))
    (or (and root-ids (listp root-ids))
        (quail-error "LAYOUT-GRAPH needs a LIST of root node ids"))
    (dolist (r root-ids)
      ; the nodes of ROOT-IDS must be
      ; in THE-NODE-LST
      (unless (dolist (node the-node-lst nil)
                (when (eq r (graph-node-id node))
                  (return t)))
        (quail-error 
         "~S is in ROOT-IDS but no GRAPH-NODE for it in THE-NODE-LST"
         r)))
    (setq font (if (wb::canvas-font-p font)
                 font
                 *graph-default-font*))
    (or mother-d (setq mother-d (wb::string-width "AAAAAA" font)))
    (or personal-d (setq personal-d (if (eq-or-member 'vertical format
                                                      )
                                      (wb::string-width "AA" font)
                                      0)))
    (or family-d (setq family-d (half (wb::canvas-font-ascent font))))
    (interpret-mark-format format)
    (init-nodes-for-layout the-node-lst format root-ids font)
    (and (eq-or-member 'vertical format)
         (switch-node-height-width the-node-lst))
    (setq g (cond ((eq-or-member 'lattice format)
                   (browse-layout-lattice root-ids))
                  ((eq-or-member 'fast format)
                   (browse-layout-horiz root-ids))
                  (t (browse-layout-horiz-compactly root-ids))))
    (dolist (n the-node-lst)
      (or (wb::position-p  (graph-node-position n))
          (quail-error 
           "Disconnected graph. Root(s) didn't connect to : ~S"
           (graph-node-label n))))
    (offset-to-origin the-node-lst)
    (cond ((eq-or-member 'vertical format)
           (switch-node-height-width the-node-lst)
           (reflect-graph-diagonally g)
           (and (eq-or-member 'reverse format)
                (reflect-graph-vertically g))
           (and (eq-or-member 'reverse-daughters format)
                (reflect-graph-horizontally g)))
          (t (and (eq-or-member 'reverse format)
                  (reflect-graph-horizontally g))
             (and (eq-or-member 'reverse-daughters format)
                  (reflect-graph-vertically g))))
    (return g)))

(defun layout-s-expr (tree &optional format boxing font mother-d personal-d 
                           family-d)
  "Assumes CAR of Tree is Node label, CDR is daughter trees."

       (if tree
           (prog (result)
                 (declare (special result))
                 (layout-s-expr-1 tree)
                 
                 ;; Result contains a list of node corresponding to Tree which
                 ;; label and to-nodes link are updated 
                 (return (layout-graph result (list tree)
                                (append (if (listp format)
                                            format
                                            (list format))
                                       boxing)
                                font mother-d personal-d family-d)))
           (quail-error "Cannot layout NIL as S-EXPRESSION")))

(defun layout-s-expr-1 (tree)
       

;;; 

       (declare (special result))
       (cond ((dolist (r result nil)
                                               ; If Tree is already in result
                                               ; nothing is done
                  (when (eq tree (graph-node-id r))
                        (return t))))
             ((not (listp tree))
                                               ; if Tree is a simple node it
                                               ; is added in result
              (push (make-graph-node :id tree :label tree)
                    result))
                                               ; otherwise Tree is a node with
                                               ; subnodes which are
                                               ; recursively added to result
             (t (push (make-graph-node :id tree :label (first tree)
                             :to-nodes
                             (append (cdr tree)))
                      result)
                (dolist (d (cdr tree))
                    (layout-s-expr-1 d)))))

(defun mark-graph-node (node)
  "Changes appearance of graph node to indicate that a link has been snapped."

       (declare (special border-for-marking label-shade-for-marking))
       (or (eq border-for-marking 'dont)
           (setf (graph-node-border node)
                 border-for-marking))
       (or (eq label-shade-for-marking 'dont)
           (setf (graph-node-label-shade node)
                 label-shade-for-marking)))

(defun max-right (node-lst)
  "Returns the largest right position of nodes in Node-lst."

       (let ((largest (gn-right (car node-lst))))
            (dolist (node node-lst)
                (when (> (gn-right node)
                         largest)
                    (setq largest (gn-right node))))
            largest))

(defun max-top (node-lst)
  "Returns the largest Top position of nodes in Node-Lst."

       (let ((largest (gn-top (car node-lst))))
            (dolist (node node-lst)
                (when (> (gn-top node)
                         largest)
                    (setq largest (gn-top node))))
            largest))

(defun measure-graph-node (node &optional (reset-flg nil))
  "Measure the node label image."

       (set-label-size node reset-flg)
       (set-layout-position node (or (graph-node-position node)
                                     (quail-error 
                           "This Graph node has not been given a position : ~S"
                                            node))))

(defun memb-to-nodes (to-node to-nodes)
       (dolist (z to-nodes)
           (when (eq z to-node)
                 (return z))))

(defun min-bottom (node-lst)
  "Returns the smallest bottom position of nodes in Node-Lst."

       (let ((smallest (gn-bottom (car node-lst))))
            (dolist (node node-lst)
                (when (< (gn-bottom node)
                         smallest)
                    (setq smallest (gn-bottom node))))
            smallest))

(defun min-left (node-lst)
  "Returns the smallest left position of nodes in Node-lst."

       (let ((smallest (gn-left (car node-lst))))
            (dolist (node node-lst)
                (when (< (gn-left node)
                         smallest)
                    (setq smallest (gn-left node))))
            smallest))

(defun new-instance-of-graph-node (node)
  "Returns a second instance of the graph-node, boxing it."
  (declare (special the-node-lst box-leaves-flg box-both-flg))
  (prog ((new (make-graph-node
               :id (list (graph-node-id node))
               :label (graph-node-label node)
               :font (graph-node-font node)
               :width (graph-node-width node)
               :height (graph-node-height node)
               :border (graph-node-border node)
               :label-shade  (graph-node-label-shade node))))
    (push new the-node-lst)
    (when (or box-leaves-flg (graph-node-to-nodes node))
      ; in the original version, the node are boxed when they are
      ; duplicated. we remove this so that every node seems identical
      ; on the map
      ;(mark-graph-node new)
      ;(when box-both-flg (mark-graph-node node))
      )
    (return new)))

(defun node-lst-as-menu (node-lst position)
  "Find the node that is closest to position and returns it ~
   the node N of Node-lst is returned if position is inside the region it ~
   occupies in the window otherwise NIL is returned."
  
  (let ((x (wb::position-x position))
        (y (wb::position-y position))
        t1 t2)
    (dolist (n node-lst nil)
      (when (and (< (- (setq t1 (wb::position-y (graph-node-position  n)))
                       (setq t2 (half (graph-node-height n))))
                    y)
                 (< y (+ t1 t2))
                 (< (- (setq t1 (wb::position-x (graph-node-position
                                             n)))
                       (setq t2 (half (graph-node-width n))))
                    x)
                 (< x (+ t1 t2)))
        (return n)))))

(defun offset-to-origin (node-lst)
  "After layout, the computed positions can be negative, which can be a problem ~
   on some environments. To avoid this, we systematically offset the graph so ~
   that each position is positive."

  (let ((inf-x (min-left node-lst))
        (inf-y (min-bottom node-lst))
        pos)
     (unless (and (eq 0 inf-x)
                  (eq 0 inf-y))
         (dolist (node node-lst)
            (setq pos (graph-node-position node))
            (setf (wb::position-x pos) (- (wb::position-x pos) inf-x))
            (setf (wb::position-y pos) (- (wb::position-y pos) inf-y))))))


(defun print-display-node (node window &optional clip-reg
                                operation)
  "Prints a node at its position.  Takes the operation from ~
   the stream so that when editor has set the operation to :invert, this may ~
   erase as well as draw; but when the operation is :paint, then nodes obliterate ~
   any link lines that they are drawn over."
  (declare (special *graph-cache-node-labels*))
  (setf operation
        (case operation
          (:invert boole-xor)
          (:paint :default)
          (T  NIL)))
  (unless (eq 0 (graph-node-height node))
    (prog ((left (gn-left node))
           (bottom (gn-bottom node))
           (width (graph-node-width node))
           (height (graph-node-height node))
           (font (graph-node-font node))
           (nbw (graph-node-border-width (graph-node-border node))))
      (when (and clip-reg
                 (not (intersect-regionp-lbwh
                       left bottom width height clip-reg)))
        (return node))
      (cond ((wb::bitmap-p (graph-node-label-bitmap node))
             (wb::canvas-bitblt (graph-node-label-bitmap node) window
                            :canvas-left left
                            :canvas-bottom bottom
                            :width width :height height
                            :operation operation))
            ((wb::bitmap-p (graph-node-label node))
             (cond ((eq 0 nbw)
                    (wb::canvas-bitblt (graph-node-label node) window
                                   :canvas-left left
                                   :canvas-bottom bottom
                                   :width width :height height
                                   :operation operation))
                   (t
                    (draw-graph-node-border (graph-node-border node)
                                            left bottom width height window)
                    (wb::canvas-bitblt (graph-node-label node) window
                                   :canvas-left (+ left nbw)
                                   :canvas-bottom (+ bottom nbw)
                                   :width width :height height
                                   :operation operation))))
            (t (or (wb::canvas-font-p font)
                   (setf (graph-node-font node)
                         (setq font (wb::canvas-font window))))
               (if (not (eq nbw 0))
                 (draw-graph-node-border
                  (graph-node-border node)
                  left bottom width height window))
               (wb::canvas-draw-center-string
                (graph-node-label node)
                left bottom width height window
                :font font)
               (if (graph-node-label-shade node)
                 (fill-graph-node-label
                  (graph-node-label-shade node)
                  left bottom width height nbw window))
               (when (and *graph-cache-node-labels*
                          clip-reg
                          (intersect-regionp-lbwh left bottom width height
                                                  clip-reg))
                 (setf (graph-node-label-bitmap node)
                       (wb::make-bitmap :width width :height height))
                 (wb::copy-canvas-region-to-bitmap window left 
                                               bottom (graph-node-label-bitmap node)
                                               width height))
               ))
      (return node))))

(defun raise-transition-chain (tc raise)
  "Raises a daughter's transition chain by adding in the offset of the ~
   daughter's box relative to the mother's box."

       (dolist (p tc tc)
           (setf (wb::position-y p)
                 (+ (wb::position-y p)
                    raise))))

(defun redisplay-graph (window  &rest rest)
  "Displays the graph that is in a window."
  
  (declare (ignore rest))
  (wb::canvas-clear window)
  (when (wb::display-of window)
    (display-graph (wb::display-of window)
                   window
                   (wb::clipping-region-of window)
                   )))

(defun reflect-graph-diagonally (graph)
  (setf (graph-side-flg graph) (not (graph-side-flg graph)))
  (let (yn)
    (dolist (n (graph-node-lst graph))
      (setq n (graph-node-position n))
      (setq yn (wb::position-y n))
      (setf (wb::position-y n) (wb::position-x n))
      (setf (wb::position-x n) yn))))

(defun reflect-graph-horizontally (graph)

       (let ((w (+ (max-right (graph-node-lst graph))
                   (min-left (graph-node-lst graph)))))
            (dolist (n (graph-node-lst graph))
                (setq n (graph-node-position n))
                (setf (wb::position-x n)
                      (- w (wb::position-x n))))))

(defun reflect-graph-vertically (graph)

       (let ((h (+ (max-top (graph-node-lst graph))
                   (min-bottom (graph-node-lst graph)))))
            (dolist (n (graph-node-lst graph))
                (setq n (graph-node-position n))
                (setf (wb::position-y n)
                      (- h (wb::position-y n))))))

(defun set-label-size (node &optional reset-flg)
  
  
  ;;; 
  ;;; the SHADE and null font stuff is for ZOOM-GRAPH
  ;;; 
  
  (or (and (not reset-flg)
           (integerp (graph-node-height node))
           (integerp (graph-node-width node)))
      (prog ((font (graph-node-font node))
             (lab (graph-node-label node))
             (nbw (graph-node-border-width (graph-node-border node)))
             width height)
        (cond ((wb::bitmap-p lab)
               (setq width (wb::bitmap-width lab))
               (setq height (wb::bitmap-height lab)))
              (t (or (stringp lab)
                     (if (symbolp lab)
                       (setq lab (symbol-name lab))
                       (setq lab (format nil "~s" lab))) )
                 (or (wb::canvas-font-p font)
                     (setf (graph-node-font node)
                           (setq font *graph-default-font*)))
                 (setq width (+ (wb::string-width lab font)
                                (wb::canvas-font-descent font)))
                 (setq height (+ (wb::canvas-font-height font)
                                 (wb::canvas-font-descent font)))))
        (or (and (not reset-flg)
                 (integerp (graph-node-width node)))
            (setf (graph-node-width node)
                  (+ width nbw nbw)))
        (or (and (not reset-flg)
                 (integerp (graph-node-height node)))
            (setf (graph-node-height node)
                  (+ height nbw nbw)))
        (return node))))

(defun set-layout-position (node position)
  "Sets a node position."
  
  (setf (wb::position-x (graph-node-position node))
        (wb::position-x position))
  (setf (wb::position-y (graph-node-position node))
        (wb::position-y position)))

(defun size-graph-window (graph window-or-title &optional top-justify-flg)
  "Returns a window sized to fit the given graph. WINDOW-OR-TITLE can be ~
   either a window to be printed in or a title of a window to be created.If ~
   TOP-JUSTIFY-FLG is T, scrolls so top of graph is at top of window, else ~
   puts bottom of graph at bottom of window."
  
  (declare (special *min-width-graph* *max-width-graph* *min-height-graph*
                    *max-height-graph*))
  (prog ((graph-reg (graph-region graph))
         cursor-pos title window)
    (if (wb::canvas-p window-or-title)
      (setq window window-or-title)
      (setq title window-or-title))
    (or title 
        (setq title "Graph window"))
    
    ;; 
    ;; If there is not already a window, ask the user for one to fit.
    ;; 
    (cond ((null window)
           ;;(setq cursor-pos (screen-mouse-position))
           (setq window
                 (make-browser
                  :region
                  (list (wb::position-x cursor-pos)
                        (wb::position-y cursor-pos)
                        (wb::compute-window-width
                         (min (max (third graph-reg)
                                   *min-width-graph*)
                              *max-width-graph*))
                        (wb::compute-window-height
                         (min (max (fourth graph-reg)
                                   *min-height-graph*)
                              *max-height-graph*)
                         title))
                  
                  :title title)))
          (t (wb::canvas-to-top window)))
    (wb:set-extent-canvas-region window graph-reg)
    (wb:set-canvas-x-offset window (- (wb::canvas-x-offset window)
                                      (first graph-reg)))
    (wb:set-canvas-y-offset window
                            (- (wb::canvas-y-offset window)
                               (if top-justify-flg
                                 (- (+ (second graph-reg)
                                       (fourth graph-reg))
                                    (wb::canvas-height window))
                                 (second graph-reg))))
    (return window)))

(defun show-graph (&optional graph window top-justify-flg)
  "Puts the graph in a window and create one if it is not given."

       (setq window (size-graph-window
                           (or graph (setq graph (make-graph)))
                           window top-justify-flg))
       (setf (wb::display-of window) graph)
       (redisplay-graph window)
       window)

(defun switch-node-height-width (node-lst)
  
       (let (wn)
            (dolist (n node-lst)
                (setq wn (graph-node-width n))
                (setf (graph-node-width n)
                      (graph-node-height n))
                (setf (graph-node-height n)
                      wn))))

(defun to-links (node) (graph-node-to-nodes node))


(defun move-node-to (node to-pos window
                          &optional
                          clipping-region)

  (let* ((graph (wb::display-of window)))

    (erase-node-links node window graph)
    (erase-node-label node window)

    (setf (graph-node-position node) to-pos)

    (display-node-links node window graph)
    (print-display-node node window clipping-region)

    (flip-node node window)
    ))

(defun relative-move-node-to (node dx dy window
                          &optional
                          clipping-region)
  (let ((current-pos (graph-node-position node)))
    (move-node-to node (wb::make-position (+ (wb::position-x current-pos)
                                         dx)
                                      (+ (wb::position-y current-pos)
                                         dy))
                  window
                  clipping-region)))


(defun move-nodes (node-list current-position canvas
                             &optional
                             clipping-region)
  "Dynamically moves all nodes of node-list in the graph of the window canvas."
  (let ((old-pos current-position)
        (new-pos current-position)
        dx dy)
    (loop while (wb::mouse-down-p) do
          (setf new-pos (wb::mouse-position canvas))
          (unless (and (= (wb::position-x new-pos) (wb::position-x old-pos))
                       (= (wb::position-y new-pos) (wb::position-y old-pos)))
            (loop
              for node in node-list
              do
              (setf dx (- (wb::position-x new-pos) (wb::position-x old-pos)))
              (setf dy (- (wb::position-y new-pos) (wb::position-y old-pos)))
              (relative-move-node-to node dx dy
                                     canvas clipping-region))
            
            (setf old-pos new-pos)))
    (unless
      (and (= (wb::position-x new-pos) (wb::position-x current-position))
           (= (wb::position-y new-pos) (wb::position-y current-position)))
      (if clipping-region
        (wb::canvas-clear canvas
                      :canvas-left   (wb::region-left clipping-region)
                      :canvas-bottom (wb::region-bottom clipping-region)
                      :width         (wb::region-width clipping-region)
                      :height        (wb::region-height clipping-region))
        (wb::canvas-clear canvas))
      (display-graph (wb::display-of canvas) canvas clipping-region))))


(defun move-node (node current-position canvas
                       &optional
                       clipping-region)
  "Dynamically moves the node in the graph of the canvas."
  (let ((old-pos current-position)
        (new-pos current-position))
    (loop while (wb::mouse-down-p) do
          (setf new-pos (wb::mouse-position canvas))
          (unless (and (= (wb::position-x new-pos) (wb::position-x old-pos))
                       (= (wb::position-y new-pos) (wb::position-y old-pos)))
            (move-node-to node new-pos canvas clipping-region)
            (setf old-pos new-pos)))
    (unless
      (and (= (wb::position-x new-pos) (wb::position-x current-position))
           (= (wb::position-y new-pos) (wb::position-y current-position)))
      (if clipping-region
        (wb::canvas-clear canvas
                      :canvas-left (wb::region-left clipping-region)
                      :canvas-bottom (wb::region-bottom clipping-region)
                      :width (wb::region-width clipping-region)
                      :height (wb::region-height clipping-region))
        (wb::canvas-clear canvas))
      (display-graph (wb::display-of canvas) canvas clipping-region))))


(defun erase-node-label (node canvas)
  "Erases the label of the node in the canvas"
   (unless (eq 0 (graph-node-height node))
     (let ((left (+ (gn-left node)  -1))
           (bottom (+ (gn-bottom node)  -1))
           (width (+ (graph-node-width node) 2 ))
           (height (+ (graph-node-height node) 2)))
       (wb::canvas-clear  canvas
                      :canvas-left left
                      :canvas-bottom bottom 
                      :width width
                      :height height))))
