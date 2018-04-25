;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               grapher-layout.lisp
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
;;;     J.R. MacPhail 1995
;;;     R.W. Oldford  1988-1992
;;;
;;;
;;;----------------------------------------------------------------------------------

;;; This file is the part of old "grapher.lisp" that lays it out. -- jrm


(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(layout-graph)))


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
                   (wb:make-position (+ x (half width)) nil))
             (cond ((null ds))
                   ((> y-height (setq d-height (brh-layout-daughters
                                                ds
                                                (+ x width mother-d)
                                                y
                                                (list n))))
                    (brh-offset ds (half (- y-height d-height))))
                   (t (setq y-height d-height)))
             (setf (wb:position-y (graph-node-position gn))
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
      (declare (special the-node-lst))
       (dolist (n node-ids)
           (setq n (get-node-from-id n the-node-lst))
           (setf (wb:position-y (graph-node-position n))
                 (+ (wb:position-y (graph-node-position n))
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
            (loop (setq dist (- (wb:position-y op)
                                (wb:position-y np)))
                  (and (> dist raise)
                       (setq raise dist))
                  (cond ((null btc)
                         (return raise))
                        ((null ttc)
                         (return raise))
                        ((eq (wb:position-x (first btc))
                             (wb:position-x (first ttc)))
                         (setq np (pop btc))
                         (setq op (pop ttc)))
                        ((< (wb:position-x (first btc))
                            (wb:position-x (first ttc)))
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
                   (wb:make-position (+ x (half w)) y-center))
             (push (wb:make-position x (+ personal-d (- y-center h/2) h))
                   return-ttc)
             (push (wb:make-position x (- y-center h/2))
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
            (setq return-ttc (cons (wb:make-position x-sw (wb:position-y (first ttc)))
                                   ttc))
            (setf (wb:position-x (first btc))
                  x-sw)
            (setf (wb:position-y (first ttc))
                  (+ (wb:position-y (first ttc))
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
       (setq return-ttc (list (wb:make-position x-sw 0)))
       (setq return-btc (list (wb:make-position x-sw (graph-node-height gn))))
       (half (graph-node-height gn)))


(defun brhc-offset (n absy)
  "Adds  in all the offsets. see comment on BHRC-LAYOUT-DAUGHTERS."

       (declare (special the-node-lst))
       (prog ((gn (get-node-from-id n the-node-lst)))
             (setf (wb:position-y (graph-node-position gn))
                   (+ absy (wb:position-y (graph-node-position gn))))
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
                       (wb:make-position (+ x (half width)) (list n)))
                 (and ds (setq y-height (max (brhl-layout-daughters
                                              ds
                                              (+ x width mother-d)
                                              y
                                              (list n))
                                             y-height)))
                 (setf (wb:position-y (graph-node-position gn))
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
                     (cond ((null (wb:position-y np))
                            (setq gn (new-instance-of-graph-node gn))
                            (setf (first d-tail)
                                  (graph-node-id gn))
                            (setq the-floor (+ the-floor
                                               (brhl-layout (graph-node-id
                                                             gn)
                                                      x the-floor mom-lst gn))))
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
                  (quail-error "Loop caught in BRHL-MOVE-RIGHT at ~S"
                               (graph-node-label-symbol gn)))
             (when (< x (- (wb:position-x np)
                           (half width)))
                   (return))
             (let ((new-x (+ x width mother-d))
                   (new-stack (cons gn stack)))
                  (dolist (d (to-links gn))
                      (brhl-move-right (get-node-from-id d the-node-lst)
                             new-x new-stack)))
             (setf (wb:position-x np)
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
           (brhc-layout roots 0 nil (get-node-from-id roots the-node-lst))
           (brhc-offset roots 0))
          ((null (cdr roots))
           (brhc-layout (first roots) 0 nil (get-node-from-id (first roots) the-node-lst))
           (brhc-offset (first roots) 0))
          (t (prog ((gn (make-graph-node :label-symbol (make-symbol "")
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
                   (setq rn (get-node-from-id n the-node-lst))
                   (setf (graph-node-from-nodes rn)
                         (remove top-node (graph-node-from-nodes rn)))))
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
      (setq y (+ y (brhl-layout n 0 y nil (get-node-from-id n the-node-lst)))))
    (make-graph :node-lst the-node-lst :side-flg t :directed-flg nil)))


(defun extend-transition-chain (ltc rtc)
  "Extends the left transition chain by appending the part of the right ~
   transition chain that is to the right of the end of the left transition ~
   chain. End point of left transition chain is changed to intersect right ~
   transition chain."

  (let ((l-tail ltc)
        (r-tail rtc)
        lx rx)
    (loop (cond ((null (cdr r-tail))
                 (setf (wb:position-y (first (last l-tail)))
                       (wb:position-y (first r-tail)))
                 (return ltc))
                ((null (cdr l-tail))
                 (rplacd l-tail (cdr r-tail))
                 (setf (wb:position-y (first l-tail))
                       (wb:position-y (first r-tail)))
                 (return ltc))
                ((eq (setq lx (wb:position-x (second l-tail)))
                     (setq rx (wb:position-x (second r-tail))))
                 (setq l-tail (cdr l-tail))
                 (setq r-tail (cdr r-tail)))
                ((< lx rx)
                 (setq l-tail (cdr l-tail)))
                (t (setq r-tail (cdr r-tail)))))))


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


(defun lattice-break-cycles (node stack)

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
  "Layout in lattice for tests"

  (let (roots)
    (dolist (node (graph-node-lst graph))
      (unless (dolist (link (graph-node-from-nodes node))
                (when (get-node-from-id link (graph-node-lst graph))
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
    (setq font (if (wb:canvas-font-p font)
                 font
                 *graph-default-node-font*))
    (or mother-d (setq mother-d
                       (wb:canvas-string-width NIL "AAAAAA"
                                                :font font)))
    (or personal-d (setq personal-d (if (eq-or-member 'vertical format
                                                      )
                                      (wb:canvas-string-width
                                       NIL "AA" :font font)
                                      0)))
    (or family-d (setq family-d (half (wb:canvas-font-ascent font))))
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
      (or (wb:position-p  (graph-node-position n))
          (quail-error 
           "Disconnected graph. Root(s) didn't connect to : ~S"
           (graph-node-label-symbol n))))
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
       
  (declare (special result))
  (cond ((dolist (r result nil)
           ; If Tree is already in result nothing is done
           (when (eq tree (graph-node-id r))
             (return t))))
        ((not (listp tree))
         ; if Tree is a simple node it is added in result
         (push (make-graph-node :id tree :label-symbol tree)
               result))
        ; otherwise Tree is a node with subnodes which are recursively added to result
        (t (push (make-graph-node :id tree :label-symbol (first tree)
                                  :to-nodes
                                  (append (cdr tree)))
                 result)
           (dolist (d (cdr tree))
             (layout-s-expr-1 d)))))


(defun raise-transition-chain (tc raise)
  "Raises a daughter's transition chain by adding in the offset of the ~
   daughter's box relative to the mother's box."

  (dolist (p tc tc)
    (setf (wb:position-y p)
          (+ (wb:position-y p)
             raise))))

