;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               grapher-node.lisp
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

;;; This file, split off from grapher.lisp, defines graph-node, operations on
;;; graph-node, and operations on lists of graph-node.

(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(graph-node-position
          graph-node-height
          graph-node-width
          graph-node-font
          graph-node-label-symbol
          graph-node-to-nodes
          graph-node-id
          gn-bottom
          gn-left
          make-graph-node
          to-links
          flip-node)))


;; This structure should become a class.  Subclasses will probably make the last
;; three slots into class variables.  Add instance variable selected-p. -- jrm

(defstruct graph-node
  id
  position
  label-bitmap
  width
  height
  to-nodes
  from-nodes
  label-symbol
  (label-shade *graph-default-node-label-shade*)
  (font *graph-default-node-font*)
  (border *graph-default-node-border*))


;; Make accessors for the node "position"

(defun get-node-centre (node) (graph-node-position node))

(defun set-node-centre (node value) (setf (graph-node-position node) value))


;; The accessors from-links and to-links will go into the defclass.

(defun from-links (node) (graph-node-from-nodes node))

(defun to-links (node) (graph-node-to-nodes node))


;; Define a half function, safer than the macro which looks messy if x<0.
;; The function can claim integer-p if we think we can prove it.
;; Other than this file, only grapher-layout.lisp calls this half.

(defun half (x)
  "Integer division by 2, rounded down."
  (when x
    (/
     (if (oddp x) (1- x) x)
     2)))



(defun set-layout-position (node position)
  "Sets a node position."
  
  (setf (wb:position-x (graph-node-position node))
        (wb:position-x position))
  (setf (wb:position-y (graph-node-position node))
        (wb:position-y position)))


;; Beware that graph-layout uses NIL graph-node-position,
;; and also uses NIL position-x, position-y within it. -- jrm

(defun gn-bottom (node &aux (pos (graph-node-position node)))
  "Determines the bottom position of a Graph-Node."
   
     (if pos
       (- (or (wb:position-y pos) 0)
          (half (or (graph-node-height node) 0)))
       0))

(defun gn-left (node &aux (pos (graph-node-position node)))
  "Determines the left position of a Graph-Node."
 
       (if pos
           (- (or (wb:position-x pos) 0)
              (half (or (graph-node-width node) 0)))
           0))

(defun gn-right (node &aux (pos (graph-node-position node)))
  "Determines the right position of a Graph-Node."

       (if pos
           (+ (or (wb:position-x pos) 0)
              (- (half (+ 1 (or (graph-node-width node) 0)))
                 1))
           0))

(defun gn-top (node &aux (pos (graph-node-position node)))
  "Determines the top position of a Graph-Node."

       (if pos
          (+ (or (wb:position-y pos) 0)
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

(defun mark-graph-node (node)
  "Changes appearance of graph node to indicate that a link has been snapped."

       (declare (special border-for-marking label-shade-for-marking))
       (or (eq border-for-marking 'dont)
           (setf (graph-node-border node)
                 border-for-marking))
       (or (eq label-shade-for-marking 'dont)
           (setf (graph-node-label-shade node)
                 label-shade-for-marking)))


(defun measure-graph-node (node &optional (reset-flg nil))
  "Measure the node label image."

       (set-label-size node reset-flg)
       ;; This is redundant!
       ;;(set-layout-position node (or (graph-node-position node)
       ;;                              (quail-error 
       ;;                    "This Graph node has not been given a position : ~S"
       ;;                                     node)))
       )



;; Wow, look at how the node gets cloned. -- jrm

(defun new-instance-of-graph-node (node)
  "Returns a second instance of the graph-node, boxing it."
  (declare (special the-node-lst box-leaves-flg box-both-flg))
  (prog ((new (make-graph-node
               :id (list (graph-node-id node))
               :label-symbol (graph-node-label-symbol node)
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


;; print-display-node has two callers: display-graph and move-node-to.
;; Trouble seems to occur because callers somehow set clip-reg incorrectly.
;; In both cases, the callers got clip-reg from their callers.
;; In the case of display-graph, its caller redisplay-graph sends a
;; clipping 'region' gotten by (wb:clipping-region-of window).  One caller
;; of redisplay-graph is show-graph, an exported function. -- jrm

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

 ; This when command must be wrong -- either bad logic or using bad data -- jrm
#|
      (when (and clip-reg
                 (not (intersect-regionp-lbwh
                       left bottom width height clip-reg)))
        (return node))
|#
       node

      (cond ((wb:bitmap-p (graph-node-label-bitmap node))
             (wb:canvas-bitblt (graph-node-label-bitmap node) window
                               :canvas-left left
                               :canvas-bottom bottom
                               :width width :height height
                               :operation operation))
            ((wb:bitmap-p (graph-node-label-symbol node))
             (cond ((eq 0 nbw)
                    (wb:canvas-bitblt (graph-node-label-symbol node) window
                                      :canvas-left left
                                      :canvas-bottom bottom
                                      :width width :height height
                                      :operation operation))
                   (t
                    (draw-graph-node-border (graph-node-border node)
                                            left bottom width height window)
                    (wb:canvas-bitblt (graph-node-label-symbol node) window
                                      :canvas-left (+ left nbw)
                                      :canvas-bottom (+ bottom nbw)
                                      :width width :height height
                                      :operation operation))))
            (t (or (wb:canvas-font-p font)
                   (setf (graph-node-font node)
                         (setq font (wb:canvas-font window))))
               (if (not (eq nbw 0))
                 (draw-graph-node-border
                  (graph-node-border node)
                  left bottom width height window))
               (wb:canvas-draw-string
                window (graph-node-label-symbol node)
                :left left :bottom bottom :width width :height height 
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
                       (wb:make-bitmap :width width :height height))
                 (wb:copy-canvas-region-to-bitmap window left 
                                                  bottom (graph-node-label-bitmap node)
                                                  width height))))
      (return node))))


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
      (wb:canvas-draw-rectangle stream x1 x2 y1 y2
                             :width border-width
                             :color color
                             :dashing dashing
                             :operation operation))))


(defun fill-graph-node-label
       (shade left bottom width height border window)
  (declare (ignore shade))
  ;; 
  ;;  border must be subtracted from the node's region
  ;;
  (wb:canvas-invert window                                ;<----- Better as a draw-filled?
                 :canvas-left (+ left border)          ;       ... rwo
                 :canvas-bottom (+ bottom border)
                 :width (- width border border)
                 :height (- height border border)))


(defun erase-node-label (node canvas)
  "Erases the label of the node in the canvas"
  (wb:with-focused-canvas canvas
    (unless (eq 0 (graph-node-height node))
      (let ((left (+ (gn-left node)  -1))
            (bottom (+ (gn-bottom node)  -1))
            (width (+ (graph-node-width node) 2 ))
            (height (+ (graph-node-height node) 2)))
        (wb:canvas-clear  canvas
                           :canvas-left left
                           :canvas-bottom bottom 
                           :width width
                           :height height)))))


(defun flip-node (node window)
  "Flip the region around the node."
  
  (wb:canvas-invert window
                 :canvas-left
                 (+ (gn-left node) -1)
                 :canvas-bottom
                 (+ (gn-bottom node) -1)
                 :width (+ (graph-node-width node) 2)
                 :height (+ (graph-node-height node) 2)))


(defun set-label-size (node &optional reset-flg)
  
  (declare (special *graph-default-node-font*))
  ;;; 
  ;;; the SHADE and null font stuff is for ZOOM-GRAPH
  ;;; 
  
  (or (and (not reset-flg)
           (integerp (graph-node-height node))
           (integerp (graph-node-width node)))
      (prog ((font (graph-node-font node))
             (lab (graph-node-label-symbol node))
             (nbw (graph-node-border-width (graph-node-border node)))
             width height)
        (cond ((wb:bitmap-p lab)
               (setq width (wb:bitmap-width lab))
               (setq height (wb:bitmap-height lab)))
              (t (or (stringp lab)
                     (if (symbolp lab)
                       (setq lab (symbol-name lab))
                       (setq lab (format nil "~s" lab))) )
                 (or (wb:canvas-font-p font)
                     (setf (graph-node-font node)
                           (setq font *graph-default-node-font*)))
                 (setq width (wb:canvas-string-width NIL lab :font font))
                 (setq height (+ (wb:canvas-font-height font)
                                 (wb:canvas-font-descent font)))))
        (or (and (not reset-flg)
                 (integerp (graph-node-width node)))
            (setf (graph-node-width node)
                  (+ width nbw nbw)))
        (or (and (not reset-flg)
                 (integerp (graph-node-height node)))
            (setf (graph-node-height node)
                  (+ height nbw nbw)))
        (return node))))

