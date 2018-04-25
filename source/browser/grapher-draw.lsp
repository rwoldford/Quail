;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               grapher-draw.lisp
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

;;; This file is that part of the old "grapher.lisp" to do with drawing a whole graph.
;;; Drawing of individual nodes and links is in graph-node.lisp and graph-link.lisp

(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(show-graph
          move-node)))


;; This display-graph ALWAYS calls adjust-graph-positions (why?).
;; Also notice that highlighted nodes are drawn twice, which is crude. -- jrm

(defun display-graph (graph window clipping-region &optional line-width)
  "Displays graph on WINDOW. ~
   Draws links first, then labels so that lattices don't have lines through ~
   the labels."
  (wb:with-focused-canvas window
    (let* ((gr (graph-region graph))
           (g-top (wb:region-top gr))
           (g-left (wb:region-left gr))
           (c-region (wb:canvas-region window))
           (c-top (wb:region-top c-region))
           (c-left (wb:region-left c-region))
           (y-adjust (if (> g-top c-top)
                       (- c-top g-top)
                       0))
           (x-adjust (if (< g-left c-left)
                       (- c-left g-left)
                       0)))
      (adjust-graph-positions graph x-adjust y-adjust)
      (dolist (n (graph-node-lst graph))
        (display-node-links n window graph t line-width))
      (dolist (n (graph-node-lst graph))
        (print-display-node n window clipping-region))
      (let ((nodes (selected-nodes-of window)))
        (if nodes
          (dolist (node nodes)
            (flip-node node window)))))))


(defun redisplay-graph (window  &rest rest)
  "Displays the graph that is in a window."
  
  (declare (ignore rest))
  (wb:canvas-clear window)
  (when (wb:display-of window)
    (display-graph (wb:display-of window)
                   window
                   ;;(wb:canvas-region window)
                   (wb:clipping-region-of window))))


;; Largely rewritten.  I took out the calls that resized the window four times.
;; It is still necessary to fix this so TOP-JUSTIFY-FLAG is properly heeded.  -- jrm

(defun size-graph-window (graph window-or-title &optional (top-justify-flg T))
  "Returns a window sized to fit the given graph. WINDOW-OR-TITLE can be ~
   either a window to be printed in or a title of a window to be created.If ~
   TOP-JUSTIFY-FLG is T, scrolls so top of graph is at top of window, else ~
   puts bottom of graph at bottom of window."
  
  (declare (special *min-width-graph*  *min-height-graph*)
           (ignore TOP-JUSTIFY-FLG))
  
  (prog
    ((window))
 
    (cond
     ((wb:canvas-p window-or-title)
      (setq window window-or-title)
      (wb:canvas-to-top window-or-title))
     (t
      (setq window (make-browser :title (or window-or-title "Network display")))))

    (wb:shape-canvas
     window
     (max (wb:region-width (graph-region graph)) *min-width-graph*)
     (max (wb:region-height (graph-region graph)) *min-height-graph*))

    (return window)))
   

(defun show-graph (&optional graph window top-justify-flg)
  "Puts the graph in a new window, creating a graph if none is given."

       (setq window (size-graph-window
                           (or graph (setq graph (make-graph)))
                           window top-justify-flg))
       (setf (wb:display-of window) graph)
       (redisplay-graph window)
       window)


(defun move-node-to (node to-pos window
                          &optional
                          clipping-region)
  
  (wb:with-focused-canvas window
    (let* ((graph (wb:display-of window)))
      
      (erase-node-links node window graph)
      (erase-node-label node window)
      
      (set-node-centre node to-pos)
      
      (display-node-links node window graph)
      (print-display-node node window clipping-region)
  
;; Bizarrely, the code seems to work when this flip is removed.  Why? -- jrm    
;; (flip-node node window)
      )))


(defun relative-move-node-to (node dx dy window
                          &optional
                          clipping-region)
  (let ((current-pos (get-node-centre node)))
    (move-node-to
     node (wb:make-position (+ (wb:position-x current-pos) dx)
                            (+ (wb:position-y current-pos) dy))
     window
     clipping-region)))


(defun move-nodes (node-list current-position canvas
                             &optional
                             clipping-region)
  "Dynamically moves all nodes of node-list in the graph of the window canvas."
  
  (wb:with-focused-canvas canvas
    (let ((old-pos current-position)
          (new-pos current-position)
          dx dy)
      (loop while (wb:mouse-down-p) do
            (setf new-pos (wb:mouse-position canvas))
            (unless (and (= (wb:position-x new-pos) (wb:position-x old-pos))
                         (= (wb:position-y new-pos) (wb:position-y old-pos)))
              (loop
                for node in node-list do
                (setf dx (- (wb:position-x new-pos) (wb:position-x old-pos)))
                (setf dy (- (wb:position-y new-pos) (wb:position-y old-pos)))
                (relative-move-node-to node dx dy
                                       canvas clipping-region))
              
              (setf old-pos new-pos)))
      (unless
        (and (= (wb:position-x new-pos) (wb:position-x current-position))
             (= (wb:position-y new-pos) (wb:position-y current-position)))
        (if clipping-region
          (wb:canvas-clear canvas
                            :canvas-left   (wb:region-left clipping-region)
                            :canvas-bottom (wb:region-bottom clipping-region)
                            :width         (wb:region-width clipping-region)
                            :height        (wb:region-height clipping-region))
          (wb:canvas-clear canvas))
        (display-graph (wb:display-of canvas) canvas clipping-region)))))


(defun move-node (node current-position canvas
                       &optional
                       clipping-region)
  "Dynamically moves the node in the graph of the canvas."
  
  (wb:with-focused-canvas canvas
    (let ((old-pos current-position)
          (new-pos current-position))
      (loop while (wb:mouse-down-p) do
            (setf new-pos (wb:mouse-position canvas))
            (unless (and (= (wb:position-x new-pos) (wb:position-x old-pos))
                         (= (wb:position-y new-pos) (wb:position-y old-pos)))
              (move-node-to node new-pos canvas clipping-region)
              (setf old-pos new-pos)))
      (unless
        (and (= (wb:position-x new-pos) (wb:position-x current-position))
             (= (wb:position-y new-pos) (wb:position-y current-position)))
        (if clipping-region
          (wb:canvas-clear canvas
                            :canvas-left (wb:region-left clipping-region)
                            :canvas-bottom (wb:region-bottom clipping-region)
                            :width (wb:region-width clipping-region)
                            :height (wb:region-height clipping-region))
          (wb:canvas-clear canvas))
        (display-graph (wb:display-of canvas) canvas clipping-region)))))
