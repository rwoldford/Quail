;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               graph.lisp
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
          graph-node-label-symbol
          graph-node-to-nodes
          graph-node-from-nodes
          graph-node-id
          graph-node-lst
          make-graph-node
          graph
          make-graph
          graph-node-lst)))

(defclass graph-node (simple-view)
  ((id :initform NIL :accessor graph-node-id)
   (position :initform NIL :accessor graph-node-position)
   (label-bitmap :initform NIL :accessor graph-node-label-bitmap)
   (width  :initform NIL :accessor graph-node-width)
   (height :initform NIL :accessor graph-node-height)
   (links :initform NIL :accessor graph-node-links)
   (to-nodes :initform NIL :accessor graph-node-to-nodes)
   (from-nodes :initform NIL :accessor graph-node-from-nodes)
   (label-symbol :initform NIL :accessor graph-node-label-symbol)
   (label-shade :initform *graph-default-node-label-shade*
                :accessor graph-node-label-shade )
   (font  :initform *graph-default-node-font*
          :accessor graph-node-font)
   (border  :initform *graph-default-node-border*
            :accessor graph-node-border))
  (:documentation  "A node of a directed-graph."))

#| The next step would be to remove the nodes from each other and insert a link.
(defgeneric graph-node-to-nodes (graph-node)
  (:documentation "Determines the nodes which are on a direct link from ~
                   the current node.  This could be at the end of a directed ~
                   or undirected link."))

(defmethod graph-node-to-nodes ((self graph-node))
  (loop for link in (graph-node-links-of self)
        when (graph-link-from-p link self)
        collect (graph-link-to-node link)))

;;; Need to do more checking here in order to use the same links
;;; between nodes.

(defmethod (setf graph-node-to-nodes) (new-nodes (self graph-node))
  (setf (graph-node-links self)
        (loop for new-node in new-nodes
              collect (make-instance 'graph-link :from-node self :to-node new-node)))
  new-nodes)

(defmethod (setf graph-node-from-nodes) (new-nodes (self graph-node))
  (setf (graph-node-links self)
        (loop for new-node in new-nodes
              collect (make-instance 'graph-link :from-node new-node :to-node self)))
  new-nodes)

(defmethod graph-node-from-nodes ((self graph-node))
  (loop for link in (graph-node-links-of self)
        when (graph-link-to-p link self)
        collect (graph-link-from-node link)))
|#



(defun make-graph-node (&rest args)
  "Makes an instance of graph-node."
  (if args
    (apply #'make-instance 'graph-node args)
    (make-instance 'graph-node)))

(defclass graph (pass-draws-to-subviews compound-view)
 ((node-lst :initform NIL :accessor graph-node-lst)
  (side-flg  :initform NIL :accessor graph-side-flg)
  (directed-flg :initform NIL :accessor graph-directed-flg))
 (:documentation "A directed graph."))

(defun make-graph (&rest args)
  "Makes an instance of a graph."
  (if args
    (apply #'make-instance 'graph args)
    (make-instance 'graph)))

(defclass graph-link (line-segment)
  ((from-node :initform NIL :accessor graph-link-from-node)
   (to-node :initform NIL :accessor graph-link-to-node)
   (directed? :initform T :accessor graph-link-directed-p)
   )
  (:documentation "A link between two nodes in a graph."))

(defun graph-link-from-p (link node)
  "Tests whether the argument node defines the root of the link. ~
   Note that undirected links are considered to be bi-directional ~
   so that if node is at either end of an undirected link, this function ~
   will return t. ~
   (:required ~
   (:arg link A graph link) ~
   (:arg node The node to be tested.)) ~
   (:returns T or NIL if the node is considered the root of a direction, or not)"
  (if (graph-link-directed-p link)
    (eq node (graph-link-from-node link))
    (or (eq node (graph-link-from-node link))
        (eq node (graph-link-to-node link)))))

(defun graph-link-to-p (link node)
  "Tests whether the argument node defines the end of the link. ~
   Note that undirected links are considered to be bi-directional ~
   so that if node is at either end of an undirected link, this function ~
   will return t. ~
   (:required ~
   (:arg link A graph link) ~
   (:arg node The node to be tested.)) ~
   (:returns T or NIL if the node is considered the end of a direction, or not)"
  (if (graph-link-directed-p link)
    (eq node (graph-link-to-node link))
    (or (eq node (graph-link-from-node link))
        (eq node (graph-link-to-node link)))))
