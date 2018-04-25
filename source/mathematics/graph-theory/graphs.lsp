;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                                graphs.lsp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (C) 2001 Statistical Computing Laboratory, University Of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 2001.
;;;
;;;
;;;----------------------------------------------------------------------------

(in-package :quail-kernel)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(vertex make-vertex edge make-edge make-planar-graph
            planar-graph add-edge remove-edge add-vertex
            remove-vertex directed-edge edges-of vertices-of graph-value-of
            start-of-edge end-of-edge travelled-edge-p
            mark-edge-travelled mark-edge-untravelled
            number-of-edges-of number-of-vertices-of connected-vertices-p
            connect-vertices make-complete-graph
            determine-graph-from)
          ))

(defgeneric add-edge (graph-thing edge)
  (:documentation "Adds the edge to some kind of graph (sub)structure.")
  )

(defgeneric remove-edge (graph-thing edge)
  (:documentation "Removes the edge to some kind of graph (sub)structure.")
  )

(defgeneric add-vertex (graph-thing vertex)
  (:documentation "Adds the vertex to some kind of graph (sub)structure.")
  )

(defgeneric remove-vertex (graph-thing vertex)
  (:documentation "Removes the vertex to some kind of graph (sub)structure.")
  )

(defgeneric number-of-vertices-of (graph-thing)
  (:documentation "Returns the number of vertices of a graph (sub)structure.")
  )

(defgeneric number-of-edges-of (graph-thing)
  (:documentation "Returns the number of edges of a graph (sub)structure.")
  )

;;;;;;;
;;;
;;;   Classes
;;;

(defclass vertex ()
  ((edges  :initform NIL :initarg :edges
           :accessor edges-of
           :documentation
           "The edges connected to this vertex.")
   (graph-value  :initform NIL :initarg :graph-value
           :accessor graph-value-of
           :documentation "A place to store an arbitrary value associated ~
                           with this vertex.")
   )
  (:documentation
   "A class structure representing a vertex (or node) in a planar graph.")
  )

(defun make-vertex (&key (graph-value NIL) (edges NIL))
  "Creates and returns a vertex."
  (let ((vertex (make-instance 'vertex :graph-value graph-value :edges edges))
        )
    (loop
      for edge in edges
      do (add-vertex edge vertex))
    vertex)
  )



(defclass edge ()
  ((vertices  :initform NIL :initarg :vertices
              :accessor vertices-of
              :documentation
              "The vertices connected by this edge.")
   (graph-value  :initform NIL :initarg :graph-value
           :accessor graph-value-of
           :documentation "A place to store an arbitrary value associated ~
                           with this edge.")

   (travelled?  :initform NIL :initarg :travelled?
           :accessor travelled-edge-p
           :documentation "A place to store a flag to indicate whether this ~
                           edge has been travelled by some path.  Useful for marking~
                           paths in a graph.")
   )
  (:documentation
   "A class structure representing an edge in a planar graph.")
  )


(defun make-edge (&key (graph-value NIL) (vertices NIL))
  "Creates and returns a graph edge."
  (let ((edge (make-instance 'edge :graph-value graph-value :vertices vertices))
        )
    (loop
      for vertex in vertices
      do (add-edge vertex edge))
    edge)
  )


(defun mark-edge-travelled (edge)
  "As the name suggests, this function marks the edge as having ~
   been travelled."
  (setf (travelled-edge-p edge) T))

(defun mark-edge-untravelled (edge)
  "As the name suggests, this function marks the edge as NOT having ~
   been travelled, i.e. as having been untravelled."
  (setf (travelled-edge-p edge) NIL))


(defclass directed-edge (edge)
  ((from  :initform NIL :initarg :from
          :accessor start-of-edge
          :documentation
          "The vertex at the start of by this directed edge.")
   (to  :initform NIL :initarg :to
        :accessor end-of-edge
        :documentation
        "The vertex at the end of by this directed edge.")
   )
  (:documentation
   "A class structure representing a directed edge in a planar graph.")
  )


(defmethod initialize-instance :after ((self directed-edge) &key from to vertices)
  (cond
   ((and vertices from to)
    (unless (eq (first vertices) from) (setf (first vertices) from))
    (unless (eq (second vertices) to) (setf (second vertices) to))
    )
   ((and from to)
    (setf (first vertices) from)
    (setf (second vertices) to)
    )
   ((and vertices (null from) (null to))
    (setf from (first vertices))
    (setf to (second vertices))
    )
   ((and vertices from (null to))
    (setf vertices (remove from vertices))
    (if  (/= 1 (length vertices))
      (quail-error "~&Vertex information is inconsistent."))
    (setf to (first vertices))
    (setf vertices (list from to)))
   ((and vertices to (null from))
    (setf vertices (remove to vertices))
    (if  (/= 1 (length vertices))
      (quail-error "~&Vertex information is inconsistent."))
    (setf from (first vertices))
    (setf vertices (list from to)))
   (T (quail-error "~&Vertex information is inconsistent."))
   )
  (setf (vertices-of self) vertices)
  (setf (start-of-edge self) from)
  (setf (end-of-edge self) from)
  )


(defclass planar-graph ()
  ((vertices  :initform NIL :initarg :vertices
              :accessor vertices-of
              :documentation
              "The vertices contained in this planar-graph.")
   (edges  :initform NIL :initarg :edges
           :accessor edges-of
           :documentation
           "The edges contained in this planar-graph.")
   )
  (:documentation
   "A class structure representing a planar graph.")
  )


(defun make-planar-graph (&key (vertices NIL) (edges NIL))
  "Creates and returns a planar-graph."
  (make-instance 'planar-graph :edges edges :vertices vertices)
  )

;;;;;;
;;;
;;;   Methods
;;;


(defmethod add-edge :around (graph-thing (edge edge))
  (unless (member edge (edges-of graph-thing))
    (call-next-method)))

(defmethod add-edge ((vertex vertex) (edge edge))
  (push edge (edges-of vertex)))

(defmethod add-edge ((graph planar-graph) (edge edge))
  (push edge (edges-of graph)))

;;;

(defmethod remove-edge ((vertex vertex) (edge edge))
  (setf (edges-of vertex)
        (remove edge (edges-of vertex)))
  )

(defmethod remove-edge ((graph planar-graph) (edge edge))
  (setf (edges-of graph)
        (remove edge (edges-of graph)))
  )

;;;

(defmethod add-vertex :around (graph-thing (vertex vertex))
  (unless (member vertex (vertices-of graph-thing))
    (call-next-method)))

(defmethod add-vertex ((edge edge) (vertex vertex))
  (push vertex (vertices-of edge)))

(defmethod add-vertex ((graph planar-graph) (vertex vertex))
  (push vertex (vertices-of graph)))


;;;



(defmethod remove-vertex ((edge edge) (vertex vertex))
  (setf (vertices-of edge)
        (remove vertex (vertices-of edge)))
  )

(defmethod remove-vertex ((graph planar-graph) (vertex vertex))
  (setf (vertices-of graph)
        (remove vertex (vertices-of graph)))
  )

;;;


(defmethod number-of-vertices-of ((graph planar-graph))
  "Returns the number of vertices in the given graph."
  (length (vertices-of graph)))
 

(defmethod number-of-vertices-of ((edge edge))
  "Returns the number of vertices connected in the given edge."
  (length (vertices-of edge)))

;;;


(defmethod number-of-edges-of ((graph planar-graph))
  "Returns the number of edges in the given graph."
  (length (edges-of graph)))

(defmethod number-of-edges-of ((vertex vertex))
  "Returns the number of edges at the given vertex."
  (length (edges-of vertex)))


;;;


(defun determine-graph-from (start)
  "Beginning at the graph-element start, connections are followed ~
   to termination to produce the entire collection of vertices and ~
   edges which are connected to one another and to the start element.~
   A planar-graph object is returned containing the vertices and edges ~
   which were found."
  (let 
    (vertices-found edges-found)
    (labels
      ((chase-down-edges (vertex)
         (let ((new-edges
                (loop for e in (edges-of vertex)
                      unless (member e edges-found)
                      collect e)))
           (loop for e in new-edges do (push e edges-found))
           (loop for e in new-edges do (chase-down-vertices e))
           )
         )
       (chase-down-vertices (edge)
         (let ((new-vertices
                (loop for v in (vertices-of edge)
                      unless (member v vertices-found)
                      collect v)))
           (loop for v in new-vertices do (push v vertices-found))
           (loop for v in new-vertices do (chase-down-edges v))
           )
         )
       )
      (cond
       ((typep start 'vertex)
        (setf vertices-found (list start))
        (chase-down-edges start)
        )
       ((typep start 'edge)
        (setf edges-found (list start))
        (chase-down-vertices start)
        )
       )
      (make-instance 'planar-graph :edges edges-found :vertices vertices-found)
      )
    )
  )

;;; Other functions


(defun connected-vertices-p (vertex1 vertex2)
  "Returns list of common edges if an edge exists joining vertex1 and vertex2, NIL otherwise."
  (intersection  (edges-of vertex1) (edges-of vertex2)
                 :test #'(lambda (e1 e2)
                           (or (eq e1 e2)
                               (and (member vertex1 (vertices-of e1))
                                    (member vertex1 (vertices-of e2))
                                    (member vertex2 (vertices-of e1))
                                    (member vertex2 (vertices-of e2)))
                               )
                           )
                 )
  )

(defun connect-vertices (vertex1 vertex2 &key (edge-value NIL))
  "Makes an edge and connects the two vertices with this edge ~
   (having value of edge-value). ~
   Returns the edge which makes the connection."
  (let ((edge (make-edge  :vertices (list vertex1 vertex2)
                          :graph-value edge-value)))
    (add-edge vertex1 edge)
    (add-edge vertex2 edge)
    edge))

(defun complete-graph! (planar-graph)
  "Takes a planar graph and adds edges until the graph is complete, that is ~
   until every vertex is connected to every other vertex.  Changes the original ~
   graph and returns it."
  (let ((vertices (vertices-of planar-graph))
        other-vertices
        edge
        edges
        )
    (loop
      for vertex in vertices
      do
      (setf other-vertices (remove vertex vertices))
      (loop
        for ov in other-vertices
        unless (connected-vertices-p vertex ov)
        do
        (setf edge (connect-vertices vertex ov))
        (push edge edges)
        )
      )
    (loop for e in edges do (add-edge planar-graph e))
    planar-graph))

(defun make-complete-graph (items)
  "From any number of values to be associated with vertices, ~
   creates and returns a planar-graph in which every vertex is ~
   connected to every other vertex."
  (let ((vertices (loop for item in items
                        collect
                        (make-vertex :graph-value item)))
        )
    
    (complete-graph! (make-planar-graph :vertices vertices))))
