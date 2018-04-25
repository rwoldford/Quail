;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                                euler.lsp                               
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
  (export '(extend-complete-graph-to-euler get-euler-trail
            euler-pairwise-order mirror-euler-pairwise-order)))




(defun extend-complete-graph-to-euler (complete-graph)
  "Adds sufficient edges to the complete graph such that the ~
   number of edges at each vertex is even."
  (let ((vertices (vertices-of complete-graph))
        edges
        )
    (loop
      for v in vertices
      unless (evenp (number-of-edges-of v))
      do
      (let ((connection-made? NIL)
            (other-vertices (remove v vertices))
            edge)
        (loop
          until connection-made?
          for ov in other-vertices
          unless (evenp (number-of-edges-of ov))
          do
          (setf connection-made? T)
          (setf edge (connect-vertices v ov))
          (push edge edges))
        )
      )
    (loop for e in edges
          do (add-edge complete-graph e))
    complete-graph))



(defun get-euler-trail (planar-graph &key (start NIL) (comparison-fn NIL))
  
  
  ;; Error checking.
  (let
    ((vertices (vertices-of planar-graph))
     returned-vertices
     (remaining-edges (copy-list (edges-of planar-graph)))
     next-route
     current-node)
    (unless (reduce #'(lambda (x y)
                        (and x
                             (evenp (number-of-edges-of y)))
                        )
                    vertices
                    :initial-value T)
      (quail-error "No Euler trail exists because there are vertices ~
                    in the graph which have an odd number of edges ~
                    associated with them.")
      )
    
    (unless (member start (vertices-of planar-graph))
      (setf start (first (vertices-of planar-graph))))
    
    (unless (functionp comparison-fn)
      (setf comparison-fn
            (function (lambda (a b)
                        (declare (ignore a b))
                        1)))
      )
    
    ;; Here comes the algorithm
    
    (labels
      ;; some helper functions
      ((collect-eligible-routes (node)
         (loop for edge in (edges-of node)
               unless (travelled-edge-p edge)
               collect edge))
       
       (number-of-eligible-routes (node)
         (length (collect-eligible-routes node)))
       
       (destination-of (edge from)
         (let ((remaining-vertices (remove from (vertices-of edge))))
           (if (= 1 (length remaining-vertices))
             (first remaining-vertices)
             remaining-vertices)))
       
       (find-best-route (available-routes from)
         (let (best-route)
           
           (setf available-routes
                 (loop for route in available-routes
                       for dest = (destination-of route from)
                       unless
                       (and (eq dest start)
                            (= 1 (number-of-eligible-routes start))
                            (setf best-route route))
                       collect route))
           (if (null available-routes)
             best-route
             (reduce #'(lambda (a b)
                         (if (<= (graph-value-of a) (graph-value-of b))
                           a
                           b))
                     available-routes)
             )
           )
         )
       
       
       )
      
      
      ;; Initialization
      (loop
        for edge in remaining-edges
        unless (graph-value-of edge)
        do
        (setf (graph-value-of edge)
              (apply comparison-fn
                     (mapcar #'graph-value-of (vertices-of edge)))))
      
      (setf current-node start)
      
      (push current-node returned-vertices)
      
      ;; Here goes the traversal
      (loop
        until (null remaining-edges)
        do
        (setf next-route
              (find-best-route (collect-eligible-routes current-node)
                               current-node)
              )
        
        (mark-edge-travelled next-route)
        (setf current-node (destination-of next-route current-node))
        (setf remaining-edges (remove next-route remaining-edges))
        (push current-node returned-vertices)
        )
      (reverse returned-vertices)
      )
    )
  )



;;; 
;;;
;;;   Putting it together to give an Euler pairwise ordering
;;;


(defun euler-pairwise-order (items &key (comparison-fn)
                                   (start NIL))
  "Returns a list of the items repeated so that each item in items~
   repeated often enough so ~
   that every pair of items occur beside one another (and no oftener). ~
   Choice of ordering will depend upon the comparison-fn.  The ~
   comparison-fn is a function which can be called on any ~
   two items and which will return a numerical score where two items ~
   are considered closer or more similar if the score is smaller and ~
   the opposite if it is larger.  If supplied, start is that element (eq) of items ~
   from which the Euler trail is to start (and finish). ~
   Returns multiple-values: euler trail of original items, ~
   euler-trail of vertices in the graph, the planar-graph itself."
  (let* ((planar-graph (make-complete-graph items))
         (euler-graph (extend-complete-graph-to-euler planar-graph))
         start-vertex
         euler-trail
         return-values)
    (when start
      (setf start-vertex
            (find start (vertices-of euler-graph)
                  :test #'(lambda (x y)
                            (or (equal x (graph-value-of y))
                                (and (numberp x)
                                     (numberp (graph-value-of y))
                                     (= x (graph-value-of y))
                                     ))))))
    (setf euler-trail (get-euler-trail euler-graph 
                                       :comparison-fn comparison-fn
                                       :start start-vertex))
    (setf return-values
          (mapcar #'graph-value-of euler-trail ))
    (values return-values euler-trail euler-graph )
    )
  )


(defun mirror-euler-pairwise-order
       (items
        &key
        (comparison-fn NIL)
        (start NIL)
        (mirror-fn #'(lambda (x)
                       (list :mirror x)))
        (match-fn #'(lambda (x y)
                      (cond
                       ((and x (listp x) (eq (first x) :mirror))
                        (setf x (second x)))
                       ((and y (listp y) (eq (first y) :mirror))
                        (setf y (second y)))
                       )
                      (equal x y))
                  )  
        )
  "Like euler-pairwise-order except that each item in items has a mirrored ~
   version found by calling mirror-fn on it.  The original item and its ~
   mirrored version are recognized as a match by funcalling the match-fn ~
   on both.  Default mirroring matches item with the list (:mirror item). ~%~
   Returns an euler pairwise list of the combined items and mirrored-items ~
   with the exception that no item is beside its mirrored self. ~
   Otherwise the returned list is such that each item~
   in the combined list is repeated often enough so ~
   that every pair of items occur beside one another (and no oftener). ~
   Choice of ordering will depend upon the comparison-fn.  The ~
   comparison-fn is a function which can be called on any ~
   two items and which will return a numerical score where two items ~
   are considered closer or more similar if the score is smaller and ~
   the opposite if it is larger.  If supplied, start is that element (eq) of ~
   combined items from which the Euler trail is to start (and finish). ~%~
   Returns multiple-values: euler trail of original items, ~
   euler-trail of vertices in the graph, the planar-graph itself. ~
   (:see-also (euler-pairwise-order :function))"
  (let* ((new-items 
          (append items
                  (loop for item in items
                        collect
                        (funcall mirror-fn item))))
         (planar-graph (make-complete-graph new-items))
         euler-graph
         euler-trail
         return-values
         start-vertex
         )
    (when start
      (setf start-vertex
            (find start (vertices-of euler-graph)
                  :test #'(lambda (x y)
                            (or (equal x (graph-value-of y))
                                (and (numberp x)
                                     (numberp (graph-value-of y))
                                     (= x (graph-value-of y))
                                     ))))))
    ;; remove mirrored edges
    (loop for edge in (edges-of planar-graph)
          for v1 = (first (vertices-of edge))
          for v2 = (second (vertices-of edge))
          when
          (funcall match-fn
                   (graph-value-of v1)
                   (graph-value-of v2))
          do
          (remove-edge v1 edge)
          (remove-edge v2 edge)
          (remove-edge planar-graph edge)
          )
    (setf euler-graph (extend-complete-graph-to-euler planar-graph))
    (setf euler-trail
          (get-euler-trail euler-graph 
                           :comparison-fn comparison-fn
                           :start start-vertex))
    (setf return-values (mapcar #'graph-value-of euler-trail))
    (values return-values euler-trail euler-graph)
    )
  )




#|

(euler-pairwise-order '(a b c d e))

(loop for item in '(a b c d e)
      collect
      (euler-pairwise-order '(a b c d e) :start item))
(euler-pairwise-order '(a b c d e) :start 'c)

(mirror-euler-pairwise-order '(a b c d e))
(mirror-euler-pairwise-order '(1 2 3 4 5)
                             :mirror-fn #'-
                             :match-fn
                             #'(lambda (x y)
                                 (= x (- y)))
                             )


(setf items (append (iseq 1 5) (mapcar #'- (iseq 1 5))))

(setf planar-graph (make-complete-graph items))

(loop for edge in (edges-of planar-graph)
      for v1 = (first (vertices-of edge))
      for v2 = (second (vertices-of edge))
      when
      (= (graph-value-of v1)
         (- (graph-value-of v2)))
      do
      (remove-edge v1 edge)
      (remove-edge v2 edge)
      (remove-edge planar-graph edge)
      )

(inspect planar-graph)

         (setf euler-graph (extend-complete-graph-to-euler planar-graph))

    (mapcar #'graph-value-of
            (get-euler-trail euler-graph))

(equal '(1 2) (reverse '(2 1)))
|#