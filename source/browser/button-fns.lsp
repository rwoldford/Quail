;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               button-fns.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1988-1991 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1992
;;;
;;;
;;;----------------------------------------------------------------------------------
;;;

(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '()))

(defun *browser-left-button-fn* (canvas mouse-pos)
  
  (let ((node (find-graph-node canvas mouse-pos))
        (already-selected-nodes (selected-nodes-of canvas)))
    (cond
     (node
      (cond 
       ((member node already-selected-nodes)
        NIL)
       (T
        (loop for n in already-selected-nodes
              do (flip-node n canvas))
        (setf (selected-nodes-of canvas) (list node))
        (flip-node node canvas)))
      (move-node node mouse-pos canvas))
     (T 
      (loop for n in already-selected-nodes
            do (flip-node n canvas))
      (setf (selected-nodes-of canvas) NIL)))))



(defun *browser-shift-left-button-fn* (canvas mouse-pos)
  
  (let ((node (find-graph-node canvas mouse-pos))
        (already-selected-nodes (selected-nodes-of canvas)))
    (cond
     (node
      (cond 
       ((member node already-selected-nodes)
        (flip-node node canvas)
        (setf (selected-nodes-of canvas)
              (remove node already-selected-nodes)))
       (T
        (push node (selected-nodes-of canvas))
        (flip-node node canvas)))
      (move-nodes (selected-nodes-of canvas) mouse-pos canvas))
     (T
      (if (selected-nodes-of canvas)
        (move-nodes (selected-nodes-of canvas) mouse-pos canvas))))))

(defun *browser-ctrl-left-button-fn* (canvas mouse-pos)
  (*browser-left-button-fn* canvas mouse-pos))

(defun *browser-middle-button-fn* (canvas mouse-pos)
  (*browser-left-button-fn* canvas mouse-pos))

(defun *browser-shift-middle-button-fn* (canvas mouse-pos)
  (*browser-left-button-fn* canvas mouse-pos))

(defun *browser-ctrl-middle-button-fn* (canvas mouse-pos)
  (*browser-left-button-fn* canvas mouse-pos))

(defun *browser-right-button-fn* (canvas mouse-pos)
  (*browser-left-button-fn* canvas mouse-pos))

(defun *browser-shift-right-button-fn* (canvas mouse-pos)
  (*browser-left-button-fn* canvas mouse-pos))

(defun *browser-ctrl-right-button-fn* (canvas mouse-pos)
  (*browser-left-button-fn* canvas mouse-pos))
