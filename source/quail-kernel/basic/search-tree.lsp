;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                           search-tree.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (C) 1992 Statistical Computing Laboratory, University Of Waterloo
;;;
;;;
;;;  Authors:
;;;     M.E. Lewis 1992.
;;;
;;;
;;;----------------------------------------------------------------------------

(in-package :quail-kernel)

;;;;

(defmethod family-tree ((tree key-tree) (me node))
  (labels ((fam (node-list node)
             (cond ((null node) NIL)
                   (t
                    (cons (name node)
                          (fam node-list
                               (as-node tree (dad node))))))))
    (fam (nodes tree) me)))

(defmethod successor-nodes ((tree key-tree))
  #'(lambda (node)
      (mapcar #'(lambda (son)
                  (as-node tree son))
              (sons node))))

(defmethod share-family-tree-p ((tree key-tree) value)
  #'(lambda (node)
      (let ((f-tree (family-tree tree node)))
        (and (= (length value)
                (length f-tree))
             (every #'(lambda (x y)
                        (contained x (atomcar y)))
                    value f-tree)))))

(defun atomcar (obj)
  (cond ((null obj) nil)
        ((atom obj) obj)
        (t
         (atomcar (car obj)))))

(defun contained (x y &key (test #'equalp))
  (eql 0 (search x
                 y
                 :test test)))

