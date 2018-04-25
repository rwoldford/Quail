;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                           tree.lisp                               
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

;;;; NODES

(defclass node ()
  ((name :initarg :name :accessor name :initform NIL)
   (dad :initarg :dad :accessor dad :initform NIL)
   (pedigree :initarg :pedigree :accessor pedigree :initform NIL)
   (sons :initarg :sons :accessor sons :initform NIL)
   (value :initarg :value :accessor value :initform NIL)
   (hold :initarg :hold :accessor hold :initform NIL)
   (show :initarg :show :accessor show :initform NIL)
   ))

(defmethod show ((me t))
  #'(lambda () me))

(defmethod print-object ((self node) stream)
  (format stream "#<ND ~a>" (name self)))

(defun root-node-p (node)
  (equalp ":root" (name node)))

;;;; KEY-TREES

(defclass key-tree ()
  ((nodes :initarg :nodes :accessor nodes :initform NIL)))

(defun make-key-tree (nested-list)
  (make-instance 'key-tree
    :nodes (mapcar #'(lambda (x)
                       (make-instance 'node
                         :name (first x)
                         :dad (second x)
                         :sons (third x)))
                   (node-specs nested-list))))

(defun make-empty-tree ()
  (make-key-tree '((":root"))))

(defmethod as-node ((me key-tree) name)
  (find name (nodes me) :test #'equalp :key #'name))

(defmethod root-tree ((me key-tree))
  (make-instance 'key-tree
    :nodes (list (root-node me))))

(defmethod root-node ((me key-tree))
  (as-node me ":root"))

(defmethod root-pedigree ((me key-tree))
  (pedigree (root-node me)))

;;;;

(defmethod find-node ((me key-tree) trail)
  (find trail (nodes me) :test #'equalp :key #'pedigree))

(defmethod add-node ((tree key-tree) (me node))
  (make-instance 'key-tree
    :nodes (cons me (nodes tree))))

;;;; DECORATING A TREE

(defun decorate (key-tree result-tree dad item)
  (cond ((null item) 
         (collect-items result-tree dad))
        ((atom item) 
         (add-item result-tree dad item))
        (t
         (cond ((find-node result-tree (cons (first item) dad))
                (decorate key-tree
                          result-tree
                          (cons (first item) dad)
                          (rest item)))
               ((find-node key-tree (cons (first item) dad))
                (decorate key-tree
                          (add-node result-tree
                                    (find-node key-tree 
                                               (cons (first item) dad)))
                          (cons (first item) dad)
                          (rest item)))
               (t
                (decorate key-tree
                          (decorate key-tree
                                    result-tree
                                    dad
                                    (first item))
                          dad
                          (rest item)))))))

(defun add-item (result-tree dad item)
  (push item (hold (find-node result-tree dad)))
  result-tree)

(defun collect-items (result-tree dad)
  (unless (null (hold (find-node result-tree dad)))
    (push (hold (find-node result-tree dad))
          (value (find-node result-tree dad)))
    (setf (hold (find-node result-tree dad))
          NIL))
  result-tree)

;;;; utility functions

(defun node-specs (nested-list &optional (dad nil))
  (cond ((null nested-list) nil)
        ((atom nested-list)
         (list 
          (list nested-list dad NIL)))
        ((atom (car nested-list))
         (cons
          (list (car nested-list)
                dad
                (mapcar #'atomcar (cdr nested-list)))
          (mapcan #'(lambda (x)
                      (cond ((null x) nil)
                            ((atom x)
                             (list
                              (list x (car nested-list) NIL)))
                            (t
                             (node-specs x
                                         (car nested-list)))))
                  (cdr nested-list))))
        (t
         (mapcan #'(lambda (x)
                     (node-specs x dad))
                 nested-list))))

(defun tree-apply (fn tree)
  (let ((result nil))
    (dolist (v tree (nreverse result))
      (cond ((null v) (push "NIL" result))
            ((atom v) (push (funcall fn v) result))
            (t
             (push (tree-apply fn v) result))))))

(defun tree-remove (item tree &key (test #'equalp))
  (cond ((null tree) nil)
        ((atom tree) tree)
        ((funcall test item (car tree))
         (tree-remove item (cdr tree) :test test))
        (t
         (cons (tree-remove item (car tree) :test test)
               (tree-remove item (cdr tree) :test test)))))



