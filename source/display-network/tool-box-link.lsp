;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               tool-box-link.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1992 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1992.
;;;
;;;
;;;----------------------------------------------------------------------------

(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(tool-box-link-mixin
          )))

(defclass tool-box-link-mixin ()
  ((link-list :initform nil))
  (:documentation "Link manager of ToolBoxes.")
  )

(defmethod toolbox-link ((self tool-box-link-mixin) node linkee)
    "Establish a toolbox link between two quail-object-classes or toolboxes."

       (if (and (not (equal self node))
                (not (equal linkee node))
                (not (equal linkee self)))
           (progn (add-forward-toolbox-link self linkee node)
                  (add-back-toolbox-link self node linkee))))



(defmethod toolbox-unlink ((self tool-box-link-mixin)
                           node linkee)
  "Break an existing link between two quail-objectS or toolboxes - both ~
   directions."
  
  (remove-forward-toolbox-link self node linkee)
  (remove-back-toolbox-link self node linkee)
  (remove-forward-toolbox-link self linkee node)
  (remove-back-toolbox-link self linkee node))



(defmethod remove-forward-toolbox-link ((self tool-box-link-mixin)
                                        node linkee)
  "Removes linkee from the forward toolbox links of node."
  
  (let ((links-of-node (find-link self node)))
    (if links-of-node
      (progn (setf (slot-value self 'link-list)
                   (remove links-of-node (slot-value self
                                                     'link-list)))
             (setq links-of-node (list (first links-of-node)
                                       (remove linkee (second 
                                                       links-of-node
                                                       ))
                                       (third links-of-node)))
             (if (not (equal links-of-node (list node nil nil)))
               (push links-of-node (slot-value self 'link-list)))))
    ))



(defmethod remove-back-toolbox-link ((self tool-box-link-mixin)
                                     node linkee)
  "Removes linkee from the back toolbox links of node."
  (let ((links-of-node (find-link self node)))
    (if links-of-node
      (progn (setf (slot-value self 'link-list)
                   (remove links-of-node (slot-value self
                                                     'link-list)))
             (setq links-of-node (list (first links-of-node)
                                       (second links-of-node)
                                       (remove linkee (third 
                                                       links-of-node
                                                       ))))
             (if (not (equal links-of-node (list node nil nil)))
               (push links-of-node (slot-value self 'link-list)))))
    ))



(defmethod make-link ((self tool-box-link-mixin))
  "Add a new Toolbox link."
  
  (let ((node-1 (prompt-read self "Node to make link from : "))
        (node-2 (prompt-read self "Node to make link to : ")))
    (setq node-1 (or ($! node-1 t)
                     (find-class node-1)))
    (setq node-2 (or ($! node-2 t)
                     (find-class node-2)))
    (if (and node-1 node-2)
      (progn (toolbox-link self node-2 node-1)
             (recompute self)))))



(defmethod find-link ((self tool-box-link-mixin) node)
    "Return the element of link-list corresponding to node."

       (let (selection)
            (dolist (item (slot-value self 'link-list))
                (if (eq (car item)
                        node)
                    (return (setq selection item))))
            (if selection
                selection
                (list node nil nil))))



(defmethod break-link ((self tool-box-link-mixin))
  "Break an existing Toolbox link."

       (let ((node-1 (prompt-read self "Node to break link from : "))
             (node-2 (prompt-read self "Node to break link to : ")))
            (setq node-1 (or ($! node-1 t)
                             (find-class node-1)))
            (setq node-2 (or ($! node-2 t)
                             (find-class node-2)))
            (if (and node-1 node-2)
                (progn (toolbox-unlink self node-1 node-2)
                       (recompute self)))))



(defmethod add-forward-toolbox-link ((self tool-box-link-mixin)
                                     node linkee)
  "Adds Linkee to cadr of LinkList."
  
  (let ((links-of-node (find-link self node)))
    (if (not links-of-node)
      (push (list node nil (list linkee))
            (slot-value self 'link-list))
      (if (not (member linkee (second links-of-node)))
        (progn (setf (slot-value self 'link-list)
                     (remove links-of-node (slot-value self
                                                       'link-list)))
               (push linkee (second links-of-node))
               (push links-of-node (slot-value self 'link-list)))))
    ))



(defmethod add-back-toolbox-link ((self tool-box-link-mixin)
                                  node linkee)
    "Adds Linkee to CADDR of LinkList."

       (let ((links-of-node (find-link self node)))
            (if (not links-of-node)
                (push (list node nil (list linkee))
                      (slot-value self 'link-list))
                (if (not (member linkee (third links-of-node)))
                    (progn (setf (slot-value self 'link-list)
                                 (remove links-of-node (slot-value self
                                                              'link-list)))
                           (push linkee (third links-of-node))
                           (push links-of-node (slot-value self 'link-list)))))
            ))



