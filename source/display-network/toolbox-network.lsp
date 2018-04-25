;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                             toolbox-network.lisp                              
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1991 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     G. Desvignes 1988 - 1989
;;;     R.W. Oldford 1985 - 1991.
;;;
;;;
;;;--------------------------------------------------------------------------------

(in-package :quail)

;;;
;;; class definition
;;;


(defclass toolbox (find-where-mixin network-view tool-box-link-mixin)
       
  ((local-commands
    :initform '(#'sub-toolbox #'delete-from-browser 
                #'name-this-item)
    :allocation :class)
        
        ;; 
        ;; Definitions of the menus
        ;; 

   (left-button-items
    :initform
    '(("Name a Sub toolbox" #'name-this-item 
       "Give a name to this node (Only for Sub-toolboxes).")
      ("Summary" #'short-summary 
       "A description of this kind of item and its use."
       :sub-items
       (("Short summary" #'short-summary 
         "A short description of this kind of item and its use.")
        ("Long summary" #'long-summary 
         "A longer description of this kind of item and its use.")))
      ("References" #'references 
       "A list of references to sources in the literature on this item.")
      ("Internal Description" #'print-summary 
       "The internal description of this item.")
      ("Zoom" #'zoom "View this item in greater detail."))
    :accessor left-button-items-of
    :allocation :class)
   (middle-button-items
    :initform
    '(("Start Sub-Network from this node." #'sub-toolbox 
       "Create another toolbox network display beginning at the selected node."))
    :accessor middle-button-items-of
    :allocation :class)
   (left-title-items
    :initform
    '(("How this works" #'how-this-works 
       "A brief explanation of how this browser works.")
      ("Buttoning behavior" #'buttoning-behavior
       "How the mouse buttons work in this browser.")
      ("Network description" #'describe-contents
       "A description of the kinds of items in the toolbox network." )
      ("Name this toolbox" #'name-this-item 
       "Give a unique name to this toolbox."))
    :accessor left-title-items-of
    :allocation :class)
   (middle-title-items
    :initform
    '(("Widen" #'add-toolbox-node  "Add a new toolbox node."
       :sub-items
       (("Add a toolbox node" #'add-toolbox-node "Add a new toolbox node.")
        ("Add a node and its specializations" #'add-toolbox-tree 
         "Add a node and all its specializations.")))
      ("Narrow" #'narrow-view "Narrow the view"
       :sub-items
       (("Narrow the view" #'narrow-view 
         "Narrow the view by excluding the selected objects.")
        ("Create a View" #'create-view 
         "Compress part of the current view into a smaller toolbox.")))
      ("Links" nil ""
       :sub-items
       (("Break a link" #'break-link 
         "Break an existing link between two nodes in the toolbox.")
        ("Make a link" #'make-link 
         "Construct a directed link between two nodes in the toolbox.")))
      ("Recompute" #'recompute "Recompute."
       :sub-items
       (("Recompute" #'recompute "Recompute the graph.")
        ("Recompute labels" #'recompute-labels "Recompute the labels.")
        ("In Place" #'recompute-in-place
         "Recompute keeping current display in window.")
        ("Shape to hold" #'shape-to-hold 
         "Make window large or small enough to just hold graph.")
        ("Lattice/Tree" #'change-format 
         "Change format between lattice and tree")))
      ("Save value" #'save-it "Save this toolbox."))
    :accessor middle-title-items-of
    :allocation :class)
   
   ;; 
   ;; Definitions of the icons
   ;; 
   
   (icon :initform toolbox-icon :allocation :class)
   (mask :initform toolbox-icon-mask :allocation :class)
   (invert :initform nil :allocation :class)
   (title-reg :initform '(2 2 96 24)
              :allocation :class)
   (sub-view-icon :initform toolbox-map-icon :allocation :class)
   
   ;; 
   ;; Other variables
   ;;
   
   (graph-format :initform *horiz-tree*)
   (cache-menu-p :initform t)
   (title :initform "A toolbox network")))

;;;
;;; method definitions
;;;

(defmethod create-view ((self toolbox))
  "Creates a new view prompting for elements as necessary."

 (let ((the-new-view (make-instance (class-of self)))
       new-member)
      
      ;; 
      ;; Put the new view in the old one
      ;; 

      (widen-view self the-new-view)
      (browse the-new-view)
      
      ;; 
      ;; Add members to the new view and delete them from the old one. 
      ;; 

      (do nil
          ((not (setq new-member (prompt-read self "The new member : "))))
        
        ;; 
        ;; Add new node in Sub-toolbox
        ;; 

        (setq new-member (or ($! new-member t)
                             (find-class new-member)))
        (widen-view the-new-view new-member t)
        
        ;; 
        ;; Transporte the links of the new node inside the Sub-toolbox and
        ;; link the Sub-toolbox to the main toolbox
        ;; 

        (dolist (link (forward-links self new-member))
            (toolbox-link the-new-view link new-member)
            (toolbox-link self link the-new-view))
        (dolist (link (backward-links self new-member))
            (toolbox-link the-new-view new-member link)
            (toolbox-link self the-new-view link))
        (narrow-view self new-member t))  
      (recompute the-new-view)))



(defmethod backward-links ((self toolbox)
                           linked-object)
       

;;; 
;;;  Return the BacktoolboxLink of the Linked-object
;;; 

       (let ((links-of-nodes (find-link self linked-object)))
            (if links-of-nodes
                (third links-of-nodes)
                (error-cant-deduce-links-for self linked-object))))



(defmethod add-toolbox-tree ((self toolbox))
  "Build the tree of classes issued from the given node and add them in the ~
   toolbox."

       (let ((the-node-name (prompt-read self 
                                   "Name of the new root toolbox node : ")))
            (add-toolbox-sub-tree self (find-class the-node-name))
            (recompute self)))



(defmethod add-toolbox-sub-tree ((self toolbox)
                                 root-node)
  "This method adds the sub-classes of a node on the toolbox map. It is ~
   called iteratively."

       (dolist (link-name (class-direct-superclasses root-node))
           (when (in-view! self link-name)
                 (toolbox-link self root-node link-name)))
       (widen-view self root-node t)
       
       (let ((list-sub-nodes (class-direct-subclasses root-node)))
            (if list-sub-nodes
                (dolist (node list-sub-nodes)
                    (add-toolbox-sub-tree self node)))))



(defmethod narrow-view ((self toolbox)
                        &optional old-member dont-add-to-enveloping-views-flag
                        dont-recompute-flag)
       

;;; 
;;; Remove Old-member from this view. if DONT-ADD-TO-ENVELOPING-VIEWS-FLAG is
;;; T then do not add OLD-MEMBER to those view which envelop this one.
;;; 

       (if old-member
           (let (links-of-node)
                
                ;; 
                ;; Remove the links with the node to remove
                ;;
 
                (when (setq links-of-node (find-link self old-member))
                    (dolist (link (backward-links self old-member))
                        (remove-forward-toolbox-link self link old-member))
                    (dolist (link (forward-links self old-member))
                        (remove-back-toolbox-link self link old-member))
                    (setf (slot-value self 'link-list)
                          (remove links-of-node (slot-value self 'link-list))))
                
                ;; 
                ;; Remove old-Member from browser
                ;; 

                (setf (slot-value self 'starting-list)
                      (remove old-member (slot-value self 'starting-list)))
                
                ;; 
                ;; If DONT-ADD-TO-ENVELOPING-VIEWS-FLAG is NIL the Old-member
                ;; must be added in all views that enveloped self.
                ;; if Old-member is a view, some care must be taken to sort
                ;; out its enveloping-views as well.
                ;; 

                (if (typep old-member (class-name (class-of self)))
                    (unenvelop-by old-member self))
                (unless dont-add-to-enveloping-views-flag
                    (dolist (bigger-view (slot-value self 'enveloping-views))
                        (widen-view bigger-view old-member)))
                (unless dont-recompute-flag (recompute self)))
           (do nil
               ((not (setq old-member (prompt-read self 
                                      "The member to be removed from this view"
                                             ))))
             (setq old-member (or ($! old-member t)
                                  (find-class old-member)))
             (narrow-view self old-member dont-add-to-enveloping-views-flag 
                    dont-recompute-flag))))



(defmethod name-this-item ((self toolbox)
                           &optional object)
     "Specialization : method only available for sub toolboxes."

       (if (or (null object)
               (typep object 'network-view))
           (call-next-method)
           (prompt-print self "Can't rename a quail class.")))



(defmethod get-links ((self toolbox) object &key (reverse? NIL))
  "Returns a list of objects to which object should be linked in the display.  ~
   If reverse? is T then the direction is reversed, giving all the ~
   objects which are forward linked to object.  ~
   Specialization of NetworkView : Subtoolboxes are treated as normal nodes."
  
  (let (the-subs)
    (if reverse?
      
      ;; 
      ;; Get all of the linked-objects first.
      
      (dolist (item (backward-links self object))
        (when (in-view self item)
          (push item the-subs)))
      
      ;; 
      ;; Get all of the linked-objects first.
      
      (dolist (item (forward-links self object))
        (when (in-view self item)
          (push item the-subs))))
    the-subs))



(defmethod forward-links ((self toolbox) linked-object)
    "Returns  the toolbox links of the linked-object."

       (let ((links-of-node (find-link self linked-object)))
            (if links-of-node
                (second links-of-node)
                (error-cant-deduce-links-for self linked-object))))



(defmethod sub-toolbox-iteration ((self toolbox)
                                  new-toolbox node)
    "Add a node and its linked nodes shown on toolbox self in toolbox ~
     new-toolbox."

       (widen-view new-toolbox node t)
       (if (member node (slot-value self 'bad-list))
           (push node (slot-value new-toolbox 'bad-list)))
       (dolist (link (backward-links self node))
           (toolbox-link new-toolbox node link))
       (dolist (link (forward-links self node))
           (when (in-view self link)
                 (sub-toolbox-iteration self new-toolbox link))))



(defmethod sub-toolbox ((self toolbox)
                        obj &rest rest)
  "Create a new toolbox with every specialization of Obj visualized on the ~
   toolbox self."

       (let ((new-toolbox (make-instance (class-of self))))
            (browse new-toolbox)
            (sub-toolbox-iteration self new-toolbox obj)
            (recompute new-toolbox)))


(defmethod add-toolbox-node ((self toolbox))
  "Add a node to the toolbox map, set its required variables and link it to ~
   an existing node if desired."

       (let ((the-node-name (prompt-read self "Name of new toolbox node : "))
             link-name)
            (setq the-node-name (or ($! the-node-name t)
                                    (find-class the-node-name)))
            (do nil
                ((not (setq link-name (prompt-read self "Link to node : "))))
              (toolbox-link self the-node-name (or ($! link-name t)
                                                   (find-class link-name))))
            
            ;; 

            (widen-view self the-node-name)))



(defmethod widen-view ((self toolbox)
                       &optional new-member dont-recompute-flag)
       

;;; 
;;; add existing objects to this view
;;; 

       (if new-member
           (progn (unless (member new-member (slot-value self 'starting-list))
                      (progn (push new-member (slot-value self 'starting-list))
                             (if (not (find-link self new-member))
                                 (push (list new-member nil nil)
                                       (slot-value self 'link-list)))))
                  (if (typep new-member (class-name (class-of self)))
                      (envelop-by new-member self))
                  (unless dont-recompute-flag (recompute self)))
           (do nil
               ((not (and (setq new-member (prompt-read self 
                                                  "The new member : "))
                          (setq new-member (or ($! new-member t)
                                               (find-class new-member))))))
             (widen-view self new-member dont-recompute-flag))))



(defmethod unread ((self toolbox)
                   &optional object)
       

;;; 
;;; Push the name of the class of OBJECT in the buffer but if the object is an
;;; Analysis Path, push the object itself
;;; 

       (if (typep object 'network-view)
           (call-next-method)
           (push-in-buffer (class-name object))))

