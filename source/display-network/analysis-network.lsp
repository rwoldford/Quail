;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               analysis-network.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1988-1991 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     G. Desvignes 1988,1989
;;;     R.W. Oldford 1988 - 1992.
;;;
;;;
;;;----------------------------------------------------------------------------------
;;;
(in-package :quail)

;;;
;;; class definition
;;;

(defclass analysis-map (body-menu network-view)
       ((contents-description
         :initform
         "If you have time, you should type in a description of the content of an Analysis Map"
         :allocation :class)
        (how-this-works
         :initform "As you will notice, it works pretty well." 
         :allocation :class)
        
        ;; 
        ;; Menu items for Left button selection. Value sent as message to
        ;; object or browser .. See LocalCommands
        ;; 

        (left-button-items 
         :initform 
         '(("Name this item" #'name-this-item "Give a name to this item.")
           ("Edit Notes" #'add-notes "Edit the notes attached to this item.")
           ("A short summary" #'short-summary 
            "A short description of the kind of item selected.")
           ("Zoom" #'zoom "View this item in greater detail."))
         :accessor left-button-items-of
         :allocation :class)
        (left-title-items 
         :initform 
         '(("How this works" #'how-this-works 
            "A brief explanation as to how this kind of view works.")
           ("Buttoning Behavior" #'buttoning-behavior
            "How the mouse buttons work in this kind of View.")
           ("Building analysis" #'describe-contents
            "How to build analysis in this display.")
           ("Name this Analysis Map" #'name-this-item
            "Give a unique name to this Analysis Map."))
         :accessor left-title-items-of
         :allocation :class)
        (middle-title-items
         :initform 
         '(("Widen" #'widen-view "Widen the View."
            :sub-items
            (("Widen the View" #'widen-view 
              "Widen the view by including existing objects.")
             ("Add a new kind of Analysis node" #'add-analysis-node 
              "Add a new kind of node to the Analysis Map ~
               (usually by taking one from a ToolBox).")
             ("Add result of a form evaluation" #'add-root 
              "Prompt for a form to be evaluated; ~
               the result is added to this Analysis Map.")))
           ("Narrow" #'narrow-view "Narrow the view."
            :sub-items 
            (("Narrow the View" #'narrow-view 
              "Narrow the view by excluding the selected Objects.")
             ("Create a View" #'create-view 
              "Compress part of the current view into a smaller Analysis Map.")))
           ("Nesting" nil ""
            :sub-items
            (("Explode a nested Analysis Map" #'explode-a-nested-view
              "Explode an Analysis map nested in this display.")
             ("Return an exploded Analysis Map" #'return-an-exploded-view
              "Return to the display an Analysis Map that was exploded.")))
           ("Links" nil ""
            :sub-items
            (("Break a Link" #'break-link 
              "Break an existing link between two nodes in he map.")
             ("Make a Link" #'make-link
              "Construct a directed Analysis link between two nodes in the Map.")
             ("Insert a Memo between two nodes" #'interpose-memo 
              "Interpose a Memo Object between two nodes in the Map.")))
           ("Create an Analysis Path" #'create-analysis-path 
            "Create a new Analysis Path")
           ("Recompute" #'recompute "Recompute."
            :sub-items
            (("Recompute" #'recompute "Recompute the graph." )
             ("Recompute labels" #'recompute-labels "Recompute the labels.")
             ("In Place" #'recompute-in-place
              "Recompute keeping current display in window.")
             ("Shape to hold" #'shape-to-hold 
              "Make window large or small enough to just hold graph.")
             ("Lattice/Tree"  #'change-format
              "Change format between lattice and tree")))
           ("Save Value" #'save-it "Save this Analysis Map"))
         :allocation :class
         :accessor middle-title-items-of)
        (icon :initform map-icon :allocation :class)
        (mask :initform map-icon-mask :allocation :class)
        (invert :initform nil :allocation :class)
        (title-reg :initform '(2 2 46 21)
               :allocation :class)
        (sub-view-icon :initform analysis-map-icon :allocation :class)
        (title :initform "An Analysis Map" :accessor title-of)))

;;;
;;; method definitions
;;;

(defmethod make-link ((self analysis-map))
       

;;; 
;;;  Link from Node-1 to Node-2
;;; 

       (let ((node-1 ($! (prompt-read self "Node to link from :")))
             node-2)
            (if (and node-1 (eq (find-class 'analysis-map)
                                (class-of node-1)))
                (prompt-print self (format nil "Sorry, there are no Analysis links from an Analysis Map. Instead, you must explode ~S to manipulate the Analysis links."
                                          (or (slot-value node-1 'name)
                                              node-1)))
                (progn (setq node-2 ($! (prompt-read self 
                                               "Node to break the link to : "))
                             )
                       (if (and node-2 (eq (find-class 'analysis-map)
                                           (class-of node-2)))
                           (prompt-print self (format
                                               nil "Sorry, there are no Analysis links from an Analysis Map. Instead, you must explode ~S to manipulate the Analysis links."
                                               (or (slot-value node-2
                                                          'name)
                                                   node-2)))
                           (if (and node-1 node-2)
                               (progn (analysis-link node-1 node-2)
                                      (recompute self))))))))



(defmethod break-link ((self analysis-map))
       

;;; 
;;; Break a link between two nodes
;;; 

       (let ((node-1 ($! (prompt-read self "Node to break the link from : ")))
             node-2)
            (if (and node-1 (eq (find-class 'analysis-map)
                                (class-of node-1)))
                (prompt-print self (format nil "Sorry, there are no Analysis links from an Analysis Map. Instead, you must explode ~S to manipulate the Analysis links."
                                          (or (slot-value node-1 'name)
                                              node-1)))
                (progn (setq node-2 ($! (prompt-read self 
                                               "Node to break the link to : "))
                             )
                       (if (and node-2 (eq (find-class 'analysis-map)
                                           (class-of node-2)))
                           (prompt-print self (format
                                               nil "Sorry, there are no Analysis links from an Analysis Map. Instead, you must explode ~S to manipulate the Analysis links."
                                               (or (slot-value node-2 'name)
                                                   node-2)))
                           (if (and node-1 node-2)
                               (progn (analysis-unlink node-1 node-2)
                                      (recompute self))))))))



(defmethod forward-links ((self analysis-map)
                          linked-object)
       

;;; 
;;; Returns the Analysis-Links of the Linked-Object
;;; 

       (if (slot-exists-p linked-object 'analysis-links)
           (slot-value linked-object 'analysis-links)
           (error-cant-deduce-links-for linked-object)))


(defmethod backward-links ((self analysis-map)
                           linked-object)
       

;;; 
;;; Returns the Back-Analysis-Links of the Linked-Object
;;; 

       (if (slot-exists-p linked-object 'back-analysis-links)
           (slot-value linked-object 'back-analysis-links)
           (error-cant-deduce-links-for linked-object)))



(defmethod interpose-memo ((self analysis-map))
  "Place a memo object between two quail objects."
  
  (let ((node-1 ($! (prompt-read self "Node to break link from ")))
        node-2 memo-node)
    (if (eq (class-of node-1)
            (find-class 'analysis-map))
      (progn (prompt-print self 
                           "Can't, Memos can only be inserted between two quail-objects."
                           )
             (prompt-print self (format nil 
                                        "Explode the Analysis Map ~S and insert ~
                                         the Memo accordingly."
                                        node-1)))
      (progn (setq node-2 ($! (prompt-read self 
                                           "Node to break link to ")))
             (if (eq (class-of node-2)
                     (find-class 'analysis-map))
               (progn (prompt-print self 
                                    "Can't, Memos can only be inserted between~
                                     two quail-objects."
                                    )
                      (prompt-print self (format nil 
                                                 "Explode the Analysis Map ~S and~
                                                  insert the Memo accordingly."
                                                 node-2)))
               (if (and node-1 node-2)
                 (progn (setq memo-node (make-instance
                                         'memo))
                        (analysis-unlink node-1 node-2)
                        (analysis-link node-1 memo-node)
                        (analysis-link memo-node node-2)
                        (widen-view self memo-node))))))))



(defmethod add-analysis-node ((self analysis-map) new-node)
  "Add a new kind of node (usually selecting it in a Toolbox). If the node ~
   corresponds to a class, add-instance-node is applied otherwise method ~
   apply-analysis-path is applied."
  
  (if new-node
    (if (eq (find-class 'analysis-path)
            (class-of ($! new-node t)))
      (apply-analysis-path self ($! new-node))
      (add-instance-node self new-node))))



(defmethod add-instance-node ((self analysis-map)
                              new-node)
    "Add an object to an analysis map. The user is asked for the analysis links ~
     but the causal links are updated automatically."

       (let ((the-node-object (make-instance new-node))
             link-name)
            (do nil
                ((not (setq link-name ($! (prompt-read self "Link to node : "))
                            )))
              (analysis-link link-name the-node-object))
            (widen-view self the-node-object)
            the-node-object))



(defmethod extraction ((self analysis-map)
                       object slot-to-extract)
    "Add a new object on the Map which is taken among the instance variables of ~
     the parent-object selected on the analysis-Map.  ~
     We first link the new object with his parent and then add it in the view."

       (let ((object-to-extract (slot-value object slot-to-extract)))
            (analysis-link object object-to-extract)
            (widen-view self object-to-extract)))



(defmethod get-class-menu-items ((self analysis-map) item-cv)
  "Build the part of menu related to extraction."
  
  (declare (special object))
  (let ((the-item-list (call-next-method))
        extraction-list quail-object-slots slot-name)
    
    ;; compute the list of slots of quail-object
    
    (dolist (item (class-slots (find-class 'quail-object)))
      (push (slot-value item 'name)
            quail-object-slots))
    
    ;; the variables you can extract are those which are not inherited from
    ;; quail-object and bound to an object
    
    (dolist (item (class-slots (class-of object)))
      (setq slot-name (slot-value item 'name))
      (when (and (typep (slot-value object slot-name) 'object)
                 (not (member slot-name quail-object-slots)))
        (push (list slot-name
                    (list 'extraction (list 'quote slot-name))
                    "Release button to extract the selected object.")
              extraction-list)))
    (when extraction-list
      (cons :sub-items (list extraction-list))
      (setq the-item-list
            (append the-item-list
                    (list (list "Extraction" nil 
                                "Slide over to see the objects you can extract."
                                extraction-list)))))
    the-item-list))


(defmethod create-analysis-path ((self analysis-map))
  "Create and zoom a new analysis-path."

       (let ((new-path (make-instance 'analysis-path)))
            (zoom new-path)
            (widen-view new-path)
            new-path))