;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               analysis-path.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1988-1991 Statistical Computing Laboratory, University of Waterloo
;;;
;;; 
;;;  Authors:
;;;     G. Desvignes 1988,1989
;;;     R.W. Oldford 1988-1992
;;;
;;;
;;;----------------------------------------------------------------------------------
;;;

(in-package :quail)

;;;
;;; class definitions
;;;

(defclass analysis-path (network-view)
       ((contents-description :initform 
               "Building an Analysis Path : Ask your quail Dealer how to do it."
               :allocation :class)
        (how-this-works :initform "Only practice can teach you that." 
               :allocation :class)
        (middle-button-items
         :initform
         '(("Show location in Path" #'show-location-in-path 
            "Flashes the node on the general browser."))
         :allocation :class)
        (left-title-items
         :initform
         '(("How this works" #'how-this-works 
            "A Brief explanation as to how this browser works.")
           ("Buttoning behavior" #'buttoning-behavior
            "How the mouse buttons work in this browser.")
           ("Building Analysis Path" #'describe-contents 
            "How to build an Analysis Path.")
           ("Name this Analysis Path" #'name-this-item
            "Give a Unique name to this ToolBox Map."))
               :allocation :class)
        (middle-title-items
         :initform
         '(("Widen" #'widen-view "Add a new node in the Analysis path."
            :sub-items
            (("Widen the Analysis Path" #'widen-view 
              "Add a new node in the Analysis path.")
             ("Add a parameter" #'add-parameter 
              "Add a new parameter to the Analysis path.")))
           ("Narrow" #'narrow-view "Removes a node from the Analysis path."
            :sub-items
            (("Narrow the Analysis path" #'narrow-view 
              "Removes a node from the Analysis path.")
             ("Create a View" #'create-view 
              "Order the Analysis Path to create a sub view when executed.")))
           ("Links" nil ""
            :sub-items
            (("Break a link" #'break-link 
              "Break an existing link between two nodes in the map.")
             ("Make a link" #'make-link
              "Build an directed Analysis link between two nodes in the Analysis path.")))
           ("Recompute" #'recompute "Recompute."
            :sub-items
            (("Recompute" #'recompute "Recompute the graph.")
             ("Recompute labels" #'recompute-labels "Recompute the labels.")
             ("In Place" #'recompute-in-place
              "Recompute keeping current display in window.")
             ("Shape to hold" #'shape-to-hold 
              "Make window large or small enough to just hold graph.")
             ("Lattice/Tree" #'change-format "Change format between lattice and tree")))
           ("Inspecting" nil ""
            :sub-items
            (("Browse all operations" #'global-browser 
              "Browse all requested operations in order to achieve the Analysis path.")
             ("Browse the parameters" #'parameter-browser 
              "Browse the parameters required by the Analysis path.")
             ("General causal browser" #'general-browser 
              "Browse the operations and the parameters using causal links.")
             ("General Analysis browser" #'general-analysis-browser
              "Browse the operations and the parameters using Analysis links.")
             ("Recompute" #'recompute "Recomputes the graph."))))
         :allocation :class)
        
        ;; 
        ;; Definitions of icons
        ;; 

        (icon :initform analysis-path-icon :allocation :class)
        (mask :initform analysis-path-icon-mask :allocation :class)
        (invert :initform nil :allocation :class)
        (title-reg :initform '(2 2 66 20)
               :allocation :class)
        (sub-view-icon :initform analysis-path-map-icon :allocation :class)
        
        ;; 
        ;; Other variables
        ;; 

        (list-of-elements :initform nil)
                                               ; All the nodes of the Analysis
                                               ; path (visible or not)
        (cache-menu-p :initform t)
        (title :initform "An Analysis Path")))



(defclass path-element (quail-object)
  ((title :initform nil
          :documentation
          "For Path Elements of views, name of the view.")
   (class-of-element :initform nil
                     :documentation
                     "Class of the node that can be constructed by this Path element.")
   (needed-arguments :initform nil
                     :documentation
                     "Arguments needed to compute the spawning expression.")
   (local-data :initform nil
               :documentation
               "IVs of the corresponding analysis node that have a
    corresponding path-element in the Analysis path.")
   ))


;;;
;;; function definitions
;;;


(defun tree-member (element tree)
  "Returns NIL unless element is a member of tree or of a sub-tree of tree."

   (or (member element tree)
       (dolist (sub-tree tree nil)
          (and (listp sub-tree)
               (tree-member element sub-tree)
               (return t)))))


(defun translate-parameter (list-to-translate path)
  "Translate a list of arguments to a list of corresponding path-elements."

       (let (translate result)
            (dolist (item list-to-translate)
                (push (cond ((not (object-p item)) item)
                            ((typep item 'network-view) path)
                            ((setf translate 
                                   (intersection 
                                      (slot-value path 'list-of-elements)
                                      (slot-value item 'list-of-path-elements)))
                             (first translate)))
                      result))
            (nreverse result)))


;;;
;;; method definitions
;;;


(defmethod update-links ((self path-element) from to)
   "Build the symetric links for a path-element."

       (dolist (link (slot-value self from))
           (unless (member self (slot-value link to))
               (push self (slot-value link to)))))



(defmethod replace-in-equivalence ((self path-element)
                                   value)
  "Replaces in equivalence table of the analysis-path the previous value ~
   equivalent to path-element (which is NIL if everything works OK) by value ~
   equivalence is a special variable defined in method apply-analysis-path."
  
  (declare (special equivalence))
  (nsubst (cons self value)
          (assoc self equivalence)
          equivalence)
  (dolist (item (slot-value self 'local-data))
    (replace-in-equivalence (second item)
                            (slot-value value (first item)))))



(defmethod remove-other-elements ((self path-element)
                                  path)
       
;;; 

       (setf (slot-value path 'list-of-elements)
             (remove self (slot-value path 'list-of-elements)))
       (dolist (link (slot-value self 'back-causal-links))
           (when (and (not (member link (slot-value path 'starting-list)))
                      (equal (slot-value link 'causal-links)
                             (list self)))
                 (remove-other-elements link path))))



(defmethod parent-nodes ((self quail-object))
  "Return the list of parent nodes of self i.e. the back-causal-links for the ~
   nodes which have some and the required inits for other ones."

       (or (slot-value self 'back-causal-links)
           (let (data list-data)
                (dolist (item (list-required-inits (class-of self)))
                    (when (object-p (setf data (slot-value self
                                                   (slot-value item
                                                          'name))))
                          (push data list-data)))
                list-data)))


(defmethod build-make-instance-expression ((self path-element) path template)
  "If a node has been built using add-analysis-node, we build a path-element ~
   with a make-instance spawning-expression and the required arguments as ~
   parameters."

(let (value result)
   (dolist (slot (list-required-inits (class-of template)))
       (setf value (slot-value template (slot-value slot 'name)))
       (push (first (translate-parameter (list value) path)) 
             result)
       (push (slot-initarg slot) result))
   (push (list 'quote (class-name (class-of template))) result)
   (push 'make-instance result)
   (list 'with-link result)))


(defmethod compute-spawning-expression ((self path-element) path)
  
  "Build a spawning-expression to create the node corresponding to a ~
   path-element when an analysis path is applied.  ~
   This method returns three values : the first one is T or NIL depending on the ~
   validity of the spawning-expression computed (value is T if some arguments ~
   are unknown in the spawning-expression); the second value is the ~
   spawning-expression if this one is correct NIL otherwise; the third is ~
   the result when the spawning expression is applied.  ~
   Special variables are defined in method apply-analysis-path."
  
  (let ((new-spawning-expression (copy-tree (slot-value self 'spawning-expression)))
        evaluable-expression)
    (if                           ;compute the new spawning expression
      (labels 
        ((repl (expression &aux the-arg)
           (declare (special list-of-elements equivalence))          
           (do ((arg expression (cdr arg)))
               ((null arg) t)
             (cond ((listp (setf the-arg (first arg)))
                    (unless (repl (first arg))
                      (return nil)))
                   ((object-p the-arg)
                    (cond ((member the-arg list-of-elements)
                           (unless (setf (first arg) 
                                         (cdr (assoc the-arg equivalence)))
                             (return nil)))
                          ((typep the-arg 'analysis-path)
                           (setf (first arg) 
                                 (cdr (assoc path equivalence)))
                           (t (quail-error "Unknown argument in expression to eval : ~s" the-arg)))))))))
        
        (repl new-spawning-expression))
      
      ;; 
      ;; The spawning-expression computed has the same form of that of the
      ;; template but is not directly evaluable. we save it to put in
      ;; spawning-expression because this is a simple form and we then
      ;; transform it in order to have an evaluable one
      ;; The link in an analysis path are  not updated by with-link but by the
      ;; method Apply-Analysis-path itself. So we just consider the rest of the
      ;; expression
      ;; 
      
      (progn (setf evaluable-expression (if (eql (first new-spawning-expression)
                                                 'with-link)
                                          (second new-spawning-expression)
                                          new-spawning-expression))
             
             ;; 
             ;; The transformation  to obtain an evaluable expression given a
             ;; simple spawning-expression is obtain by applying
             ;; Form-for-eval.
             ;; We can finally return the 3 values of the method
             ;; 
             
             (values nil new-spawning-expression (eval (format-for-eval 
                                                        evaluable-expression
                                                        ))))
      
      ;; 
      ;; Else, if some arguments are unknown
      ;; 
      
      (values t nil nil))))



(defmethod add-other-elements ((self quail-object)
                               path)
  "Add recursively the nodes which are needed to construct a given node self ~
   in the analysis-path.  ~
   We must first create the path-elements parents of some of the new-member ~
   if not already created otherwise just add them in the list-of-elements."

       (declare (special list-of-elements))
       (dolist (node (parent-nodes self))
             (let ((path-node (intersection 
                                    (slot-value node 'list-of-path-elements)
                                    list-of-elements)))
                (cond (path-node
                       (unless (member (first path-node)
                                       (slot-value path 'list-of-elements))
                               (push (first path-node)
                                     (slot-value path 'list-of-elements))))
                      ((parent-nodes node)
                       (add-other-elements node path))
                      (t (add-parameter path node)))))
       
       ;; 
       ;; Now we can create the Path-element for new-member
       ;; 

       (if (not (intersection (slot-value self 'list-of-path-elements)
                       list-of-elements))
           (let ((new-node (make-instance 'path-element)))
                (update-path-element new-node path self)
                (push new-node (slot-value path 'list-of-elements))
                (push new-node list-of-elements)
                (push new-node (slot-value self 'list-of-path-elements)))
           
           ;; 
           ;; or only update List-of-elements of the Analysis-path if the
           ;; new-member already exists as a path-element
           ;; 

           (let ((new-node (first (intersection (slot-value self 
                                                       'list-of-path-elements)
                                         list-of-elements))))
                (when new-node
                     (push new-node (slot-value path 'list-of-elements))))))



(defmethod add-parents-node ((self path-element)
                             the-new-view)
  "Copy the parents nodes of a new member in the list-of-elements of ~
   the-new-view.  This is used by create-view so that the new ~
   analysis-path created as a ~
   sub-view can be applied separately to its parent. In order to do that, it ~
   must contain every path-element needed to build the nodes selected by the ~
   user."
  
  (dolist (node (append (slot-value self 'back-causal-links)
                        (slot-value self 'needed-arguments)))
    (unless (or (null node)
                (typep node 'analysis-path)
                (member node (slot-value the-new-view 'list-of-elements)
                        ))
      (push node (slot-value the-new-view 'list-of-elements))
      (add-parents-node node the-new-view))))



(defmethod compute-list-of-elements ((self analysis-path))
  "Returns a list of all the Path-elements needed to complete an ~
   analysis-path, which is the collection of path-elements of the ~
   analysis-path and every enveloped analysis-path."
  
  (declare (special list-of-elements))
  (dolist (element (slot-value self 'list-of-elements))
    (when element
      (push element list-of-elements)))
  (dolist (sub-analysis-path (slot-value self 'enveloped-views))
    (compute-list-of-elements sub-analysis-path))
  list-of-elements)



(defmethod zoom ((self path-element))
  (describe self))



(defmethod update-path-element ((self path-element) path template)
  "Initializes the path element corresponding to the analysis node template ~
   in the analysis path."
  
  (setf (slot-value self 'spawning-expression)
        (if (tree-member 'add-analysis-node (slot-value template 'spawning-expression))
          (build-make-instance-expression self path template)
          (translate-parameter (slot-value template 'spawning-expression)
                               path)))
  
  ;; 
  
  (dolist (arg (slot-value self 'spawning-expression))
    (when (object-p arg)
      (push arg (slot-value self 'needed-arguments))))
  (dolist (arg (list-required-inits (class-of template)))
    (when (and (setf arg (slot-value template (slot-value arg 'name)))
               (typep (setf arg (car (translate-parameter (list arg)
                                                          path)))
                      'path-element)
               (not (member arg (slot-value self 'needed-arguments))))
      (push arg (slot-value self 'needed-arguments))))
  
  ;; 
  
  (setf (slot-value self 'class-of-element)
        (class-of template))
  
  ;; 
  
  (update-links-path-element self path template)
  
  ;; 
  
  (if (understands template 'list-subs t)
    (let (data)
      (dolist (item (list-subs template t))
        (when (and (object-p (setf data (car item)))
                   (object-p (setf data (car (translate-parameter
                                              (list data)
                                              path)))))
          (push (list (cdr item)
                      data)
                (slot-value self 'local-data))))))
  
  ;; 
  ;; Convert local data of parent Elements in Path-Elements
  ;; 
  
  (let (data)
    (dolist (node (slot-value template 'back-causal-links))
      (when (object-p node)
        (dolist (item (class-slots (class-of node)))
          (when (and (setf item (slot-value item 'name))
                     (eq (setf data (slot-value node item))
                         template))
            (push (list item self)
                  (slot-value (car (translate-parameter (list node)
                                                        path))
                              'local-data))))))))



(defmethod update-links-path-element ((self path-element)
                                      path template)
  "Update links of path-element by transposing links of template on it."
  
  (dolist (type-link '((analysis-links . back-analysis-links)
                       (back-analysis-links . analysis-links)
                       (causal-links . back-causal-links)
                       (back-causal-links . causal-links)))
    (setf (slot-value self (first type-link))
          nil)
    (dolist (link (translate-parameter (slot-value template (first
                                                             type-link))
                                       path))
      (when (object-p link)
        (push link (slot-value self (first type-link)))))
    (update-links self (first type-link)
                  (cdr type-link))))



(defmethod descriptive-label ((self path-element))
  ;;; 
  ;;; Specialization
  ;;; 
  (if (class-p (slot-value self 'class-of-element))
    (class-name (slot-value self 'class-of-element))
    (slot-value self 'title)))



(defmethod widen-view ((self analysis-path) &optional
                       (new-member nil) (dont-recompute-flg nil))
  
  ;;; Add existing Objects to this View
  ;;; 
  
  (declare (special list-of-elements))
  (let (list-member)
    (cond ((and new-member (listp new-member))
           (setf list-member new-member))
          (new-member (setf list-member (list new-member)))
          (t (do nil
                 ((not (setf new-member ($! (prompt-read self 
                                                         "The new member : ")))))
               (push new-member list-member))))
    
    ;; 
    ;; List-member is a list of new members to add in the Analysis path
    ;; 
    
    ;;
    ;; first add the parameters in the path if some are selected by user
    ;;
    
    (dolist (member list-member)
      (when (and (not (eq (class-of member)
                          (find-class 'path-element)))
                 (member 'add-root (slot-value member 
                                               'spawning-expression)))
        (add-parameter self member t)))
    
    ;;
    ;; then add the other members
    ;;
    
    (dolist (member list-member)
      (let (new-node)
        
        ;; New-node is the associated Path-Element of a given member
        (cond 
         ;; 
         ;; If Member is a Path element, that means that we want
         ;; an invisible path-element of the path to become
         ;; visible so  New-node = Member 
         ;; If Member is an Analysis path, we don't build a new
         ;; Path-element, we just add it in the starting-list.
         ;; New-node = Member
         ;; 
         
         ((member (class-of member)
                  (list (find-class 'path-element)
                        (class-of self)))
          (setf new-node member))
         
         ;; 
         ;; If Member is a view, we add all its elements in a
         ;; sub-path and the sub-path in the AnalysisPath
         ;; 
         
         ((typep member 'network-view)
          (let ((sub-path (make-instance 'analysis-path)))
            (zoom sub-path)
            (widen-view sub-path (slot-value member 'starting-list))
            (widen-view self sub-path)))
         
         ;; 
         ;; If the selected node has been built by an Analysis
         ;; path, New-node is the Path-Element used to build it.
         ;; We only must update this Path-Element in case the
         ;; links have been changed
         ;; 
         
         ((setf new-node (first (intersection (slot-value
                                               member
                                               
                                               '
                                               list-of-path-elements
                                               )
                                              (slot-value self
                                                          'list-of-elements))))
          (update-links-path-element new-node self member))
         
         ;; 
         ;; Otherwise we must create and update a new Path element
         ;; 
         
         (t (setf list-of-elements nil)
            (compute-list-of-elements self)
            (add-other-elements member self)
            (setf new-node (first (intersection (slot-value
                                                 member
                                                 
                                                 '
                                                 list-of-path-elements
                                                 )
                                                (slot-value self
                                                            'list-of-elements))
                                  ))))
        
        ;; 
        ;; Now we have New-node. We just have to add it in the starting
        ;; list and in list-of-elements
        ;; 
        
        (when new-node
          (unless (member new-node (slot-value self 'list-of-elements))
            (push new-node (slot-value self 'list-of-elements)))
          (unless (member new-node (slot-value self 'starting-list))
            (push new-node (slot-value self 'starting-list)))
          (if (eq (class-of new-node)
                  (class-of self))
            (envelop-by new-node self)))
        
        ;; 
        ;; Recompute if necessary
        ;; 
        
        (unless dont-recompute-flg (recompute self))))))



(defmethod show-location-in-path ((self analysis-path)
                                  object)
  "Flashes the selected node on the general browser of the analysis path."

       (general-browser self)
       (flash-node self object))



(defmethod print-summary ((self analysis-path))
       (print-summary (class-of self)))



(defmethod narrow-view ((self analysis-path)
                        &optional old-member dont-add-to-enveloping-views-flag
                        dont-recompute-flag)
  
  
  ;;; 
  ;;; Remove OldMember from this view. If Dont-Add-To-Enveloping-Views-Flag is T
  ;;; then do not add OldMember to those views which envelop this one
  ;;; 
  
  (call-next-method)
  
  ;; 
  ;; Updating of List-of-elements :
  ;; Old-member and its parents are removed from List-of-Elements if they
  ;; are not necessary for building other nodes of the Analysis-Path
  ;; 
  
  (if (and old-member (typep old-member 'path-element))
    (if (slot-exists-p old-member 'causal-links)
      (unless (slot-value old-member 'causal-links)
        (remove-other-elements old-member self))
      (error-cant-deduce-links-for old-member))
    (setf (slot-value self 'list-of-elements)
          (remove old-member (slot-value self 'list-of-elements)))))



(defmethod make-link ((self analysis-path))
  ;;; 
  ;;; Make a link from node-1 to node-2
  ;;; 
  
  (let ((node-1 ($! (prompt-read self "Node to link from :")))
        node-2)
    (if (not (typep node-1 'path-element))
      (prompt-print self 
                    "You can only make links between elements of the Analysis-path"
                    )
      (progn (setf node-2 ($! (prompt-read self 
                                           "Node to break the link to : "))
                   )
             (if (not (typep node-2 'path-element))
               (prompt-print self 
                             "You can only make links between elements of the Analysis-path"
                             )
               (when (and node-1 node-2)
                 (analysis-link node-1 node-2)
                 (recompute self)))))))



(defmethod parameter-browser ((self analysis-path))
  ;;; 
  ;;; Browse the parameters of the analysis path
  ;;; 
  
  (let ((old-starting-list (slot-value self 'starting-list))
        new-starting-list)
    (dolist (item (slot-value self 'list-of-elements))
      (unless (class-p (slot-value item 'class-of-element))
        (push item new-starting-list)))
    (setf (slot-value self 'starting-list)
          new-starting-list)
    (recompute self)
    (setf (slot-value self 'starting-list)
          old-starting-list)))



(defmethod global-browser ((self analysis-path))
"Browse all the operations to apply to complete the analysis path (the ~
 parameters are not browsed -- used links are causal links)."

       (declare (special *causal-link-flg*))
       (let ((old-starting-list (slot-value self 'starting-list))
             new-starting-list)
            (dolist (item (slot-value self 'list-of-elements))
                (when (class-p (slot-value item 'class-of-element))
                      (push item new-starting-list)))
            (setf (slot-value self 'starting-list)
                  new-starting-list)
            (setf *causal-link-flg* t)
            (recompute self)
            (makunbound '*causal-link-flg*)
            (setf (slot-value self 'starting-list)
                  old-starting-list)))




(defmethod general-browser ((self analysis-path))
    "Browse all the nodes constructed to complete the analysis-path, even those ~
     which will not be viewed when the analysis path is applied.  ~
     Used links are the causal links."

       (declare (special *causal-link-flg*))
       (let ((old-starting-list (slot-value self 'starting-list)))
            (setf (slot-value self 'starting-list)
                  (copy-list (slot-value self 'list-of-elements)))
            (setf *causal-link-flg* t)
            (recompute self)
            (makunbound '*causal-link-flg*)
            (setf (slot-value self 'starting-list)
                  old-starting-list)))



(defmethod general-analysis-browser ((self analysis-path))
    "Browse all the nodes constructed to complete the analysis path. Even those ~
     which will not be viewed when the analysis path is applied.  ~
     Links used are the analysis links."

       (let ((old-starting-list (slot-value self 'starting-list)))
            (setf (slot-value self 'starting-list)
                  (copy-list (slot-value self 'list-of-elements)))
            (recompute self)
            (setf (slot-value self 'starting-list)
                  old-starting-list)))



(defmethod forward-links ((self analysis-path) linked-object)
  "Returns the analysis-links or causal-links of the linked-object depending ~
   on the value of *causal-link-flg*."
  
  (declare (special *causal-link-flg*))
  (if (boundp '*causal-link-flg*)
    (if (slot-exists-p linked-object 'causal-links)
      (slot-value linked-object 'causal-links)
      (error-cant-deduce-links-for linked-object))
    (if (slot-exists-p linked-object 'analysis-links)
      (slot-value linked-object 'analysis-links)
      (error-cant-deduce-links-for linked-object))))



(defmethod create-view ((self analysis-path))
  "Creates a view inside the analysis path."
  
  (let ((the-new-view (make-instance (class-of self)))
        new-member)
    
    ;; 
    ;; add the new view in SELF
    ;; 
    
    (widen-view self the-new-view)
    (zoom the-new-view)
    
    ;; 
    ;; Add members to the new view and delete them from SELF. The user
    ;; is prompt for New-member in the promptWindow
    ;; 
    
    (do nil
        ((not (setf new-member ($! (prompt-read self 
                                                "The new member : ")))))
      (widen-view the-new-view new-member t)
      (add-parents-node new-member the-new-view)
      (narrow-view self new-member t))
    (recompute the-new-view)))



(defmethod break-link ((self analysis-path))
       

;;; 
;;; Break a link between two nodes
;;; 

       (let ((node-1 ($! (prompt-read self "Node to break the link from : ")))
             node-2)
            (if (not (typep node-1 'path-element))
                (prompt-print self 
     "You can only manipulate the links between Elements of the Analysis path."
                       )
                (progn (setf node-2 ($! (prompt-read self 
                                               "Node to break the link to : "))
                             )
                       (if (not (typep node-2 'path-element))
                           (prompt-print self 
     "You can only manipulate the links between Elements of the Analysis path."
                                  )
                           (if (and node-1 node-2)
                               (progn (analysis-unlink node-1 node-2)
                                      (recompute self))))))))



(defmethod backward-links ((self analysis-path)
                           linked-object)
       

;;; 
;;; Returns the back-analysis-links or Back-causal-links of the linked object
;;; depending on the value of *CAUSAL-LINK-FLG*
;;; 

       (declare (special *causal-link-flg*))
       (if (boundp '*causal-link-flg*)
           (if (slot-exists-p linked-object 'back-causal-links)
               (slot-value linked-object 'back-causal-links)
               (error-cant-deduce-links-for linked-object))
           (if (slot-exists-p linked-object 'back-analysis-links)
               (slot-value linked-object 'back-analysis-links)
               (error-cant-deduce-links-for linked-object))))



(defmethod add-parameter ((self analysis-path)
                          &optional new-parameter visible-in-path)
  "Prompt and update the analysis path for a new parameter to the path."
  
  (if new-parameter
    (prog (new-node)
      (if (typep new-parameter 'path-element)
        (setf new-node new-parameter)  
        (if (setf new-node (intersection (slot-value self 
                                                     'list-of-elements)
                                         (slot-value new-parameter
                                                     'list-of-path-elements)))
          (setf new-node (first new-node))
          (progn (setf new-node (make-instance 'path-element))
                 (setf (slot-value new-node 'class-of-element)
                       "PARAMETER")
                 (push new-node (slot-value self 'list-of-elements))
                 (push new-node (slot-value new-parameter
                                            'list-of-path-elements))
                 (when visible-in-path
                   (push new-node (slot-value self 'starting-list))))))
      
      ;; 
      (unless (equal (slot-value new-node 'class-of-element)
                     "PARAMETER")
        (setf (slot-value new-node 'class-of-element)
              "PARAMETER")
        (dolist (parent (slot-value new-node 'needed-arguments))
          (remove-other-elements parent self))) 
      (setf (slot-value new-node 'title)
            (prompt-read self
                         (format nil "Name of ~A in the Analysis path : "
                                 (or (get-name new-parameter)
                                     (string (class-name (class-of 
                                                          new-parameter
                                                          ))))))))
    
    ;; 
    
    (do nil
        ((not (setf new-parameter ($! (prompt-read self 
                                                   "The new parameter : ")))))
      (add-parameter self new-parameter))))



(defmethod apply-analysis-path ((self analysis-map) &optional (template nil))
  "Apply a user defined Analysis path : The definition is made by building an ~
   instance of Analysis-path which contains all the operations to execute ~
   (Path elements). See Create-Analysis-Path."

 (declare (special list-of-elements equivalence))
 
 ;; 
 ;; ---------------------------  VARIABLES DEFINITIONS --------------------------
 ;;
 ;; TEMPLATE : Name of the reference Analysis-Path
 ;; 

 (unless template
     (setf template ($! (prompt-read self 
                               "Name of the Analysis Path to apply : "))))
 
 ;; 
 ;; LIST-OF-ELEMENTS : All the Path-Elements and Sub-Analysis-Path necessary
 ;; to complete the Analysis-Path
 ;; 

 (setf list-of-elements (list template))
 (compute-list-of-elements template)
 (setf list-of-elements (delete-duplicates list-of-elements))
 
 ;; 
 ;; THE-NEW-VIEW : The result of the Analysis-path is put in a new AnalysisMap
 ;; included in the main one. Its name is THE-NEW-VIEW

 (let
  ((the-new-view (make-instance 'analysis-map))
   (nb-functions-in-previous-loop 0)
   sub-nodes list-functions)
  
  ;; 
  ;; EQUIVALENCE : List of equivalence of Data. There are 4 types of data : 
  ;; 1 - Parameters : Path-elements with slot Class-of-element bound to
  ;; "PARAMETER" . Their equivalent value is given by the user when he applies
  ;; the Analysis path
  ;; 2 -  Local-Variables : Data which doesn't have to be built because they
  ;; are created simultaneously with an enveloping object and will be built
  ;; when the enveloping object is created by a regular path-element 
  ;; 3 - Analysis-Path : Element corresponding to an analysis-path applied
  ;; inside another analysis path. The result of this sub-analysis-path is put
  ;; in a nested analysis-map when applied
  ;; 4 - Path-Elements : The regular path-element correspond to the building
  ;; of a node
  ;; 

  (setf equivalence (list (cons template the-new-view)))
  (dolist (node list-of-elements)
      (unless (eq node template)
          (push (cons node nil)
                equivalence)))
  
  ;; 
  ;; SUB-NODES : List of nodes which are generated at creation of other nodes
  ;; so they don't have to be computed, only extracted from their parent (They
  ;; correspond to what we called local-variables in EQUIVALENCE)
  ;; 

  (let (list-init)
       (dolist (node list-of-elements)
           (when (typep node 'path-element)
               (if (understands (slot-value node 'class-of-element)
                          'list-required-inits)
                   (setf list-init (list-required-inits (slot-value
                                                         node
                                                         'class-of-element)))
                   (setf list-init nil))
               (dolist (item (slot-value node 'local-data))
                   (unless (or (member (first item)
                                      list-init)
                               (member (first item)
                                      sub-nodes))
                       (push (second item)
                             sub-nodes))))))
  
  ;; 
  ;; LIST-FUNCTIONS : List of functions to apply to complete the Analysis path
  ;; . This list is shortened every time a new node is built. WHEN NIL that
  ;; means that the Analysis path is completed
  ;; 

  (dolist (node list-of-elements)
      (unless (or (eq node template)
                  (member node sub-nodes))
             (push node list-functions)))
  
  ;; 
  ;; --------------------------   END OF VARIABLE DEFINITIONS -------------------
  ;; 

  (widen-view self the-new-view)
  (zoom the-new-view)
  
  ;; 
  ;; Ask for the values of parameters and update the Equivalence Table
  ;; 

  (dolist (node list-of-elements)
      (when (and (typep node 'path-element)
                 (not (class-p (slot-value node 'class-of-element))))
          (let ((answer ($! (prompt-read self (format nil 
                                                 "Parameter equivalent to ~S :"
                                                     (slot-value node
                                                            'title))))))
               (replace-in-equivalence node answer)
               (setf list-functions (remove node list-functions)))))
  
  ;; 
  ;; Beginning of the method
  ;; 

  (do nil
      ((null list-functions))
    (if (eql nb-functions-in-previous-loop (setf nb-functions-in-previous-loop
                                                 (length list-functions)))
        (quail-error "Infinite Loop in evaluation of Analysis Path"))
    (dolist (funct list-functions)
        (let (expression-to-eval unknown-arg new-node)
             
             ;; 
             ;; We first compute and eval the spawning expression if it is
             ;; correct.
             ;; 3 values are returned by the method : 
             ;;  - T if there are some unknown arguments in the expression to
             ;; eval NIL otherwise for the first value
             ;;  - The Expression to eval to obtain the result ( NIL if some
             ;; arguments are unknown)
             ;;  - The result of the evaluation of this expression (NIL if
             ;; some arguments are unknown)
             ;; 
             

;;; 
;;; The Computed spawning expression for a node created by Add Analysis Node
;;; should be an expression creating a new instance with initializations. This
;;; isn't programmed in the current version because the initarg option in
;;; defclass is not working correctly. So, if the user creates an Analysis
;;; path containing elements created by Add Analysis Node, he will have to
;;; give informations related to initializations which are theorically not
;;; required .
;;; When Initarg is working, we will have to modify Update Path Element so
;;; that the generated spawning expression and the required arguments are OK
;;; 

             (multiple-value-setf (unknown-arg expression-to-eval new-node)
                    (compute-spawning-expression funct template))
             
             ;; 
             ;; If EXPRESSION-TO-EVAL is correct (all required arguments are
             ;; known) then the  work variables are updated
             ;; 

             (unless unknown-arg
                 (if new-node
                     (setf (slot-value new-node 'spawning-expression)
                           expression-to-eval))
                 (setf list-functions (remove funct list-functions))
                 (replace-in-equivalence funct new-node)))))
  
  ;; 
  ;; When every node of the Analysis-path is built, we update their links and
  ;; add them in the views they must be in
  ;; 

  (dolist (node equivalence)
      (let ((new-node (cdr node))
            node-link)
           (when new-node
                 (when (slot-exists-p new-node 'list-of-path-elements)
                       (setf (slot-value new-node 'list-of-path-elements)
                             (remove (first node)
                                     (slot-value new-node 
                                            'list-of-path-elements)))
                       (push (first node)
                             (slot-value new-node 'list-of-path-elements)))
                 (when (typep (first node)
                              'path-element)
                       (dolist (link (slot-value (first node)
                                            'back-analysis-links))
                          (if (setf node-link (cdr (assoc link equivalence)))
                              (analysis-link node-link new-node)))
                       (dolist (link (slot-value (first node) 'analysis-links))
                          (if (setf node-link (cdr (assoc link equivalence)))
                              (analysis-link new-node node-link)))
                       (dolist (link (slot-value (first node) 
                                            'back-causal-links))
                          (if (setf node-link(cdr (assoc link equivalence)))
                              (causal-link node-link new-node)))
                       (dolist (link (slot-value (first node) 'causal-links))
                          (if (setf node-link (cdr (assoc link equivalence)))
                              (causal-link new-node node-link)))))))
  
  ;; 
  ;; we then add the nodes in the analysis-map they must appear in
  ;; 

  (dolist (view equivalence)
      (when (typep (first view)
                   'analysis-path)
          (dolist (node (slot-value (first view) 'starting-list))
              (let ((new-node (cdr (assoc node equivalence))))
                 (unless (member new-node (slot-value self 'starting-list))
                         (widen-view (cdr view) new-node))))))
  (recompute self)))



(defmethod compute-spawning-expression ((self analysis-path)
                                        path)
  "When applying an Analysis path, if the function to apply is an analysis ~
   path, we must create a view.  ~
   The method returns 3 values as it is the case for the corresponding method ~
   for a path element. The First one NIL indicates that all the parameters ~
   are known in the spawning expression and this one can be applied. The ~
   second value is the spawning expression which will be stored in the ~
   correct slot of the result and the THIRD value is the result after the ~
   spawning expression is applied."
  
  (values nil '(zoom (make-instance 'analysis-map))
          (let ((result (make-instance 'analysis-map)))
            (zoom result)
            result)))



(defmethod replace-in-equivalence ((self analysis-path)
                                   value)
  "Replaces in Equivalence Table of the Analysis-Path the previous value ~
   equivalent to SELF (which is NIL if everything works OK) by value.  ~
   Equivalence is a special variable defined in method Apply-Analysis-Path."
  
  (declare (special equivalence))
  (nsubst (cons self value)
          (assoc self equivalence)
          equivalence))


(defmethod after-init ((self path-element))
  ;;The default method is updating the spawning expression but this is not 
  ;;  necessary for a PathElement because the spawning expression we need
  ;; doesn't represent how the path-element was built, but how the
  ;; analysis-node represented by the path element was built. This is defined
  ;; in method Update-path-element
  ;; 
  
  self)



(defmethod add-parents-node ((self analysis-path)
                             the-new-view)
  ;;; If a new element is an Analysis Path, we don't have to create any new
  ;;; element
  ;;; So the method does nothing
  ;;; 
  self)

