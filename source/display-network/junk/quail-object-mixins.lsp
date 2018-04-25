;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                             quail-object-mixins.lisp                              
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
;;; Classes definitions
;;;


(defclass fast-dated-object ()
       ((created :initform (get-universal-time))
                                               ; Internal format time of
                                               ; creation of object
        (creator :initform (get-user-name))
                                               ; Username of creator of object
        ))


(defclass initable-object-mixin ()
       ((spawning-expression :initform nil)))


(defclass body-menu-mixin ()
       
;;; 
;;; This mixin is here only to replace the methods LEFT-CHOICE and
;;; MIDDLE-CHOICE of BROWSER in quail-BROWSER 
;;; 

       ((middle-button-items :initform nil :allocation :class)
        
        ;; 
        ;; item to be done if middleButton is selected in main Window
        ;; All the methods concerning MIDDLE-BUTTON-ITEMS are in
        ;; BODY-MENU-MIXIN
        ;; 
        (generic-middle-menus :initform nil :allocation :class)
        
        ;; offer up menus specific to the class of an item which has been
        ;; (middle) buttoned . A single ClassVariable Generic-Middle-Menus
        ;; caches as an association list the menus constructed for the various
        ;; classes.
        ))


(defclass documented-object-mixin ()
       ((long-summary :initform "No summary is available on this item" 
               :allocation :class)
        (reference :initform "No references are available for this item" 
               :allocation :class)
        (short-summary :initform "No summary is available on this item" 
               :allocation :class)
        (glossary :initform "No glossary is available for this item" 
               :allocation :class)))



(defclass editable-object-mixin (fast-dated-object initable-object-mixin)
       ((notes :initform nil                   ; (CONCATENATE 'STRING "Object
                                               ; " (OR (SLOT-VALUE SELF 'NAME)
                                               ; "") " created by "
                                               ; (SLOT-VALUE SELF 'CREATOR) "
                                               ; " (IL:GDATE (SLOT-VALUE SELF
                                               ; 'CREATED)) " using " (CAR
                                               ; (SLOT-VALUE SELF
                                               ; 'SPAWNING-EXPRESSION)))
               )))


(defclass find-where-mixin () nil)


(defclass indexed-object-mixin ()
       
       ;; Adds two methods inherited-Methods-Classification and
       ;; Local-Methods-Classification which produce indexes to the methods
       ;; available in this class
       nil)


(defclass linked-object-mixin ()
       ((analysis-links :initform nil)
                                               ; Forward pointer list for the
                                               ; analysis links
        (back-analysis-links :initform nil)
                                               ; Backward pointer list for the
                                               ; analysis links
        (causal-links :initform nil)
                                               ; Forward pointer list for the
                                               ; causal links
        (back-causal-links :initform nil)
                                               ; Backward pointer list for the
                                               ; causal links
        (back-data-links :initform nil)
                                               ; Backward pointer list for
                                               ; Data links : Non implemented
                                               ; yet
        (list-of-path-elements :initform nil)
                                               ; The path-elements associated
                                               ; with this object because the
                                               ; object has been created by
                                               ; this path-element or the path
                                               ; element has been created by
                                               ; this object
        ))


(defclass named-object-mixin ()
       

;;; 
;;; Enables to give a name to an Object. The name must be updated using
;;; SET-NAME method exclusively. Do not change directly the slot-value because
;;; an association list *quail-object-NAME* must be  updated in the same time
;;; which permits to find rapidly an object given its name
;;; An object can be found when we know its name using the macro ($ Name)
;;; which returns Object 

       ((name :initform nil)))



(defclass prompt-mixin ()
       

;;; 
;;; This class specializes the PROMPT  methods of WEB-EDITOR for quail-BROWSER
;;; so that a *quail-PROMPT-WINDOW* is used for the Prompt.
;;; The place or this mixin in the inheritence list of quail-BROWSER or
;;; quail-object is important
  nil)



(defclass title-bar-mixin ()
       ((left-title-items :initform '(("Set package" set-package 
                                               "Select a package")
                                      ("How this works" how-this-works
                                                      
                            "A brief explanation as to how this browser works."
                                                      )
                                      ("Buttoning behavior" 
                                              buttoning-behavior 
                                          "How the mouse works in this browser"
                                                      ))
               :allocation :class)
        (middle-title-items :initform
               '(("Recompute" recompute "" (sub-items ("Recompute" recompute
                                                               
                                                         "Recompute the graph."
                                                               )
                                                      ("Recompute labels" 
                                                             recompute-labels
                                                        "Recompute the labels."
                                                               )
                                                      ("In place" 
                                                             recompute-in-place 
                                     "Recompute keeping current view in window"
                                                               )
                                                      ("Shape to hold" 
                                                             shape-to-hold 
                         "Make window large or small enough to just hold graph"
                                                               )
                                                      ("Lattice/Tree"
                                                             change-format                                                        
                                       "Change format between lattice and tree"
                                                               )))
                 ("Return to display" remove-from-bad-list 
                        "Restore item previously deleted from the display"))
               :allocation :class)
        (local-commands :initform '(buttoning-behavior how-this-works 
                                          describe-contents)
               :allocation :class)
        (left-shift-button-behavior :initform 
               "Retrieves the name of (or pointer to) the selected item" 
               :allocation :class)
        (title-left-button-behavior :initform "Information on the display" 
               :allocation :class)
        (title-middle-button-behavior :initform 
               "Actions to be taken on the display" :allocation :class)
        (title-right-button-behavior :initform "The usual window operations" 
               :allocation :class)
        (body-left-button-behavior :initform 
               "Information on the selected object" :allocation :class)
        (body-middle-button-behavior :initform 
               "Actions the selected object may take" :allocation :class)
        (body-right-button-behavior :initform "The usual window operations" 
               :allocation :class)
        (body-left-control-button-behavior :initform 
               "Reposition the selected object on the display" :allocation 
               :class)
        (how-this-works :initform "No description is yet available" :allocation
               :class)
        (contents-description :initform "No description is yet available" 
               :allocation :class)))



(defclass tool-box-link-mixin ()
       ((link-list :initform nil))
                                               ; Link manager of ToolBoxes
       )

;;;
;;; methods definitions
;;;


(defmethod get-local-methods-classification ((self indexed-object-mixin))
       (classify-local-methods (class-of self)))


(defmethod get-inherited-methods-classification ((self indexed-object-mixin))
       (classify-inherited-methods (class-of self)
              'quail-object))


(defmethod read-notes ((self editable-object-mixin))
       

;;; 
;;; Read the notes recorded on this item
;;; 

       (if (null (slot-value self 'notes))
           (quail-print-help "Nothing recorded on this item yet")
           (edit-text (slot-value self 'notes)
                  :read-only t)))


(defmethod edit-notes ((self editable-object-mixin))
       

;;; 
;;; Invoke TEDIT to edit the notes on the Notes IV
;;; 

       (setf (slot-value self 'notes)
             (edit-text (slot-value self 'notes))))


(defmethod add-notes ((self editable-object-mixin))
       

;;; 
;;; Stash user supplied note onto the Notes IV of quailObject
;;; 

       (setf (slot-value self 'notes)
             (edit-text (concatenate 'string (slot-value self 'notes)
                               "
" "Edited by " (get-user-name)
                               " "
                               (get-universal-time)
                               " :
"))))



(defmethod references ((self documented-object-mixin)
                       object)
                                               ; Produce a list of references
                                               ; in the litterature for this
                                               ; kind of quail-object
       (quail-print-help (if (slot-exists-p object 'references)
                             (slot-value object 'references)
                             "description not available")))


(defmethod short-summary ((object documented-object-mixin))
                                               ; Produce a short summary of
                                               ; this kind of OBJECT
       (quail-print-help (if (slot-exists-p object 'short-summary)
                             (slot-value object 'short-summary)
                             "description not available")))



(defmethod long-summary ((object documented-object-mixin))
                                               ; Produce a long summary of
                                               ; this kind of OBJECT
       (quail-print-help (if (slot-exists-p object 'long-summary)
                             (slot-value object 'long-summary)
                             "description not available")))


(defmethod print-summary ((self documented-object-mixin))
                                               ; Produce a summary of
                                               ; this kind of quail-object
       (quail-print-help (if (slot-exists-p self 'glossary)
                             (slot-value self 'glossary)
                             "description not available")))


(defmethod describe-contents ((self title-bar-mixin))
       

;;; 
;;; Describe the content of this browser by consulting the class Value
;;; Contents-Description
;;; 

       (quail-print-help "



          The content of this type of display

" :font-case 'bold)
       (quail-print-help (slot-value self 'contents-description)))


(defmethod how-this-works ((self title-bar-mixin))
       

;;; 
;;; Describe how this browser works by consulting the CV HOW-THIS-WORKS
;;; 

       (quail-print-help "



          How this works :

" :font-case 'bold)
       (quail-print-help (slot-value self 'how-this-works)))


(defmethod buttoning-behavior ((self title-bar-mixin))
       

;;; 
;;;  Produces some text describing the behavior of the mouse buttons in this
;;; browser by consulting the Class variables Left-shift-button-behavior,
;;; Title-Left-button-behavior, Title-Middle-button-behavior,
;;; Title-Right-button-behavior, Body-Left-button-behavior,
;;; Body-Middle-button-behavior, Body-Right-button-behavior and
;;; Body-Left-Control-Button-Behavior
;;; 

       (quail-print-help 
        "



          Mouse Button behavior


  Left button and Left Shift : " :font-case 'bold)
       (quail-print-help (slot-value self 'left-shift-button-behavior))
       (quail-print-help "


When in the Title bar

Left Button : " :font-case 'bold)
       (quail-print-help (slot-value self 'title-left-button-behavior))
       (quail-print-help "Middle Button : " :font-case 'bold)
       (quail-print-help (slot-value self 'title-middle-button-behavior))
       (quail-print-help "Right Button : " :font-case 'bold)
       (quail-print-help (slot-value self 'title-right-button-behavior))
       (quail-print-help "


When in the body of the display

Left Button : " :font-case 'bold)
       (quail-print-help (slot-value self 'body-left-button-behavior))
       (quail-print-help "Middle Button : " :font-case 'bold)
       (quail-print-help (slot-value self 'body-middle-button-behavior))
       (quail-print-help "Right Button : " :font-case 'bold)
       (quail-print-help (slot-value self 'body-right-button-behavior))
       (quail-print-help "Control key and left Button : " :font-case
              'bold)
       (quail-print-help (slot-value self 'body-left-control-button-behavior)))


(defmethod get-class-menu-items ((self body-menu-mixin) item-cv)
       

;;; 
;;; Build the list necessary to construct the middle menu of an object of the
;;; body of the browser
;;; 

       (declare (special object))

       (flet ((make-menu-list (list-methods &aux result)
                   (do ((pair list-methods (cddr pair)))  
                       ((null pair))
                      (push (list (first pair)
                                  (first pair)                                            
                                  "Slide over to see methods fitting this description."
                                  (append (list 'sub-items)
                                          (second pair)))
                             result))
                   result))

          (let ((local-methods (make-menu-list
                                  (classify-local-methods (class-of object))))
                (inherited-methods (make-menu-list 
                                      (classify-inherited-methods
                                               (class-of object)
                                               'quail-object)))
                item-1 item-2)

            (setq item-1 
                  (if local-methods 
                      (list "Class specific methods" nil 
     "Slide over to see methods intended specifically for this class of object"
                            (append (list 'sub-items)
                                    local-methods))
                      (list "Class specific methods" nil 
     "Slide over to see methods intended specifically for this class of object")))
             
            (setq item-2
                  (if inherited-methods
                      (list "Other methods" nil 
     "Slide over to see methods inherited by this object from more general classes"
                            (append (list 'sub-items)
                                    inherited-methods))
                      (list "Other methods" nil 
     "Slide over to see methods inherited by this object from more general classes")))                   
                
            (list item-1 item-2))))



(defmethod build-middle-menu ((self body-menu-mixin)
                              item-cv)
       

;;; 
;;; Method used to build a menu corresponding to a middle buttoning in the
;;; body of the window. The difference with the standard CHOICE-MENU used for
;;; the other menus is that the cached menu is seeked in Generic-middle-menus
;;; The menu is built by calling method GET-CLASS-MENU-ITEMS instead of
;;; reading the class variable MIDDLE-BUTTON-ITEMS so that the menu is
;;; automatically built according the methods defined for the class of the
;;; selected node.
;;; 

       (declare (special object))
       (prog (items (menu (assoc (class-of object)
                                 (slot-value self 'generic-middle-menus))))
             (if menu
                 (setq menu (cdr menu)))
                                               ; if menu is not NIL it
                                               ; contains the cached menu
             (cond ((and (slot-value self 'cache-menu-p)
                         (menu-p menu))
                                               ; if we have a cached menu and
                                               ; want to use it
                    (return menu))
                   ((not (listp (setq items    ; if we don't use the cached
                                               ; menu and can't build a
                                               ; correct one NIL is returned
                                      (get-class-menu-items self item-cv))))
                    (return nil)))
             (setq menu                        ; otherwise we build a new menu
                   (make-menu :items items :when-selected-fn 
                          'browser-when-selected-fn :when-held-fn 
                          'browser-when-held-fn :change-offset-fn t))
             (and (slot-value self 'cache-menu-p)
                                               ; and cache it if necessary
                  (let ((my-cached-menu (assoc (class-of object)
                                               (slot-value self 
                                                      'generic-middle-menus))))
                       (if my-cached-menu
                           (setf (cdr my-cached-menu)
                                 menu)
                           (push (cons (class-of object)
                                       menu)
                                 (slot-value self 'generic-middle-menus)))))
             (return menu)
                                               ; we eventually display the new
                                               ; menu
             ))


(defmethod middle-choice ((self body-menu-mixin))
       

;;; 
;;;  Make a selection from the menu build using Middle-Button-Items or
;;; Shift-middle-Button-Items 
;;; 

       (prog (menu)
             (setq menu (if (the-shift-key-p)
                            (if (slot-value self 'shift-middle-button-items)
                                (build-middle-menu self 'shift-middle-button-items)
                                (if (understands self 'middle-shift-select)
                                    (prog nil (middle-shift-select self)
                                          (return nil))
                                    (build-middle-menu self 'middle-button-items)))
                            (build-middle-menu self 'middle-button-items)))
             (return (and menu (select-in-menu menu)))))


(defmethod prompt-eval ((self prompt-mixin)
                        msg)
       

;;; 
;;; Specialization
;;; 

       (eval (prompt-read self msg)))



(defmethod prompt-read ((self prompt-mixin)
                        msg)
       

;;; 
;;; Specialization
;;; 

       (quail-prompt-read msg))



(defmethod get-name ((self named-object-mixin))
       

;;; 
;;; Read the name of an object SELF. 
;;; 

       (slot-value self 'name))



(defmethod set-name ((self named-object-mixin)
                     &optional name)
       

;;; 
;;; Give a name to an Object. If name is NIL, ask the user for a name
;;; The Hash table *quail-object-NAMES* is updated by this method
;;; The equivalence is made on the printed name only, so that the user doesn't
;;; have to care about packages. So, the name in the equivalence variable
;;; *quail-object-NAMES* is a string
;;; 

       (declare (special *quail-object-names*))
       (or name (setq name (prompt-read self 
                                  "Name to be given to this object : ")))
       (let ((previous-name (get-name self)))
            (if ($! name t)
                
                ;; 
                ;; doesn't allow 2 objects with the same name. So if the name
                ;; is already used, the user is asked whether he wants to
                ;; remove the previous existing name he has given or he wants
                ;; to put another name
                ;; 
                (if (member (prompt-read self (format nil 
   "the name ~S is already used, do you want to set this name anyway (Y or N) "
                                                     name))
                           '(y \y yes |yes|))
                    (setf (slot-value ($! name)
                                 'name)
                          nil)
                    (progn (set-name self)
                           (setq name nil))))
            
            ;; 
            (if name
                (progn 
                       ;; Build an empty Hash-Table if it doesn't exist yet
                       ;; 
                       (or (boundp '*quail-object-names*)
                           (setq *quail-object-names* (make-hash-table)))
                       
                       ;; 
                       ;; Update Hash-Table with new name
                       ;; 
                       (if previous-name (remhash previous-name 
                                                *quail-object-names*))
                       (setf (gethash name *quail-object-names*)
                             self)
                       
                       ;; 
                       ;; Update Slot-name of self
                       ;; 
                       (setf (slot-value self 'name)
                             name)))
            self))



(defmethod remove-back-analysis-link ((self linked-object-mixin)
                                      linkee)
       

;;; 
;;; Removes Linkee from Back Analysis links of self
;;; 

       (setf (slot-value self 'back-analysis-links)
             (remove linkee (slot-value self 'back-analysis-links))))


(defmethod remove-forward-analysis-link ((self linked-object-mixin)
                                         linkee)
       

;;; 
;;; Removes Linkee from  Analysis links of self
;;; 

       (setf (slot-value self 'analysis-links)
             (remove linkee (slot-value self 'analysis-links))))



(defmethod analysis-unlink ((self linked-object-mixin)
                            linkee)
       

;;; 
;;; Break an existing link between two quail Objects both directions.
;;; 

       (remove-forward-analysis-link self linkee)
       (remove-back-analysis-link self linkee)
       (remove-forward-analysis-link linkee self)
       (remove-back-analysis-link linkee self))



(defmethod analysis-link ((self linked-object-mixin)
                          linkee)
       

;;; 
;;; Establish an Analysis Link between two quail Objects
;;; 

       (add-forward-analysis-link self linkee)
       (add-back-analysis-link linkee self)
       linkee)



(defmethod causal-link ((self linked-object-mixin)
                        linkee)
       

;;; 
;;; Establish a Causal Link between two quail Objects
;;; 

       (add-forward-causal-link self linkee)
       (add-back-causal-link linkee self)
       linkee)



(defmethod add-forward-analysis-link ((self linked-object-mixin)
                                      linkee)
       

;;; 
;;; Adds linkee to Analysis-Links of self
;;; 

       (if (not (member linkee (slot-value self 'analysis-links)
                       :test
                       #'eq))
           (push linkee (slot-value self 'analysis-links))))



(defmethod add-forward-causal-link ((self linked-object-mixin)
                                    linkee)
       

;;; 
;;; Adds linkee to Causal-Links of self
;;; 

       (if (not (member linkee (slot-value self 'causal-links)
                       :test
                       #'eq))
           (push linkee (slot-value self 'causal-links))))



(defmethod add-back-analysis-link ((self linked-object-mixin)
                                   linkee)
       

;;; 
;;; Adds linkee to Back-Analysis-Links of self
;;; 

       (if (not (member linkee (slot-value self 'back-analysis-links)
                       :test
                       #'eq))
           (push linkee (slot-value self 'back-analysis-links))))



(defmethod add-back-causal-link ((self linked-object-mixin)
                                 linkee)
       

;;; 
;;; Adds linkee to Back-Causal-Links of self
;;; 

       (if (not (member linkee (slot-value self 'back-causal-links)
                       :test
                       #'eq))
           (push linkee (slot-value self 'back-causal-links))))



(defmethod initialize-instance :after ((object initable-object-mixin) &key)
       

;;; 
;;; Examines a new instance making certain that all IV's which require
;;; initialization are initialized then send the new instance the AfterInit
;;; message
;;; 

       (init-ivs object)
       (after-init object)
       (set-anonymous-object object)
       (after-building object)
       object)



(defmethod init-ivs ((self initable-object-mixin))
 

;;; 
;;; Initialize all IVs for this new instance whose have a Required Init . 
;;; We recognize a Required init because the INITFORM has the value
;;; (REQUIRED-INIT XXX)
;;; If XXX is T then the method simply checks that an initial value was
;;; supplied to  the IV (via New-With-Values). If the value of the required
;;; Init is a form or can be applied then EVAL the form or APPLY the function
;;; to determine an initial value for the IV
;;; 

 (dolist (iv (list-required-inits (class-of self)))
     (if (eq (slot-value self (slot-value iv 'name))
             (eval (slot-initform iv)))
         (let ((initializer (second (slot-initform iv))))
              (setf (slot-value self (slot-value iv 'name))
                    (if (eq initializer t)
                        (quail-error "Required Initialization missing for ~S"
                               (slot-value iv 'name))
                        (if (listp initializer)
                            (eval initializer)
                            (if (functionp initializer)
                                (funcall initializer self (slot-value
                                                           iv
                                                           'name))
                                (quail-error "Can't initialize ~S with ~S"
                                       (slot-value iv 'name)
                                       initializer)))))))))


(defmethod after-init ((self initable-object-mixin))
       
;;; 
;;; A method which is invoked just after creation of a new instance. This
;;; method only update Spawning Expression and return self. Specializations
;;; however should call-next-method, do some work then return Self
;;; 

       (declare (special *quail-expression*))
       
       ;; 
       ;; The *quail-EXPRESSION* is updated in method CALL-METHOD-AND-LINK
       ;; if the new node is built using the menus of the browser. Otherwise,
       ;; it means that a command has been entered using the listener and the
       ;; *quail-EXPRESSION* can be obtained by consulting the last item of
       ;; the History list
       ;; 
       (unless (and (boundp '*quail-expression*)
                    (listp *quail-expression*))
           (setq *quail-expression* (get-last-typed-command))
           (or (listp *quail-expression*)
               (setq *quail-expression* (list *expression* nil))))
               
       (setf (slot-value self 'spawning-expression)
             *quail-expression*)
       
       ;; we then reset *quail-EXPRESSION* for other uses

       (makunbound '*quail-expression*)

       self)


(defmethod after-building ((self initable-object-mixin))
       

;;; 
;;; This method updates the spawning-Expression and causal-links of objects
;;; generated simultaneously with a main object
;;; 

       (let (link)
            (dolist (instance-variable (set-difference (class-slots (class-of
                                                                    self))
                                              (list-required-inits (class-of
                                                                    self))))
                (when (typep (setq link (slot-value self (slot-value
                                                          instance-variable
                                                          'name)))
                             'object)
                    (causal-link self link)
                    (setf (slot-value link 'spawning-expression)
                          (slot-value self 'spawning-expression))))))



(defmethod toolbox-link ((self tool-box-link-mixin)
                         node linkee)
       

;;; 
;;; Establish a Toolbox link between two quail-object-CLASSES or TOOLBOXES
;;; 

       (if (and (not (equal self node))
                (not (equal linkee node))
                (not (equal linkee self)))
           (progn (add-forward-toolbox-link self linkee node)
                  (add-back-toolbox-link self node linkee))))



(defmethod toolbox-unlink ((self tool-box-link-mixin)
                           node linkee)
       

;;; 
;;; Break an existing link between two quail-objectS or TOOLBOXES - Both
;;; directions
;;; 

       (remove-forward-toolbox-link self node linkee)
       (remove-back-toolbox-link self node linkee)
       (remove-forward-toolbox-link self linkee node)
       (remove-back-toolbox-link self linkee node))



(defmethod remove-forward-toolbox-link ((self tool-box-link-mixin)
                                        node linkee)
       

;;; 
;;; removes linkee from the forward toolbox links of node
;;; 

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
       

;;; 
;;; removes linkee from the back toolbox links of node
;;; 

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
       

;;; 
;;; Add a new Toolbox link
;;; 

       (let ((node-1 (prompt-read self "Node to make link from : "))
             (node-2 (prompt-read self "Node to make link to : ")))
            (setq node-1 (or ($! node-1 t)
                             (find-class node-1)))
            (setq node-2 (or ($! node-2 t)
                             (find-class node-2)))
            (if (and node-1 node-2)
                (progn (toolbox-link self node-2 node-1)
                       (recompute self)))))



(defmethod find-link ((self tool-box-link-mixin)
                      node)
       

;;; 
;;; Return the element of link-list corresponding to node
;;; 

       (let (selection)
            (dolist (item (slot-value self 'link-list))
                (if (eq (car item)
                        node)
                    (return (setq selection item))))
            (if selection
                selection
                (list node nil nil))))



(defmethod break-link ((self tool-box-link-mixin))
       

;;; 
;;; Break an existing Toolbox link
;;; 

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
       

;;; 
;;; Adds Linkee to CADR of LinkList
;;; 

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
       

;;; 
;;; Adds Linkee to CADDR of LinkList
;;; 

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



