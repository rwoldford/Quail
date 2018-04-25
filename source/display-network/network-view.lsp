;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                             network-view.lisp                              
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1991 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     G. Desvignes 1988 - 1989
;;;     R.W. Oldford 1985 - 1992.
;;;
;;;
;;;--------------------------------------------------------------------------------

(in-package :quail)

;;;
;;; class definition
;;;


(defclass network-view (documented-object editable-object
                        indexed-object quail-browser)
  ((left-button-items
    :initform
    '(("A short summary" #'short-summary 
       "A short description of the kind of item selected.")
      ("Zoom" #'zoom "View this item in greater detail."))
    :allocation :class
    :documentation
    "Menu items for Left Button selection. Value sent as message to ~
     object or view."
    )
   
   (left-title-items
    :initform 
    '(("How this works" #'how-this-works 
       "A Brief explanation as to how this kind of View works.")
      ("Buttoning Behavior" #'buttoning-behavior
       "How the mouse buttons work in this kind of View.")
      ("Manipulating the View" #'describe-contents 
       "How to manipulate the items in this display.")
      ("Name this View" #'name-this-item "Give a unique name to this View."))
    :allocation :class
    :documentation
    "Menu items that describe of this kind of View, and how to interact ~
     with its display."
    )
   
   (middle-title-items
    :initform
    '(("Widen" #'widen-view "Widen the View."
       :sub-items
       (("Widen the View" #'widen-view 
         "Widen the view by including existing objects.")
        ("Add result of a evaluation"  #'add-root 
         "Prompt for a form to be evaluated; the result is added to this Map.")))
      ("Narrow" #'narrow-view "Narrow the view."
       :sub-items
       (("Narrow the View" #'narrow-view
         "Narrow the view by excluding the selected Objects.")
        ("Create a View" #'create-view "Compress part of the  ~
                                        current view into a smaller view.")))
      ("Nesting" nil ""
       :sub-items
       (("Explode a nested View" #'explode-a-nested-view 
         "Explode a View nested in this display.")
        ("Return an exploded View" #'return-an-exploded-view
         "Return to the display a view that was exploded.")))
      ("Recompute" #'recompute "Recompute."
       :sub-items
       (("Recompute"  #'recompute "Recompute the graph.")
        ("Recompute labels" #'recompute-labels "Recompute the labels.")
        ("In Place" #'recompute-in-place "Recompute keeping current ~
                                          display in window.")
        ("Shape to hold" #'shape-to-hold "Make window large or ~
                                          small enough to just hold graph.")
        ("Lattice/Tree" #'change-format "lattice/tree")))
      )
    :allocation :class)
   
   ;; 
   ;; Controls format for laying out graph for GRAPHER
   
   (graph-format :initform *vertical-lattice*)
   
   ;; 
   ;; A List of the views that contain this one
   
   (enveloping-views :initform nil)
   
   ;; 
   ;; A list of views that are enveloped in by one
   
   (enveloped-views :initform nil)
   
   ;; 
   ;; A list of the views that are exploded within this one
   
   (exploded-views :initform nil)
   
   ;; 
   ;; Title passed to the GRAPHER and displayed in the Icons
   
   (title :initform "Network")))

;;;
;;; method definitions
;;;


(defmethod envelop-by ((self network-view) the-larger-view)
     "Records that the present view is nested within the-larger-view."

       (if (eq (class-of self)
               (class-of the-larger-view))
           (progn (unless (member the-larger-view (slot-value self 
                                                         'enveloping-views)
                                 :test
                                 #'eq)
                      (push the-larger-view (slot-value self 'enveloping-views)
                            ))
                  (unless (member self (slot-value the-larger-view 
                                              'enveloped-views)
                                 :test
                                 #'eq)
                      (push self (slot-value the-larger-view 'enveloped-views)))
                  )
           (quail-error "~S cannot envelop ~S" the-larger-view self)))


(defmethod widen-view ((self network-view)
                       &optional new-member dont-recompute-flg)
  "Add existing Objects to this View."
  (if new-member
    (progn (unless (member new-member (slot-value self 'starting-list)
                           :test
                           #'eq)
             (push new-member (slot-value self 'starting-list)))
           (if (typep new-member (class-name (class-of self)))
             (envelop-by new-member self))
           (unless dont-recompute-flg (recompute self)))
    (do nil
        ((not (setq new-member ($! (prompt-read self "The new member : "
                                                )))))
      (widen-view self new-member dont-recompute-flg))))



(defmethod error-cant-deduce-links-for ((self network-view)
                                        linked-object)
       

;;; 

       (quail-error "Can't deduce the links for : ~S" linked-object))



(defmethod error-self-nested ((self network-view)
                              nesting-view)
     "An error message to indicate that self is nested within Nesting-View."

       (quail-error "~S is already nested inside ~S" self nesting-view))


(defmethod zoom ((self network-view))
    "Zooms in on the current the view displaying it in a new window."

       (browse self (starting-list-of self)))


(defmethod the-view ((self network-view))
  "Returns what can be seen in this view."
  (slot-value self 'starting-list))


(defmethod forward-links ((self network-view)
                          linked-object)
     "This method must be specialized forspecializations of Network-View."

       (sub-class-responsibility self 'forward-links))


(defmethod backward-links ((self network-view)
                           linked-object)
  "This method must be specialized for specializations of Network-View."
  (sub-class-responsibility self 'backward-links))


(defmethod narrow-view ((self network-view)
                        &optional old-member dont-add-to-enveloping-views-flag
                        dont-recompute-flag)
  "Remove old-member from this view.  If dont-add-to-enveloping-views-flag is T ~
   then do not add old-member to those views which envelop this one."
  
  (if old-member
    (progn (setf (slot-value self 'starting-list)
                 (remove old-member (slot-value self 'starting-list)))
           
           ;; 
           ;; Remove from this view. If
           ;; Dont-Add-To-Enveloping-Views-Flag is NIL the OLD-MEMBER
           ;; need to go in all views that enveloped this one. If
           ;; OLD-MEMBER is a view, some care must be taken to sort out
           ;; this envelopingViews as well
           ;; 
           
           (if (typep old-member (class-name (class-of self)))
             (unenvelop-by old-member self))
           (unless dont-add-to-enveloping-views-flag
             (dolist (bigger-view (slot-value self 'enveloping-views))
               (widen-view bigger-view old-member)))
           (unless dont-recompute-flag (recompute self)))
    
    (do nil
        ((not (setq old-member ($! (prompt-read self 
                                                "The member to be removed from this view : "
                                                )))))
      (narrow-view self old-member dont-add-to-enveloping-views-flag 
                   dont-recompute-flag))))



(defmethod create-view ((self network-view))
    "Creates a new view . Prompting for elements as necessary."

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
                ((not (setq new-member ($! (prompt-read self 
                                                  "The new member : ")))))
              (widen-view the-new-view new-member t)
              (narrow-view self new-member t))
            (recompute the-new-view)))



(defmethod set-name ((self network-view)
                     &optional name)
       

;;; 
;;; Specialization : The Label must be removed from the Cache-label-list  of
;;; every view  SELF is represented in so that they are recompute next time
;;; those views are recompute
;;; 

       (call-next-method)
       (dolist (super-view (slot-value self 'enveloping-views))
           (let ((cached-label (assoc self (slot-value super-view 'label-cache)
                                      )))
                (setf (slot-value super-view 'label-cache)
                      (remove cached-label (slot-value super-view 'label-cache)
                             )))))


(defmethod path-to ((self network-view)
                    a-member)
     "Find all possible paths to  A-Member of this view."

       (let ((result nil))
            (if (typep a-member 'network-view)
                (dolist (item (the-view a-member))
                    (setq result (append result (path-to a-member item))))
                (dolist (item (backward-links self a-member))
                    (push item result)))
            result))



(defmethod path-from ((self network-view)
                      a-member)
     "Find all possible paths from  A-Member of this view."
       (let ((result nil))
            (if (typep a-member 'network-view)
                (dolist (item (the-view a-member))
                    (setq result (append result (path-from a-member item))))
                (dolist (item (forward-links self a-member))
                    (push item result)))
            result))


(defmethod return-an-exploded-view ((self network-view)
                                    &optional the-exploded-view
                                    dont-recompute-flag)
  "Removes THE-EXPLODED-VIEW from Exploded-views list of self and adds it to ~
   the view of self. The internals of THE-EXPLODED-VIEW are then removed from ~
   the view of self."
  
  (if (slot-value self 'exploded-views)
    
    (progn
      (unless the-exploded-view
        (setq the-exploded-view
              ($! (select-in-menu
                   (make-menu 
                    :items 
                    (let (result item)
                      (dolist (view (slot-value self 'exploded-views))
                        (setq item (or (get-name view) view))
                        (unless (stringp item)
                          (setq item (format nil "~a" item)))
                        (push (list item view "")
                              result))
                      result)
                    :title "Exploded Views")))))
      (if the-exploded-view
        (progn (dolist (item (the-view the-exploded-view))
                 (narrow-view self item t t))
               (widen-view self the-exploded-view t)
               (setf (slot-value self 'exploded-views)
                     (remove the-exploded-view (slot-value self 
                                                           'exploded-views)))
               (unless dont-recompute-flag (recompute self)))
        (prompt-print self "Nothing selected.")))
    
    (prompt-print self "No Exploded views."))
  the-exploded-view)



(defmethod explode-a-nested-view ((self network-view)
                                  &optional the-selected-view 
                                  dont-recompute-flag)
  "Removes the-selected-view from the view of self and adds the internals of ~
   the-selected-view to the view of self . The-selected-view is kept on an ~
   exploded-views list and can be returned to the view of self so that ~
   explode-a-nested-view can be undone."
  
  (unless the-selected-view
    (setq the-selected-view ($! (prompt-read self 
                                             "The view to be exploded : "))))
  (if the-selected-view
    (if (typep the-selected-view 'network-view)
      (progn (narrow-view self the-selected-view t)
             (dolist (item (the-view the-selected-view))
               (widen-view self item t))
             (push the-selected-view (slot-value self 'exploded-views)
                   )
             (unless dont-recompute-flag (recompute self))
             the-selected-view)
      (prompt-print self "Sorry, can't explode that kind of node."))))



(defmethod enveloped-by! ((self network-view) some-view)
    "Determines whether self is either contained in the view (!) of some-view ~
     or in the view (!) of any view that envelops Some-view (at any level)."

       (if (in-view! some-view self)
           t
           (let ((answer nil))
                (dolist (each-view (slot-value some-view 'enveloping-views))
                    (if answer
                        (return answer)
                        (setq answer (enveloped-by! self each-view))))
                answer)))



(defmethod in-view! ((self network-view) &optional the-object)
  "Determines whether the-object is in the-view of the immediate view or any ~
   of the views that are contained in it at any level ~
   If the-object is missing, it is assumed that this method is fired by menu ~
   selection from within some enveloping view."

       (if the-object
           (if (in-view self the-object)
               t
               (let ((result nil))
                    (dolist (sub-views (slot-value self 'enveloped-views))
                        (when (and (null result)
                                   (in-view! sub-views the-object))
                            (return (setq result t))))
                    result))
           (progn (setq the-object ($! (prompt-read self 
                                              "Give the object to be found : ")
                                       ))
                  (prompt-print self (if (in-view! self the-object)
                                         (format nil 
                                                "Yes, ~S contains the ~S ~S." 
                                                self (class-name (class-of
                                                                  the-object))
                                                the-object)
                                         (format nil 
                                           "No, ~S does not contain the ~S ~S."
                                                self (class-name (class-of
                                                                  the-object))
                                                the-object))))))


(defmethod in-view ((self network-view)
                    &optional the-object)
  
  
  "Determines whether the-object is in the-view of self.  ~
   If the-object is missing, it is assumed that this method is fired by menu ~
   selection from within some enveloping view."
  
  (if the-object
    (if (member the-object (the-view self)
                :test
                #'eq)
      t
      nil)
    (progn (setq the-object ($! (prompt-read self 
                                             "Give the object to be found : ")
                                ))
           (prompt-print self (if (in-view self the-object)
                                (format nil 
                                        "Yes, ~S contains the ~S ~S." 
                                        self (class-name (class-of
                                                          the-object))
                                        the-object)
                                (format nil 
                                        "No, ~S does not contain the ~S ~S."
                                        self (class-name (class-of
                                                          the-object))
                                        the-object))))))



(defmethod unenvelop-by ((self network-view)
                         the-larger-view)
    "Records that the present view is no longer nested within the-larger-view."

       (setf (slot-value self 'enveloping-views)
             (remove the-larger-view (slot-value self 'enveloping-views)))
       (setf (slot-value the-larger-view 'enveloped-views)
             (remove self (slot-value the-larger-view 'enveloped-views))))

(defmethod get-links ((self network-view)
                      object &key (reverse? nil))
  "Returns a list of objects to which object should be linked in the display.  ~
   If reverse? is T then the direction is reversed, giving all the ~
   objects which are forward linked to object."
  
  (let (the-subs)
    (if (typep object 'network-view)
      
      ;; 
      ;; For a Sub-view, the subs is the union of the subs of the
      ;; items of the sub-view
      ;; 
      
      (dolist (internal-object (the-view object))
        (if reverse?
          (dolist (item (path-to self internal-object))
            (when (in-view self item)
              (push item the-subs)))
          (dolist (item (path-from self internal-object))
            (when (in-view self item)
              (push item the-subs)))))
      
      ;; 
      ;; For a simple OBJECT (not a sub-view), the subs is the union
      ;; of the links (to or from) related to OBJECT and represented
      ;; in the view , and the sub-views which nodes have links (to
      ;; or from) with OBJECT.
      ;; 
      
      (progn (if reverse?
               (dolist (item (path-to self object))
                 (when (in-view self item)
                   (push item the-subs)))
               (dolist (item (path-from self object))
                 (when (in-view self item)
                   (push item the-subs))))
             
             (dolist (a-view (slot-value self 'enveloped-views))
               (when (member object (get-links self a-view
                                               :reverse? (not reverse?)))
                 (push a-view the-subs)))))
    the-subs))



(defmethod do-selected-command ((self network-view) command obj)
  "This specialization checks the result to see if it should be included in ~
   this view."
  
  (let ((result (call-next-method)))
    (if (and result (typep result 'quail-object))
      (widen-view self result))))