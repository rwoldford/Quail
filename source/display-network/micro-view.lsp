;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                             micro-view.lisp                              
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

(defclass microscopic-view (network-view)
  ((left-button-items
    :initform
    '(("Name this item" #'name-this-item "Give a name to this item.")
      ("Edit Notes" #'edit-notes "Edit the notes attached to this item.")
      ("Zoom" #'zoom "View this item in greater detail."))
    :allocation :class)
   (left-title-items
    :initform
    '(("How this works" #'how-this-works 
       "A brief explanation as to how a microscopic view works.")
      ("Buttoning Behavior" #'buttoning-behavior
       "How the mouse buttons work in this kind of View.")
      ("Show location on map" #'show-location-on-map 
       "Flash the node of this zoomed object in the Analysis map.")
      ("Short summary of zoomed object" #'short-summary 
       "A short description of the kind of item zommed in on."))
    :allocation :class)
   (middle-title-items
    :initform
    '(("Recompute" #'recompute "Recompute."
       :sub-items
       (("Recompute"  #'recompute "Recompute the graph.")
        ("Recompute labels" #'recompute-labels "Recompute the labels.")
        ("In Place" #'recompute-in-place
         "Recompute keeping current display in window.")
        ("Shape to hold" #'shape-to-hold 
         "Make window large or small enough to just hold graph.")
        ("Lattice/Tree" #'change-format 
         "Change format between lattice and tree."))))
    :allocation :class)
   (icon :initform zoom-icon :allocation :class)
   (mask :initform zoom-icon-mask :allocation :class)
   (title-reg :initform '(2 2 50 27)
              :allocation :class)
   (sub-view-icon :initform zoom-icon :allocation :class)
   (graph-format :initform *horiz-lattice*)
   (title :initform "Microscopic view")
   (zoomed-object :initform nil)))

;;;
;;; method definitions
;;;


(defmethod zoom ((self microscopic-view))
       

       (let ((the-zoomed-object (slot-value self 'zoomed-object)))
            (or the-zoomed-object (setq the-zoomed-object ($! (prompt-read
                                                               
                  "Give the object whose internal structure is to be viewed : "
                                                               ))))
            (setf (slot-value self 'starting-list)
                  (list the-zoomed-object))
            (call-next-method)))



(defmethod widen-view ((self microscopic-view)
                       &optional new-member dont-recompute-flag)
        

       (should-not-implement self 'widen-view))



(defmethod unenvelop-by ((self microscopic-view)
                         the-larger-view)
       
       (should-not-implement self 'unenvelop-by))



(defmethod show-location-on-map ((self microscopic-view))
       
       (show-location-on-map (slot-value self 'zoomed-object)))



(defmethod short-summary ((self microscopic-view))
       

       (short-summary (slot-value self 'zoomed-object)))



(defmethod return-an-exploded-view ((self microscopic-view)
                                    &optional the-exploded-view 
                                    dont-recompute-flag)
       
       (should-not-implement self 'return-an-exploded-view))



(defmethod path-from ((self microscopic-view)
                      linked-object)
       
       (if (eq linked-object (slot-value self 'zoomed-object))
           (list-subs linked-object)))



(defmethod path-to ((self microscopic-view)
                    a-member)
       
       (should-not-implement self 'paths-to))



(defmethod narrow-view ((self microscopic-view)
                        &optional old-member dont-add-to-enveloping-view 
                        dont-recompute-flag)
       
       (should-not-implement self 'narrow-view))



(defmethod get-links ((self microscopic-view)
                      object &key (reverse? NIL))
 
 (if reverse?
     (quail-error "Can't go backwards in a microscopic view ! ")
     (let
      ((the-subs (path-from self object))
       result)
      (if the-subs
          (if (listp the-subs)
              (progn
               (dolist (item the-subs)
                   (if (listp item)
                       (let* ((sub-object (car item))
                              (sub-label (cdr item))
                              (cached-label (assoc sub-object
                                                   (slot-value self
                                                          'menu-cache))))
                             (if (null cached-label)
                                 (let ((new-label (format nil "~A : ~A" 
                                                         sub-label
                                                         (get-label self 
                                                                sub-object))))
                                      (push (cons sub-object new-label)
                                            (slot-value self 'label-cache))))
                             (push sub-object result))
                       (push item result)))
               result)
              the-subs)))))



(defmethod forward-links ((self microscopic-view)
                          linked-object)
       
       (should-not-implement self 'forward-links))


(defmethod explode-a-nested-view ((self microscopic-view)
                                  &optional the-selected-view 
                                  dont-recompute-flag)
       
       (should-not-implement self 'explode-a-nested-view))



(defmethod envelop-by ((self microscopic-view)
                       the-larger-view)
       
       (should-not-implement self 'envelop-by))


(defmethod create-view ((self microscopic-view))
       
       (should-not-implement self 'create-view))



(defmethod backward-links ((self microscopic-view)
                           linked-object)
       
       (should-not-implement self 'backward-links))