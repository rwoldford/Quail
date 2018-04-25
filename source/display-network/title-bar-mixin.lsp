;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                            title-bar-mixin.lisp                               
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

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(title-bar-mixin)))

(defclass title-bar-mixin ()
  ((left-title-items
    :initform
    '(("Set package" #'set-package "Select a package")
      ("How this works" #'how-this-works
       "A brief explanation as to how this browser works.")
      ("Buttoning behavior" #'buttoning-behavior 
       "How the mouse works in this browser"))
    :allocation :class)
   (middle-title-items
    :initform
    '(("Recompute" #'recompute ""
       :sub-items
       (("Recompute" #'recompute "Recompute the graph.")
        ("Recompute labels" #'recompute-labels "Recompute the labels." )
        ("In place"  #'recompute-in-place
         "Recompute keeping current view in window" )
        ("Shape to hold" #'shape-to-hold 
         "Make window large or small enough to just hold graph" )
        ("Lattice/Tree" #'change-format                                                        
         "Change format between lattice and tree" )))
      ("Return to display" #'remove-from-bad-list 
       "Restore item previously deleted from the display"))
    :allocation :class)
   (local-commands :initform '(#'buttoning-behavior #'how-this-works 
                               #'describe-contents)
                   :allocation :class)
   (left-shift-button-behavior
    :initform 
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
   (body-left-control-button-behavior
    :initform  "Reposition the selected object on the display"
    :allocation :class)
   (how-this-works :initform "No description is yet available" :allocation
                   :class)
   (contents-description :initform "No description is yet available" 
                         :allocation :class))
  (:documentation "A mixin to allow menus on title bars of windows."))


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
  "Describe how this browser works."
       (quail-print-help
        (format NIL "~&How this works:~%") :font-case 'bold)
       (quail-print-help (slot-value self 'how-this-works)))


(defmethod buttoning-behavior ((self title-bar-mixin))
  "Produces some text describing the behavior of the mouse buttons in this ~
   browser by consulting the Class variables Left-shift-button-behavior, ~
   Title-Left-button-behavior, Title-Middle-button-behavior, ~
   Title-Right-button-behavior, Body-Left-button-behavior, ~
   Body-Middle-button-behavior, Body-Right-button-behavior and ~
   Body-Left-Control-Button-Behavior."
  
  (quail-print-help 
   "~%~%~%      Mouse Button behavior


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


