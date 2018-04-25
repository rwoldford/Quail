;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               browser-generics.lisp
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

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(add-root
          browse
          change-format
          choice-menu
          do-selected-command
          flash-node
          get-display-label
          get-label
          get-links
          left-choice
          left-shift-select
          middle-choice
          middle-shift-select
          new-item
          prompt-print
          recompute
          recompute-in-place
          recompute-labels
          set-up-title-menus
          shape-to-hold
          show-browser
          title-left-shift-select
          unread)))


(defgeneric add-root (browser &optional new-item)
  (:documentation 
   "Add a named item to the starting list of browser."))

(defgeneric browse
  (browser &optional browse-list window-or-title good-list position)
  (:documentation 
   "Call show-browser and then Shape to Hold and move for first time."))

(defgeneric browser-objects (browser)
  (:documentation 
   "Return a list of all the objects shown in the Browser."))

(defgeneric change-format (browser &optional format)
  (:documentation 
   "Change format between Lattice and Tree."))

(defgeneric choice-menu (browser item-cv)
  (:documentation 
   "Create a menu and cache it in slot menu-cache."))

(defgeneric clear-label-cache (browser objects)
  (:documentation 
   "Delete the Cached label for the Objects."))

(defgeneric do-selected-command (browser command obj)
  (:documentation 
   "Does the selected command or forwards it to the object."))

(defgeneric flash-node (browser node &optional n flash-time leave-flipped?)
  (:documentation 
   "Flashes the selected  node."))

(defgeneric get-display-label (browser object)
  (:documentation 
   "Get the display label. Use the Cache if it provides the answer; If not, ~
    and Max-label-width is set, use it to compute the appropriate Bitmap and ~
    then cache the result."))


(defgeneric get-label (browser object)
  (:documentation 
   "Get a label for an object to be displayed in the browser."))

(defgeneric get-node-list (browser browse-list)
  (:documentation 
   "Compute the node data structures of the tree starting at BROWSE-LIST. If ~
    GOOD-LIST is given, only includes  elements of it."))

(defgeneric get-links (browser object &key reverse?)
  (:documentation 
   "Gets a set of subs from an object for browsing."))

(defgeneric has-object (browser object)
  (:documentation 
   "Check Object in Graph-Nodes and return if it is one of them."))

(defgeneric left-choice (browser)
  (:documentation 
   "Make a selection from the menu build using Left-Button-Items or ~
    Shift-left-Button-Items."))

(defgeneric left-selection (browser obj)
  (:documentation 
   "Choose an item from the Left button items and apply it."))

(defgeneric left-shift-select (browser)
  )
  
(defgeneric middle-choice (browser)
  (:documentation 
   "Make a selection from the menu build using Middle-Button-Items or ~
    Shift-middle-Button-Items."))
  
(defgeneric middle-selection (browser obj)
  (:documentation 
   "choose an item from the Middle button items and apply it."))
  
(defgeneric middle-shift-select (browser)
  )
  
(defgeneric new-item (browser &optional new-item)
  (:documentation 
   "Return object; prompt for it if needed."))
  
  
(defgeneric obj-name-pair (browser obj-or-name)
  (:documentation 
   "Make a pair (Object . (Obj-Name . NIL)) where Obj-Name is label to be ~
    used in browser."))
  
(defgeneric prompt-print (browser prompt)
  (:documentation 
   "Prints out the prompt in the prompt-window."))
  
(defgeneric recompute (browser &optional dont-reshape-flg)
  (:documentation
   "Recomputes the browser's network from the starting list of objects."))
  
(defgeneric recompute-in-place (browser)
  (:documentation
   "Recompute the graph maintaining the current position."))
  
(defgeneric recompute-labels (browser)
  (:documentation
   "Recompute the graph including the labels."))
  
(defgeneric shape-to-hold (browser)
  (:documentation 
   "Reshape the window to just hold the nodes of the browser."))
  
(defgeneric set-up-title-menus (browser)
  (:documentation
   "Sets up title menus by calling install-title-menus with the ~
    appropriate menu type."))

(defgeneric show-browser
  (browser &optional browse-list window-or-title good-list)
  (:documentation 
   "Show the items and their subs on a browse window  ~
    If WINDOW-OR-TITLE is not a window it will be used as a title for the ~
    window which will be created."))

(defgeneric title-left-choice (browser)
  (:documentation 
   "Make a selection from the menu build using Left-Title-Items."))
  

(defgeneric title-left-shift-select (browser)
  )
  
(defgeneric title-middle-choice (browser)
  (:documentation 
   "Make a selection from the menu built using Middle-Title-Items."))
  
(defgeneric title-middle-shift-select (browser)
  (:documentation 
   "No documentation available."))


(defgeneric unread (browser &optional object)
  (:documentation 
   "Unread Object into system buffer."))
 

(defgeneric title-middle-selection (browser)
  (:documentation 
   "Choose an item from the Middle button items and apply it."))
  
