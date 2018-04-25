;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               menu.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;
;;;  This file is part of the Window-Basics package intended to provide a uniform
;;;  window environment for Common Lisp programming.
;;;  
;;;  For copyright information and history, see window-basics-copyright.lisp
;;;
;;;  Authors:
;;;     C.B. Hurley 1989-1991
;;;     J.A. McDonald 1988-89
;;;     R.W. Oldford 1989-1991
;;;     J.O. Pedersen 1988-89
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------
;;; History:
;;;
;;; - A hierarchical pop-up menu system implemented at the University of Waterloo
;;;   as part of the Window-Basics generic window-system interface.
;;;
;;; - The system roughly follows the Xerox Interlisp-D menu model.
;;;   
;;;
;;; The Model:
;;;
;;; - A menu is a uniform means to select an item from a list of items
;;;   and have something happen as a consequence.
;;;
;;; - A menu is a data structure with the slots (or fields or ...) as
;;;   described below.
;;;   
;;;   items - The list of items to appear in the menu.  If the item is a list
;;;           then its CAR will appear in the menu.
;;;           The default selection functions interpret each item as a list of
;;;           three elements:
;;;                - a label
;;;                - a form whose value is returned upon selection, or
;;;                  a function which is applied to the value of the
;;;                  special variable *current-canvas*, and
;;;                - a help string which can be printed.
;;;           Should a fourth element exist, it is ignored by the default
;;;           selection functions but is interpreted by the default sub-item
;;;           function.  In particular, if the fourth element is the keyword
;;;           :sub-items then the fifth element of the list is taken to be a
;;;           list of sub-items by the default sub-item function.
;;;           These sub-items are just items which appear lower in the menu
;;;           hierarchy.
;;;
;;;   sub-item-fn - A function to be called to determine whether a given item
;;;           has any subitems.  It is called with two arguments:
;;;               - the menu, and
;;;               - the item.
;;;           It should return a list of the subitems, if any (NIL otherwise).
;;;           If an item has sub-items and the user moves the mouse-cursor
;;;           off to the right of that item, a sub-menu will appear having
;;;           the sub-items as its items.  Selecting from the sub-menu is
;;;           handled as if selecting from the main menu.
;;;           The default sub-item-fn is called default-sub-item-fn and
;;;           simply checks to see whether the item is a list whose fourth
;;;           element is the keyword :sub-items.  If so, it returns the
;;;           fifth element in the list; otherwise it returns NIL.
;;;
;;;   when-selected-fn - A function to be called when an item is selected.
;;;           It is called with three arguments:
;;;               - the item selected,
;;;               - the menu, and
;;;               - the last mouse key released (:left, :middle, or :right).
;;;           The default function, default-when-selected-fn evaluates and
;;;           returns the value of the second element of the item if the item
;;;           is a list of at least length 2.  Otherwise the item is returned.
;;;
;;;   when-held-fn - A function called when the user has held a mouse key on
;;;           an item for *menu-held-wait* milliseconds (initially 1200).
;;;           It is intended to be a means to prompt users.  As the default,
;;;           default-when-held-fn prints the third element of the item (i.e.
;;;           its help string) in the user prompt window.  If that element is
;;;           missing, the string "This item will be selected when the button
;;;           is released." will be printed.
;;;           The function takes three arguments:
;;;               - the item selected,
;;;               - the menu, and
;;;               - the last mouse key released (:left, :middle, or :right).
;;;
;;;   when-unheld-fn - A function called when:(1) the cursor leaves the
;;;           item, or (2) when a mouse button is released, or (3) when
;;;           another key is pressed.  It's called with the same three 
;;;           arguments as the when-held-fn and is intended to be used
;;;           to clean up after the when-held-fn function.  For example,
;;;           default-when-unheld-fn clears the prompt window.
;;;        
;;;   font - The font in which the items will appear in the menu.
;;;           Default is value of *default-menu-font*.
;;;
;;;   title - If non-NIL, the value of this field will appear in a line
;;;           above the menu.
;;;
;;;
;;;
;;;
;;; Comments:
;;;
;;; - The system is defined as part of the Window-Basics package.
;;;   However, most menu functions are independent of windows.
;;;  
;;;     
;;;
;;;
;;; Author(s):
;;;
;;;    R.W. Oldford (rwoldford@water.waterloo.edu) July 1989.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package :wb)

(export  '(*default-menu-font* *menu-held-wait* default-sub-item-fn
           default-when-selected-fn default-when-held-fn
           default-when-unheld-fn))


;;;------------------------------------------------------------------------------
;;;
;;;    Default menu variables and functions.
;;;
;;;------------------------------------------------------------------------------

(defvar *default-menu-font* *normal-graphics-font*)

(defvar *menu-held-wait* 1200
  "The number of milliseconds to wait on a menu before prompting.")

(defun default-sub-item-fn (menu item)
  (declare (ignore menu))
  (sub-items-of item))

(defun default-when-selected-fn (item menu mouse-button)
  "Special selection function that calls the function in the menu ~
   on the value of *current-canvas*"
  (declare (ignore menu mouse-button)
           (special *current-canvas*))
  (if (item-action-p item) 
    (let ((action (item-action item)))
      (cond
       ((functionp action)
        (funcall action *current-canvas*))
       (T (eval action))))
    item))

(defun default-when-held-fn (item menu mouse-button)
  (declare (ignore menu mouse-button))
  (wb-inform-user (item-message item)))

(defun default-when-unheld-fn (item menu mouse-button)
  (declare (ignore item menu mouse-button))
  ())





;;;-----------------------------------------------------------------------------
;;;
;;;                Creating a menu
;;;
;;;-----------------------------------------------------------------------------
;;;
;;; Menu creation is left unspecified at this level.
;;; Many CLs have their own menu implementations and these should be accessed
;;; to avoid a layer of indirection.
;;; However, the implemented function must have the following arguments (with
;;; defaults) and must return a data structure/ object/ whatever that represents
;;; a potentially hierarchical menu.
;;;
;;; (defun make-menu (&key items
;;;                        (font *default-menu-font*)
;;;                        (sub-item-fn default-sub-item-fn)
;;;                        (when-selected-fn default-when-selected-fn)
;;;                        (when-held-fn default-when-held-fn)
;;;                        (when-unheld-fn default-when-unheld-fn))
;;; then-the-body)
;;;
;;;
;;; ------------------------------------------------------------------------------------
;;;
;;;            Other menu functions
;;;
;;; ------------------------------------------------------------------------------------
;;; 
;;;
;;; (defun menu-p (menu) (type menu ........
;;;
;;; (defun destroy-menu (menu) ... Destroys menu so that its space can be reclaimed.
;;;
;;; (defun set-menu-fns (menu &key (selected nil) (held nil) (unheld nil) (sub-item nil))
;;;                           ... Changes the functions when-selected-fn, when-held-fn,
;;;                               et cetera according to the values of corresponding
;;;                               keyword arguments.
;;;
;;; (defun put-menu-prop (menu property value) ... Stores value on property of menu.
;;;                                                These are stored on the
;;;                                                "user-properties" slot of the menu.
;;;
;;; (defun get-menu-prop (menu property)  ... Returns the value associated with the
;;;                                           property on menu"
;;;
;;; (defun menu (menu &key position) ... Pops menu up at position in screen-coordinates.
;;;                                     (If given, otherwise at current mouse-position.)
;;;
;;;  (defun disable-menu-item (menu item-string)   Disables the menu item identified by
;;;                                                item-string
;;;
;;;  (defun enable-menu-item (menu item-string)   Enables the menu item identified by
;;;                                                item-string
;;;
;;;  (defun check-menu-item (menu item-string &optional (check-char nil) 
;;;                                                   Places a check-char beside item identified by
;;;                                                item-string
;;;
;;;   (defun set-menu-item-string (menu item-string new-string))





                       
