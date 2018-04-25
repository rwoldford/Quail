;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               body-menu.lisp                               
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

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(body-menu middle-button-items-of
          generic-middle-menus-of)))


(defclass body-menu ()
  ((middle-button-items
    :initform nil :allocation :class
    :accessor middle-button-items-of
    :documentation
    "Items to be done if middle button is selected in Window.  ~
     All the methods concerning middle-button-items are in ~
     body-menu.")
   (generic-middle-menus
    :initform nil :allocation :class
    :accessor generic-middle-menus-of
    :documentation
    "Offer up menus specific to the class of an item which has been ~
     (middle) buttoned . A single class variable generic-middle-menus ~
     caches as an association list the menus constructed for the various ~
     classes."
    ))
  (:documentation
   "This mixin will replace the methods left-choice and ~
    middle-choice of browser in quail-browser."))



(defmethod get-class-menu-items ((self body-menu) item-cv)
    "Build the list necessary to construct the middle menu of an object of the ~
     body of the browser."

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



(defmethod build-middle-menu ((self body-menu) item-cv)
  
  "Method used to build a menu corresponding to a middle buttoning in the ~
   body of the window. The difference with the standard CHOICE-MENU used for ~
   the other menus is that the cached menu is sought in Generic-middle-menus ~
   The menu is built by calling method GET-CLASS-MENU-ITEMS instead of ~
   reading the class variable MIDDLE-BUTTON-ITEMS so that the menu is ~
   automatically built according the methods defined for the class of the ~
   selected node."
  
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
          (make-menu :items items
                     :when-selected-fn #'browser-body-when-selected-fn
                     :when-held-fn #'browser-when-held-fn
                     :change-offset-fn t))
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

(defmethod middle-choice ((self body-menu))
  "Make a selection from the menu build using Middle-Button-Items or ~
   Shift-middle-Button-Items."
  
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
