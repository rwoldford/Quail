;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               menu-canvas.lisp
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
;;;     R.W. Oldford 1989-1991
;;;     
;;;
;;;
;;;-------------------------------------------------------------------
;;;
;;; The story:
;;;
;;; Want various hierarchical menus to be associated with a canvas.
;;; These will pop-up in response to certain mouse-events while the
;;; mouse is over either the title-bar of the canvas, or over the
;;; body of the canvas.
;;;
;;; This file (and others specialized for each CL) contains those
;;; functions which attach menus to a canvas.
;;; 
;;; -------------------------------------------------------------------
;;;
;;; Author(s):
;;;       rwoldford@stat.waterloo.edu
;;;
;;; The model:
;;;
;;; To achieve this, two slots are assigned to each canvas record to
;;; cache these menus.
;;;
;;; The slots are
;;;
;;;                title-menus
;;; and simply
;;;
;;;                menus
;;;
;;; for those which are typically associated with the body.  
;;;
;;; The value of each slot is a collection of keyword-menu pairs. 
;;; Interaction with these collections will be through the functions:
;;;
;;; (canvas-put-title-menu canvas &key key menu)
;;;  - stores the title-menu "menu" using the look-up key "key"
;;;
;;; (canvas-get-title-menu canvas &key key)
;;;  - if no key is given all title-menus are returned in a list, otherwise
;;;    only that title-menu associated with key is returned.
;;;
;;; (canvas-put-menu canvas &key key menu)
;;;  - stores the menu "menu" using the look-up key "key"
;;;
;;; (canvas-get-menu canvas &key key)
;;;  - if no key is given all menus are returned in a list, otherwise
;;;    only that menu associated with key is returned.
;;;
;;; ------------------------------------------------------------------------
;;;
;;; Conventions:
;;;
;;; For the title-menus,the Window-Basics package makes use of keywords
;;; corresponding to mouse-key + modifier selections supported by wb-mouse.
;;; These are:
;;;
;;;             :left
;;;             :ctrl-left
;;;             :middle
;;;             :ctrl-middle
;;;             ::right
;;;             :ctrl-right
;;;
;;; The shift modifier has been ignored - shift is meant to return different
;;; aspects of the lisp object selected.  If this recommended style is
;;; followed, no menu should be constructed.
;;;
;;; 
;;; It's important to note that the user is free to use their own collection
;;; of keys in any application.  This allows the menu interaction to be
;;; easily extended.
;;; 
;;; ... rwo
;;;
;;;

(in-package :wb)


(export  '(add-canvas-title-item remove-canvas-title-item
           canvas-put-title-menu canvas-get-title-menu
           canvas-put-menu canvas-get-menu
           title-menus-of menus-of menu-canvas
           left-title-items-of ctrl-left-title-items-of
           middle-title-items-of ctrl-middle-title-items-of
           right-title-items-of ctrl-right-title-items-of))




;;;=====================================================================
;;; Want various menus to be associated with a window.
;;; Minimally, these will pop up whenever a mouse button is depressed
;;; while the mouse is over the title-bar of the canvas.
;;; For the Macintosh, these menus will appear in the menu-bar
;;; when that canvas is the active window on the desktop.
;;;
;;; Accomplished by defining an appropriate mixin for canvas:
;;;
;;;                  menu-canvas
;;;
;;; ...rwo
;;;=====================================================================


(defclass menu-canvas (host-menu-canvas)
  ((menus       :initarg :menus
                :initform nil
                :accessor menus-of
                :documentation 
                "A collection of keyword-menu pairs.  These menus will ~
                 appear on when selection is made in the body of the ~
                 canvas.")
   (title-menus :initarg :title-menus
                :initform nil
                :accessor title-menus-of
                :documentation 
                "A collection of keyword-menu pairs.  These menus will ~
                 appear on when selection is made in the title of the ~
                 canvas.")
   (left-title-items
    :initform  NIL
    :initarg :left-title-items
    :accessor left-title-items-of)
   (middle-title-items 
    :initform  NIL
    :initarg :middle-title-items
    :accessor middle-title-items-of)
   (right-title-items
    :initarg :right-title-items
    :initform
    (list 
          (list "Redisplay" #'(lambda (c) (redisplay c))
                "Redisplay this canvas.")
          (list "Display parameters" NIL 
                "Change some display parameters of this canvas."
                :sub-items
                (list (list "Title"
                            #'(lambda (c)
                                (setf (canvas-title c)
                                      (prompt-user
                                       :prompt-string "Enter the title: "
                                       :read-type :string
                                       :type 'string)))
                            "Change the title of the canvas.")
                      (list "Background color"
                            #'(lambda (c)
                                (canvas-set-background-color c (prompt-user-for-color)))
                            "Change the background color of the canvas.")
                      (list "Pen color"
                            #'(lambda (c)
                                (set-pen-color c (prompt-user-for-color)))
                            "Change the pen color.")
                      (list "Pen width"
                            #'(lambda (c)
                                (set-pen-width
                                 c
                                 (prompt-user
                                  :prompt-string "Enter the new width (in pixels): "
                                  :type 'integer
                                  :read-type :eval)))
                            "Change the pen width.")))
          (list "Canvas fonts" NIL 
                "Change the font of this canvas."
                :sub-items
                (list (list "Font"
                            #'(lambda (c)
                                (declare (special *canvas-font-names*))
                                (let ((f-copy (copy-canvas-font (canvas-font c))))
                                  (set-canvas-font-name
                                   f-copy
                                   (first (prompt-for-items *canvas-font-names*)))
                                  (setf (canvas-font c) f-copy)))
                            "Change the font name used in the canvas.")
                      (list "Font style"
                            #'(lambda (c)
                                (declare (special *canvas-font-styles*))
                                (let ((f-copy (copy-canvas-font (canvas-font c))))
                                  (set-canvas-font-style
                                   f-copy
                                   (prompt-for-items
                                    *canvas-font-styles* :selection-type :disjoint))
                                  (setf (canvas-font c) f-copy)))
                            "Change the font style used in the canvas.")
                      (list "Font size"
                            #'(lambda (c)
                                (let ((f-copy (copy-canvas-font (canvas-font c))))
                                  (set-canvas-font-size
                                   f-copy
                                   (prompt-user
                                    :prompt-string "Enter the new font size (1 to 127): "
                                    :type 'integer
                                    :read-type :eval))
                                  (setf (canvas-font c) f-copy)))
                            "Change the font size used in the canvas.")))
          (list "Draw on canvas" 
                #'(lambda (c)
                    (let (c?)
                      (loop
                        (when (mouse-down-p c)
                          (setf c? (which-canvas (screen-mouse-position)))
                          (if (not (eq c c?)) (return))
                          (let* ((mouse-pos (mouse-position c))
                                 (old-x (position-x mouse-pos))
                                 (old-y (position-y mouse-pos))
                                 new-x new-y)
                            (do ((i 1 (+ i 1)))
                                ((not (mouse-down-p c)))
                              (setf mouse-pos (mouse-position c))
                              (setf new-x (position-x mouse-pos))
                              (setf new-y (position-y mouse-pos))
                              (canvas-draw-line c old-x old-y new-x new-y)
                              (setf old-x new-x)
                              (setf old-y new-y))
                            (return))))))
                "Draw on the current canvas.")
          (list "Clear canvas" #'canvas-clear "Clear the canvas.")
          (list "Save canvas" #'save-value "Save the current canvas on a symbol.")
          (list "Inspect canvas" #'inspect "Inspect the current canvas."))
    :accessor right-title-items-of)
   (ctrl-left-title-items
    :initform
    NIL
    :accessor ctrl-left-title-items-of)
   (ctrl-middle-title-items 
    :initform 
    NIL
    :accessor ctrl-middle-title-items-of)
   (ctrl-right-title-items
    :initform NIL
    :accessor ctrl-right-title-items-of))
  (:documentation
   "A mixin class for canvas to allow for caching menus and defining~
    some menu items on the title bar."))

;;;--------------------------------------------------------------------------
;;;
;;; Now the functions.
;;;
;;;--------------------------------------------------------------------------


(defun add-canvas-title-item (canvas item &optional (where :left))
  "Adds the item to the title items of canvas corresponding to one of ~
   :left :middle :right  ~
   :ctrl-left :ctrl-middle or :ctrl-right as the optional argument ~
   where.  The default of where is :left."
  (case where
    (:left
     (setf (left-title-items-of canvas)
           (append (left-title-items-of canvas)
                   (list item))))
    (:middle
     (setf (middle-title-items-of canvas)
           (append (middle-title-items-of canvas)
                   (list item))))
    (:right
     (setf (right-title-items-of canvas)
           (append (right-title-items-of canvas)
                   (list item))))
    (:ctrl-left
     (setf (ctrl-left-title-items-of canvas)
           (append (ctrl-left-title-items-of canvas)
                   (list item))))
    (:ctrl-middle
     (setf (ctrl-middle-title-items-of canvas)
           (append (ctrl-middle-title-items-of canvas)
                   (list item))))
    (:ctrl-right
     (setf (ctrl-right-title-items-of canvas)
           (append (ctrl-right-title-items-of canvas)
                   (list item))))
    ))

(defun remove-canvas-title-item (canvas item &optional (where :left))
  "Removes the item (if found) from ~
   the title items of canvas corresponding to one of ~
   :left :middle :right  ~
   :ctrl-left :ctrl-middle or :ctrl-right as the optional argument ~
   where (default :left)."
  (case where
    (:left
     (setf (left-title-items-of canvas)
           (remove item (left-title-items-of canvas) :test #'equal)))
    (:middle
     (setf (middle-title-items-of canvas)
           (remove item (middle-title-items-of canvas) :test #'equal)))
    (:right
     (setf (right-title-items-of canvas)
           (remove item (right-title-items-of canvas) :test #'equal)))
    (:ctrl-left
     (setf (ctrl-left-title-items-of canvas)
           (remove item (ctrl-left-title-items-of canvas) :test #'equal)))
    (:ctrl-middle
     (setf (ctrl-middle-title-items-of canvas)
           (remove item (ctrl-middle-title-items-of canvas) :test #'equal)))
    (:ctrl-right
     (setf (ctrl-right-title-items-of canvas)
           (remove item (ctrl-right-title-items-of canvas) :test #'equal)))
    )
  )

(defun canvas-put-title-menu (canvas &key key menu)
  (with-accessors ((tm title-menus-of)) canvas
    "Adds the :menu to the title-menus using :key as a lookup key."
    (setq tm (acons key menu tm))))


(defun canvas-get-title-menu (canvas &key key)
  (with-accessors ((tm title-menus-of)) canvas
    "Gets the :menu from the title-menus using :key as a lookup key.
   If no :key is given, the assoc list of title-menus is returned."
    (if key
      (cdr (assoc key tm))
      tm)))

(defun canvas-put-menu (canvas &key key menu)
  (with-accessors ((m menus-of)) canvas
    "Adds the :menu to the menus using :key as a lookup key."
    (setq m (acons key menu m))))



(defun canvas-get-menu (canvas &key key)
  (with-accessors ((m menus-of)) canvas
    "Gets the :menu from the menus using :key as a lookup key.
   If no :key is given, the assoc list of menus is returned."
    (if key
      (cdr (assoc key m))
      m)))

(defun install-title-menus (canvas
                            &key
                            (title-left        "Information")
                            (title-middle      "Edit-display")
                            (title-right       "Canvas")
                            (ctrl-title-left   "")
                            (ctrl-title-middle "")
                            (ctrl-title-right  "")
                            (menu-type :pop-up)
                            )
  "Installs the title items found on the browser as title-menus of the canvas."
  
  (let (items)
    ;; release the menu record (some systems have such non-lisp things)
    (loop for m  in (title-menus-of canvas)
          do
          (release-menu-space (cdr m)))
    (setf (title-menus-of canvas) NIL)
    (loop for f in (list
                    (list :left 
                          #'left-title-items-of           title-left)
                    (list :middle
                          #'middle-title-items-of         title-middle)
                    (list :right
                          #'right-title-items-of          title-right)
                    (list :ctrl-left
                          #'ctrl-left-title-items-of      ctrl-title-left)
                    (list :ctrl-middle
                          #'ctrl-middle-title-items-of    ctrl-title-middle)
                    (list :ctrl-right
                          #'ctrl-right-title-items-of     ctrl-title-right))
          when (setf items (funcall (second f) canvas))
          collect
          (canvas-put-title-menu canvas
                                 :key (first f)
                                 :menu (make-menu
					:canvas canvas
                                        :items items
                                        :title (third f)
                                        :menu-type menu-type
                                        :when-selected-fn
                                        #'default-when-selected-fn)))
      )
    )


(defun release-menu-space (menu)
  (declare (ignore menu)))


;;; In MCL the menu-type cannot be :pop-up
;;; ... the default

(defmethod set-up-title-menus ((canvas menu-canvas))
  (install-title-menus canvas))

(defmethod initialize-instance :after ((canvas menu-canvas) &key)
  "When a new menu-canvas is created, we must set up the title menus ~
   for its class ~
   if it doesn't already exist."
  ;; the following calls install-title-menus with appropriate menu-type
  (set-up-title-menus canvas)
)
