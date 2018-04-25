;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               browser.lisp
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

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(*graph-format-choices*
          *horiz-lattice*
          *horiz-tree*
          *vertical-lattice*
          *vertical-tree*
          bad-list
          cache-menu-p
          copies-only
          graph-format
          label-cache
          bad-list-of
          good-list-of
          starting-list-of
          graph-format-of
          label-cache-of
          lattice
          left-button-items
          left-button-items-of
          left-title-items
          left-title-items-of
          local-commands
          menu-cache
          middle-button-items
          middle-title-items
          middle-title-items-of
          right-title-items-of
          shift-left-button-items
          shift-middle-button-items
          starting-list
          title
          vertical)))


;;;
;;; variables definition
;;;

(defvar *horiz-lattice* (list 'lattice))

(defvar *horiz-tree* (list 'copies-only))

(defvar *vertical-lattice* (list 'reverse 'vertical 'lattice))

(defvar *vertical-tree* (list 'reverse 'vertical 'copies-only))

(eval-when (:compile-toplevel :load-toplevel :execute)
 (defvar *graph-format-choices*
  (list
   '("horizontal lattice" (lattice))
   '("vertical lattice" (reverse vertical lattice))
   '("horizontal tree" (copies-only))
   '("vertical tree" (reverse vertical copies-only)))))

;;;
;;; Class definition
;;;


(defclass browser (wb::canvas)
  ((icon
    :initarg :icon
    :accessor icon-of
    :initform nil)
   (icon-w
    :initarg :icon-w
    :accessor icon-w-of
    :initform nil)
   (selected-nodes
    :initarg :selected-nodes
    :accessor selected-nodes-of
    :initform nil)
   (box-line-width
    :initform 1
    :allocation :class)
   (local-commands
    :initform (list #'recompute #'add-root)
    :allocation :class
    :accessor local-commands-of)
   (left-button-items
    :initform (list (list "Flash node" #'flash-node "Flashes the selected node."))
    :allocation :class
    :accessor left-button-items-of)
   (shift-left-button-items
    :initform nil
    :allocation :class
    :accessor shift-left-button-items-of)
   (middle-button-items   
    :initform (list (list "Inspect" #'inspect "Inspect selected node."))
    :allocation :class
    :accessor middle-button-items-of)
   (shift-middle-button-items
    :initform nil
    :allocation :class
    :accessor shift-middle-button-items-of)
   (right-button-items
    :initform nil
    :allocation :class
    :accessor right-button-items-of)
   (graph-format-choices
    :initform *graph-format-choices*
    :accessor graph-format-choices-of
    :allocation :class)
   (cache-menu?          
    :initform t
    :accessor cache-menu-p)
   (menu-cache
    :initform nil
    :accessor menu-cache-of)
   (top-align             
    :initform t
    :accessor top-align-p)
   (starting-list
    :initform nil
    :accessor starting-list-of)
   (good-list
    :initform nil
    :accessor good-list-of)
   (bad-list
    :initform nil
    :accessor bad-list-of)
   (label-cache
    :initform nil
    :accessor label-cache-of)
   (graph-format
    :initform *horiz-lattice*
    :accessor graph-format-of))
  (:default-initargs
    :middle-title-items 
    (list
     (list "Add Root" #'add-root
           "Add named item to starting list for browser.")
     (list "Recompute" #'recompute 
           "Recompute Lattice from starting list objects."
           :sub-items
           (list (list "Recompute" #'recompute
                       "Recompute Lattice from starting list objects.")
                 (list "Recompute labels" #'recompute-labels "Recompute the labels.")
                 (list "In place" #'recompute-in-place
                       "Recompute keeping current view in window.")
                 (list "Shape to hold" #'shape-to-hold
                       "Make window large or small enough to just hold graph.")
                 ;; Commented out until fixed.
                 ;;(list "Change Format" #'change-format
                 ;;      "Change format between Lattice and Tree.")
                 )))))
                       

(defun browser-redisplay (canvas)
  (wb::canvas-clear canvas)
  (redisplay-graph canvas))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  make-browser
;;;

(defun make-browser (&rest initargs
                           &key
                           (color? NIL color-supplied?)
                           (browser-class 'browser)
                           left right
                           bottom top 
                           region
                           font
                           (title "browser"))
  "Creates a new window.  Region is a list (left bottom width height) ~
   the dimensions are those of the content region of the window."
  
  (unless (stringp title)
    (setf title (format nil "~a" title)))
  (when  (wb::region-p region)
    (multiple-value-setq (left right bottom top)
      (wb::region-bounds region)))
  (let ((canvas
         (if (and left right bottom top)
           (apply #'wb:make-canvas
                  :canvas-class browser-class
                  :title title
                  :left left
                  :bottom bottom 
                  :font (or font *graph-default-node-font*)
                  :width (1+ (- right left))
                  :height (1+ (- top bottom))
                  :color? (if color-supplied?
                            color?
                            (wb:color-device-p))
                  initargs)
           (apply #'wb:make-canvas 
                  :canvas-class browser-class
                  :title title
                  :left 10
                  :bottom (max 10 (- (wb::screen-height) 350))
                  :width 400
                  :height (min 300 (- (wb::screen-height) 10))
                  :color? (if color-supplied?
                            color?
                            (wb:color-device-p))
                  :font (or font *graph-default-node-font*)
                  initargs))))
    
    (wb::set-left-button-fn canvas #'*browser-left-button-fn*)
    (wb::set-middle-button-fn canvas #'*browser-middle-button-fn*)
    (wb::set-right-button-fn canvas #'*browser-right-button-fn*)
    
    (wb::set-ctrl-left-button-fn canvas #'*browser-ctrl-left-button-fn*)
    (wb::set-ctrl-middle-button-fn canvas #'*browser-ctrl-middle-button-fn*)
    (wb::set-ctrl-right-button-fn canvas #'*browser-ctrl-right-button-fn*)
    
    (wb::set-shift-left-button-fn canvas #'*browser-shift-left-button-fn*)
    (wb::set-shift-middle-button-fn canvas #'*browser-shift-middle-button-fn*)
    (wb::set-shift-right-button-fn canvas #'*browser-shift-right-button-fn*)
    
    (wb::set-redisplay-fn canvas #'browser-redisplay)
    
    (wb::show-canvas canvas)
    canvas))


