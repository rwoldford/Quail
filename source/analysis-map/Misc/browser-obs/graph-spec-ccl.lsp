;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               graph-spec-mcl.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;
;;;
;;; This file contains the definitions of functions specific for Mac needed
;;; by GRAPH.
;;;
;;;  Authors:
;;;     G. Desvignes  1988-89
;;;     R.W. Oldford  1988-1991
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package 'specific)

(import 'pcl::object)      ; make specific::object be pcl::object to avoid
(shadow 'object)           ; a conflict with ccl:object when object is exported
                           ; from pcl in browser-pcl

(use-package 'ccl)         ; in package ccl are defined the records and traps
(use-package 'ccl 'graph 'window-basics)


(eval-when (:compile-toplevel :load-toplevel :execute) (export '(apply-default-window-command
          browser-left-fn-of
          browser-middle-fn-of
          browser-right-fn-of
          graph-of
          left-title-fn-of
          get-window-middle-title-fn
          make-window
          set-window-browser-left-fn
          set-window-browser-middle-fn
          set-window-browser-right-fn
          set-window-button-event-fn
          set-window-graph
          set-window-left-title-fn
          set-window-middle-title-fn
          )))


(use-package 'specific 'graph)

(proclaim '(object-variable wptr))


;;;
;;; function definitions
;;;

(defun apply-default-window-command (window)
  "Apply the default command for the window when the button of the mouse is ~
   pressed inside the window."
  (declare (ignore window))
  ;; for Mac, we modify the title-bar of the window so that the window can be
  ;; moved by clicking inside an icon inside the title bar and graph functions
  ;; are performed when you click somewhere else in the title bar
  ;; this means that we don't need any default function
  ;;
  nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
;;;
;;; WINDOW FUNCTIONS
;;;

(defobject graph-window (*color-canvas*)) ;<----------------change later **** rwo

(defun make-window (&key region title)

;;;
;;; creates a new window
;;; region is a list (left bottom width height)
;;; the dimensions are those of the content region of the window
;;;

(or (stringp title)
    (setf title (format nil "~a" title)))
(let* ((window (oneof graph-window :window-title (or title "Untitled")
                                   :window-type :document-with-zoom
                                   :close-box-p t
                                   :window-show nil))
       (w-ptr (ask window wptr)))
   (ask window (have 'browser-left-fn nil))
   (ask window (have 'browser-middle-fn nil))
   (ask window (have 'graph nil))
   (ask window (have 'menubar nil)))
   (ask window (have 'browser nil))
   (ask window (have 'icon nil))
   (ask window (have 'icon-w nil))
   (ask window (have 'graph::selected-node nil))
   (when region
         (ask window (set-window-size (third region)(fourth region)))
         (_MoveWindow :ptr w-ptr
                      :word (first region)
                      :word (max 40 (second region))
                      :word 255))
   (ask window (window-select))
   window))
   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; WINDOW FIELDS ACCESS FUNCTIONS
;;;


(defun browser-left-fn-of (window)

  (declare (object-variable browser-left-fn))
  (ask window browser-left-fn))


(defun browser-middle-fn-of (window)

  (declare (object-variable browser-middle-fn))
  (ask window browser-middle-fn))


(defun browser-right-fn-of (window)

  (declare (ignore window))
  nil)


(defun graph-of (window)

;;; returns the graph represented in the window

  (declare (object-variable graph))
  (if (ask window (boundp 'graph))
      (ask window graph)
      nil))


(defun left-title-fn-of (window)

;;; title menu is in menubar

  (declare (ignore window))
  nil)


(defun get-window-middle-title-fn (window)

;;; title menu is in menubar

  (declare (ignore window))
  nil)



(defun set-window-browser-left-fn (window fn)

  (declare (object-variable browser-left-fn))
  (ask window (setf browser-left-fn fn)))


(defun set-window-browser-middle-fn (window fn)

  (declare (object-variable browser-middle-fn))
  (ask window (setf browser-middle-fn fn)))


(defun set-window-browser-right-fn (window fn)

  (declare (ignore window fn))
  nil)


(defun set-window-button-event-fn (window fn)
 
;;; the button event fn is implemented by creating an object function
;;; window-click-event-handler for the new window class graph-window

(declare (ignore window))
fn)

(defun set-window-graph (window gr)

  (declare (object-variable graph))
  (ask window (setf graph gr)))


(defun set-window-left-title-fn (window fn)

;;; title menu is in menubar

  (declare (ignore window))
  fn)


(defun set-window-middle-title-fn (window fn)

;;; title menu is in menubar

  (declare (ignore window))
  fn)

