;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                              help-window.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;
;;;
;;;  Authors:
;;;     C.B. Hurley 1992 George Washington University
;;;     R.W. Oldford 1992
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(*help-background-color* *help-foreground-color*
          set-help-pen-color set-help-background-color)))

(defvar *help-background-color*
  wb::*gray-color*
  ;;(wb:make-color 9509/32640 5911/8160 49087/65280)
  "The default background colour for help windows.")

(defvar *help-foreground-color*
  wb::*black-color*
  "The default foreground or pen colour for help windows.")


(defun set-help-pen-color (&optional (color (wb:prompt-user-for-color)))
  "Sets the foreground or pen colour of the help window to color. ~
   If color is not supplied the user is prompted to choose a color."
  (setf *help-foreground-color* color))


(defun set-help-background-color (&optional (color (wb:prompt-user-for-color)))
  "Sets the background colour of the help window to color. ~
   If color is not supplied the user is prompted to choose a color."
  (setf *help-background-color* color))

(defclass help-window (view-window)
  ()
  (:documentation "An empty class to distinguish help-windows from other ~
                   view-windows.  This is especially convenient when closing all ~
                   view-windows of a particular type."))

(defun make-help-window (&rest initargs
                               &key
                               (title "Help")
                               (left 10)
                               (width 400)
                               (bottom 10)
                               (height 400)
                               (background-color *help-background-color*)
                               (pen-color *help-foreground-color*))
  "Creates and returns a help-window."
  (apply #'make-view-window
         :view-window-class 'help-window
         :left left
         :right (+ left width)
         :bottom bottom
         :top (+ bottom height)
         :title title
         :background-color background-color
         :pen-color pen-color
         initargs))
  
(defun help-height (thing &optional (body-width 100))
  (typecase thing
    (text-view
     (*  (wb:canvas-font-height NIL :font (draw-style thing :font))
         (number-of-text-lines thing body-width))
     )
    (label
     (wb:canvas-font-height NIL :font (draw-style thing :font)))
    (header-box
     (multiple-value-bind
       (max-ht max-l)
       (loop for sv in (sub-views-of thing)
             for font = (draw-style sv :font)
             maximize (wb:canvas-font-height NIL :font font) into h
             maximize (wb:canvas-font-leading NIL :font font) into l
             finally (return (values h l)))
       (+ max-ht (* 4 max-l))))
    (scrolling-display
     (let ((scroll-bar-width 40))
       (+ scroll-bar-width
          (* (min 10
                  (display-length-of (first (scroller-displays-of thing))))
             (sub-view-height (first (scroller-displays-of thing)))))))
    (view-layout
     (let ((svs (subviews-of thing)))
       (loop for sv in svs sum (help-height sv))))
    ))

(defun max-help-key-width (help-view-list)
  (loop for v in (rest help-view-list) by #'cddr
        maximize (wb:canvas-string-width
                  NIL (get-text v)
                  :font (draw-style v :font))))

(defun help-window-width (help-view)
  "Returns the window width necessary to nicely display this help-view."
  (let* ((help-view-list (sub-views-of help-view))
         (header (first help-view-list)))
    (min (- (wb:screen-width) 30)
         (max (CL:+ 200 (max-help-key-width help-view-list))
              (CL:+ 30
                    (round (loop for v in (sub-views-of header)
                                 for r in (sub-view-locns-of header)
                                 sum (/
                                      (wb:canvas-string-width
                                       NIL (get-text v)
                                       :font (draw-style v :font))
                                      (width-of r)
                                      ))))))))

(defun help-window-height (help-view)
  "Returns the window height necessary to nicely display this help-view."
  (min (- (wb:screen-height) 50)
       (max 400
            (CL:+ 30
                  (round (loop for sv in (sub-views-of help-view)
                               sum
                               (help-height sv)
                               ))))))
