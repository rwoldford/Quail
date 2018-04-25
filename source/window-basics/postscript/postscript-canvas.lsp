;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               postscript-canvas.lisp
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
;;;    N.G. Bennett 1992
;;;    R.W. Oldford 1992
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :window-basics)

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(postscript-info-of)))

(defclass postscript-info ()
  ((directory  :initform NIL :accessor write-directory-of
               :initarg :directory
               :documentation "This slot contains the directory to which the ~
                               postscript files will be written upon creation.")
   (body-file  :initform NIL :accessor body-file-of
               :initarg :file-name
               :documentation "This slot contains the name of the file which ~
                               contains the postscript function calls required ~
                               to recreate the canvas on a postscript page.")
   (header-file :initform NIL :accessor header-file-of
                :initarg :header-file
                :documentation "This slot contains the name of the file which ~
                                contains the postscript function definitions ~
                                needed to redisplay the canvas on a postscript ~
                                page.")
   (postscript-file :initform NIL :accessor postscript-file-of
                    :initarg :postscript-file
                    :documentation "This slot contains the name of the file ~
                                    which describes the canvas to a postscript ~
                                    device.")
   (included :initform (copy-tree *include-list*) :accessor included-of
             :documentation "This slot contains a list of includable functions ~
                             and whether or not each has been included in the ~
                             header file.")
   (ps-x-origin :initform "CENTER" :accessor ps-x-origin-of
                :documentation "This slot allows the user to explicitly set a new ~
                               x origin for the postscript page.  Everything will ~
                               be drawn with respect to the new x origin.")
   (ps-x-scale :initform 1.0 :accessor ps-x-scale-of
               :documentation "This slot allows the user to explicitly set a new x ~
                               scale factor for the conversion from screen canvas ~
                               to postscript canvas.")
   (ps-y-origin :initform "CENTER" :accessor ps-y-origin-of
                :documentation "This slot allows the user to explicitly set a new ~
                               y origin for the postscript page.  Everything will ~
                               be drawn with respect to the new y origin.")
   (ps-y-scale :initform 1.0 :accessor ps-y-scale-of
               :documentation "This slot allows the user to explicitly set a new y ~
                               scale factor for the conversion from screen canvas ~
                               to postscript canvas.")
   (ps-horizontal-margin :initform 18 :accessor ps-horizontal-margin-of
                         :documentation "This slot contains the number which ~
                                         corresponds to how much space (in 1/72 ~
                                         inch) is to be left blank at each ~
                                         horizontal edge of the page.")
   (ps-vertical-margin :initform 9 :accessor ps-vertical-margin-of
                         :documentation "This slot contains the number which ~
                                         corresponds to how much space (in 1/72 ~
                                         inch) is to be left blank at each ~
                                         vertical edge of the page.")
   (ps-width :initform NIL :accessor ps-width-of
             :documentation "This slot allows the user to explicitly set a new ~
                             width for the postscript canvas regardless of its ~
                             actual width.")
   (ps-height :initform NIL :accessor ps-height-of
             :documentation "This slot allows the user to explicitly set a new ~
                             height for the postscript canvas regardless of its ~
                             actual height.")
   (max-procedure-lines :initform 30 :accessor max-procedure-lines-of
             :documentation "This slot determines the maximum number of lines ~
                             which are allowed to be written into each procedure ~
                             of a multiple page postscript file.  It is strongly ~
                             recommended that the user not alter this number ~
                             unless they wish to spend considerable amounts of ~
                             time adjusting it for their particular laser printer.")
   (multiple-pages :initform NIL :accessor multiple-pages-p
             :documentation "This slot is used to set whether there are multiple ~
                             Postscript pages (not files) in the Postscript output.")
   (center-vertical :initform NIL :accessor center-vertical-p
             :documentation "This slot indicates whether to centre the Postscript~
                             output vertically")
   (center-horizontal :initform NIL :accessor center-horizontal-p
             :documentation "This slot indicates whether to centre the Postscript~
                             output horizontally.")
   )
  (:documentation "A class containing the file and page property information ~
                   required to correctly generate a postscript page description ~
                   for a canvas."))

(defclass postscript-canvas (display-mode-mixin)
  ((postscript-info  :initform 
                     (make-instance 'postscript-info)
                     :accessor postscript-info-of
                     :initarg :postscript-info)))

(defmethod max-procedure-lines-of ((self postscript-canvas))
  (max-procedure-lines-of (postscript-info-of self)))

(defmethod (setf max-procedure-lines-of) (new-value (self postscript-canvas))
  (setf (max-procedure-lines-of (postscript-info-of self)) new-value))

(defmethod ps-width-of ((self postscript-canvas))
  (if (ps-width-of (postscript-info-of self))
    (ps-width-of (postscript-info-of self))
    (ps-canvas-width self)))

(defmethod (setf ps-width-of) (new-value (self postscript-canvas))
  (setf (ps-width-of (postscript-info-of self)) new-value))

(defmethod ps-height-of ((self postscript-canvas))
  (if (ps-height-of (postscript-info-of self))
    (ps-height-of (postscript-info-of self))
    (ps-canvas-height self)))

(defmethod (setf ps-height-of) (new-value (self postscript-canvas))
  (setf (ps-height-of (postscript-info-of self)) new-value))

(defmethod write-directory-of ((self postscript-canvas))
  (write-directory-of (postscript-info-of self)))

(defmethod (setf write-directory-of) (new-value (self postscript-canvas))
  (setf (write-directory-of (postscript-info-of self)) new-value))

(defmethod body-file-of ((self postscript-canvas))
  (body-file-of (postscript-info-of self)))

(defmethod (setf body-file-of) (new-value (self postscript-canvas))
  (setf (body-file-of (postscript-info-of self)) new-value))

(defun body-full-pathname (postscript-info)
  "Returns the full pathname of the body file of postscript-info."
  (prepend-write-directory postscript-info
               (body-file-of postscript-info)))

(defmethod header-file-of ((self postscript-canvas))
  (header-file-of (postscript-info-of self)))

(defmethod (setf header-file-of) (new-value (self postscript-canvas))
  (setf (header-file-of (postscript-info-of self)) new-value))

(defun prepend-write-directory (postscript-info file-name)
  "Prepends the write-directory name of postscript-info to the file-name.  ~
   Postscript-info can be a canvas."
  (concatenate 'string (write-directory-of postscript-info)
               file-name))

(defun header-full-pathname (postscript-info)
  "Returns the full pathname of the header file of postscript-info."
  (prepend-write-directory postscript-info
               (header-file-of postscript-info)))

(defmethod postscript-file-of ((self postscript-canvas))
  (postscript-file-of (postscript-info-of self)))

(defmethod (setf postscript-file-of) (new-value (self postscript-canvas))
  (setf (postscript-file-of (postscript-info-of self)) new-value))

(defun postscript-file-full-pathname (postscript-info)
  "Returns the full pathname of the self-contained ~
   postscript-file of postscript-info."
  (prepend-write-directory postscript-info
               (postscript-file-of postscript-info)))

(defmethod included-of ((self postscript-canvas))
  (included-of (postscript-info-of self)))

(defmethod (setf included-of) (new-value (self postscript-canvas))
  (setf (included-of (postscript-info-of self)) new-value))

(defmethod ps-x-origin-of ((self postscript-canvas))
  (ps-x-origin-of (postscript-info-of self)))

(defmethod (setf ps-x-origin-of) (new-value (self postscript-canvas))
  (setf (ps-x-origin-of (postscript-info-of self)) new-value))

(defmethod ps-y-origin-of ((self postscript-canvas))
  (ps-y-origin-of (postscript-info-of self)))

(defmethod (setf ps-y-origin-of) (new-value (self postscript-canvas))
  (setf (ps-y-origin-of (postscript-info-of self)) new-value))

(defmethod ps-x-scale-of ((self postscript-canvas))
  (ps-x-scale-of (postscript-info-of self)))

(defmethod (setf ps-x-scale-of) (new-value (self postscript-canvas))
  (setf (ps-x-scale-of (postscript-info-of self)) new-value))

(defmethod ps-y-scale-of ((self postscript-canvas))
  (ps-y-scale-of (postscript-info-of self)))

(defmethod (setf ps-y-scale-of) (new-value (self postscript-canvas))
  (setf (ps-y-scale-of (postscript-info-of self)) new-value))

(defmethod ps-horizontal-margin-of ((self postscript-canvas))
  (ps-horizontal-margin-of (postscript-info-of self)))

(defmethod (setf ps-horizontal-margin-of) (new-value (self postscript-canvas))
  (setf (ps-horizontal-margin-of (postscript-info-of self)) new-value))

(defmethod ps-vertical-margin-of ((self postscript-canvas))
  (ps-vertical-margin-of (postscript-info-of self)))

(defmethod (setf ps-vertical-margin-of) (new-value (self postscript-canvas))
  (setf (ps-vertical-margin-of (postscript-info-of self)) new-value))

(defmethod postscript-canvas-p ((pc postscript-canvas))
  (eq (display-mode-of pc) :postscript))

(defun canvas-to-ps-x (canvas x)
  (round (+ (ps-x-origin-of canvas) (* (ps-x-scale-of canvas) x))))

(defun canvas-to-ps-y (canvas y)
  (round (+ (ps-y-origin-of canvas) (* (ps-y-scale-of canvas) y))))

(defun ps-canvas-height (canvas)
  (ceiling (* (ps-y-scale-of canvas) (canvas-height canvas))))

(defun ps-canvas-width (canvas)
  (ceiling (* (ps-x-scale-of canvas) (canvas-width canvas))))

(defmethod multiple-pages-p ((self postscript-canvas))
  (multiple-pages-p (postscript-info-of self)))

(defmethod (setf multiple-pages-p) (new-value (self postscript-canvas))
  (setf (multiple-pages-p (postscript-info-of self)) new-value))

(defmethod center-vertical-p ((self postscript-canvas))
  (center-vertical-p (postscript-info-of self)))

(defmethod (setf center-vertical-p) (new-value (self postscript-canvas))
  (setf (center-vertical-p (postscript-info-of self)) new-value))

(defmethod center-horizontal-p ((self postscript-canvas))
  (center-horizontal-p (postscript-info-of self)))

(defmethod (setf center-horizontal-p) (new-value (self postscript-canvas))
  (setf (center-horizontal-p (postscript-info-of self)) new-value))

(defun canvas-to-ps-pen-width (canvas width)
  (declare (ignore canvas))
  (1- width))

(defun canvas-to-ps-red (canvas red)
  (declare (ignore canvas)
           ;;(special *max-color-saturation*)
           )
  (float (/ (truncate (* red 100000)) 100000))
  ;;(float (/ (truncate (* (float (/ red *max-color-saturation*)) 100000)) 100000))
  )

(defun canvas-to-ps-green (canvas green)
  (declare (ignore canvas)
           ;;(special *max-color-saturation*)
           )
  (float (/ (truncate (* green 100000)) 100000))
  ;;(float (/ (truncate (* (float (/ green *max-color-saturation*)) 100000)) 100000))
  )

(defun canvas-to-ps-blue (canvas blue)
  (declare (ignore canvas)
           ;;(special *max-color-saturation*)
           )
  (float (/ (truncate (* blue 100000)) 100000))
  ;;(float (/ (truncate (* (float (/ blue *max-color-saturation*)) 100000)) 100000))
  )
