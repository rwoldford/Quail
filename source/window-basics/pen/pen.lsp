;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                          pen.lisp
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
;;;      R.W. Oldford 1989-1992
;;;     
;;;
;;;
;;;
(in-package :wb)

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(pen pen-width-of pen-color-of
           pen-operation-of pen-texture-of with-pen-values with-pen-width
           with-pen-operation with-pen-color with-pen-values-restored)))

(defclass pen ()
  ((pen-width :initarg :pen-width
              :initform NIL
              :reader pen-width-of
              :documentation "Current width of the pen used to draw in this canvas.")
   (pen-point :initarg :pen-point
              :initform NIL
              :reader pen-point-of
              :documentation "Current point of the pen used to draw in this canvas.")
   (pen-operation :initarg :pen-operation
              :initform NIL
              :reader pen-operation-of
              :documentation "Current operation of the pen used to draw in this canvas.")
   (pen-color :initarg :pen-color
              :initform NIL
              :reader pen-color-of
              :documentation "Current color of the pen used to draw in this canvas.")
   (pen-texture :initarg :pen-texture
              :initform NIL
              :reader pen-texture-of
              :documentation "Current texture of the pen used to draw in this canvas."))
  (:documentation
   "A pen to be used to draw in a canvas."))




;#-:sbcl-linux 
(defmacro with-pen-width (canvas width &body forms)
  "Performs the forms with the pen width of canvas temporarily reset to ~
   the value given.  If width is NIL, width is ignored."
  (let ((old-width (gensym "with-pen-width")))
    `(let (,old-width)
       (declare (ignorable ,old-width))
       (cond
        ((not (null ,width))
         (setf ,old-width (pen-width-of ,canvas))
         (set-pen-width ,canvas ,width)
         ,@forms
         (set-pen-width ,canvas ,old-width))
        (T ,@forms)))))

#|
#+:sbcl-linux (defmacro with-pen-width (canvas width &body forms)
  "Performs the forms with the pen width of canvas temporarily reset ot ~
  the value given. If width is NIL, width is ignored."
  `(let ((drawing-pane (get-frame-pane ,canvas 'host-pane)))
    (cond ((not (null ,width))
      (with-drawing-options (drawing-pane :line-thickness ,width) ,@forms))
    (T ,@forms))))
|#
;#-:sbcl-linux 
(defmacro with-pen-color (canvas color &body forms)
  "Performs the forms with the pen color of canvas temporarily reset to ~
   the value given.  If color is NIL, color is ignored."
  (let ((old-color (gensym "with-pen-color")))
    `(let (,old-color)
       (declare (ignorable ,old-color))
       (cond
        ((not (null ,color))
         (setf ,old-color (pen-color-of ,canvas))
         (set-pen-color ,canvas ,color)
         ,@forms
         (set-pen-color ,canvas ,old-color))
        (T ,@forms)))))

#|
#+:sbcl-linux (defmacro with-pen-color (canvas color &body forms)
  "Performs the forms with the pen color of canvas temporarily reset to ~
   the value given.  If color is NIL, color is ignored."
   `(let ((drawing-pane (get-frame-pane ,canvas 'host-pane)))
    (cond
      ((not (null ,color))
        (with-drawing-options (drawing-pane :ink ,color) ,@forms))
      (T ,@forms)))
  )
|#
;#-:sbcl-linux 
(defmacro with-pen-operation (canvas operation &body forms)
  "Performs the forms with the pen operation of canvas temporarily reset to ~
   the value given.  If operation is NIL, operation is ignored."
  (let ((old-operation (gensym "with-pen-operation")))
    `(let (,old-operation)
       (declare (ignorable ,old-operation))
       (cond
        ((not (null ,operation))
         (setf ,old-operation (pen-operation-of ,canvas))
         (set-pen-operation ,canvas ,operation)
         ,@forms
         (set-pen-operation ,canvas ,old-operation))
        (T ,@forms)))))

#|             
#+:sbcl-linux (defmacro with-pen-operation (canvas operation &body forms)
   "Performs the forms with the pen operation of canvas temporarily reset to ~
   the value given.  If operation is NIL, operation is ignored."
   (declare (ignore operation))
  `(,@forms)
  )           
|#       
#|
(defmacro with-pen-values (canvas color width operation &body forms)
  "Executes the forms with the pen values of canvas temporarily reset to ~
   the values of color width and operation.  Null values are ignored."
  `(with-pen-width ,canvas ,width
     (with-pen-color ,canvas ,color
       (with-pen-operation ,canvas ,operation
         ,@forms))))
|#

;;; new version 08AUG2023
(defmacro with-pen-values (canvas color width operation &body forms)
  (let ((old-color (gensym "with-color"))
        (old-width (gensym "with-width"))
        (old-operation (gensym "with-operation")))
    `(let ((,old-color) (,old-width) (,old-operation))
    (declare (ignorable ,old-color ,old-width ,old-operation))
       (setf ,old-color (pen-color-of ,canvas))
       (setf ,old-width (pen-width-of ,canvas))
       (setf ,old-operation (pen-operation-of ,canvas))
    (cond
      ((not (null ,color))
       (set-pen-color ,canvas ,color)
       ))
    (cond
      ((not (null ,width))
       (set-pen-width ,canvas ,width)
       ))
    (cond
      ((not (null ,operation))
       (set-pen-operation ,canvas ,operation)
       ,@forms   
       )
      (T ,@forms
         ))
       (set-pen-color ,canvas ,old-color)
       (set-pen-width ,canvas ,old-width)
       (set-pen-operation ,canvas ,old-operation)
    )))



(defmacro with-pen-values-restored (canvas &body forms)
  "Saves the pen values of canvas, performs the forms, then ~
   resets the pen values to their original values."
  
  (let ((old-width (gensym "with-pen-values-restored"))
        (old-op    (gensym "with-pen-values-restored"))
        (old-col   (gensym "with-pen-values-restored")))
    `(let
       ((,old-width (pen-width-of ,canvas))
        (,old-col (pen-color-of ,canvas))
        (,old-op (pen-operation-of ,canvas)))
       ,@forms
       (set-pen-width ,canvas ,old-width)
       (set-pen-color ,canvas ,old-col)
       (set-pen-operation ,canvas ,old-op))))
