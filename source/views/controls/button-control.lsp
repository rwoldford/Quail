;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               button-control.lsp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;
;;;  This file is part of the Views system, a toolkit for interactive statistical graphics.
;;;  
;;;
;;;  Authors:
;;;     N. Wiebe 1998-1999
;;;     C.B. Hurley 1988-1992 George Washington University
;;;     R.W. Oldford 1988-1991
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :views)
(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(control-mixin control-start control-done)))



;;----------------------------------------------------------------------



(defclass control-mixin (  )
  ((target 
    :initarg :target 
    :initform nil 
    :accessor target-of)
   (control-state 
    :initarg :control-state 
    :initform nil 
    :accessor control-state
    :documentation "Non NIL when control is here; NIL otherwise.")
   (control-toggle 
    :initarg :toggle? 
    :initform T 
    :accessor toggle-control
    :documentation "Non NIL when control is to be toggled after the ~
                    left function is fired, NIL otherwise."))
  (:default-initargs :left-fn nil )
  (:documentation "A control-mixin applies its left-fn to targets on a left click"))

(defmethod initialize-instance :after ((self control-mixin) &key target)
  (unless (listp target)
    (setf (target-of self) (list target))))


(defmethod (setf target-of) :after (target (self control-mixin))
  (unless (listp target)
      (setf (slot-value self 'target) (list target))))


(defmethod control-start ((self control-mixin))
  (cond
   ((viewports-of self)
    (erase-view self)
    (setf (control-state self) T)
    (draw-view self))
   (T 
    (setf (control-state self) T))))

(defmethod control-done ((self control-mixin))
  (cond
   ((viewports-of self)
    (erase-view self)
    (setf (control-state self) NIL)
    (draw-view self))
   (T 
    (setf (control-state self) NIL)))
  )

;; making this before or after would be nicer, but somehow the clicks get confused
(defmethod left-button-fn  ((self control-mixin) 
                            &key viewport ) 
  
  (let* ((lbf (left-fn-of self))
         (args (mapcar #'eval  (target-of self)))
         (vlist (member :viewport args)))
    (if  vlist  (push viewport (cdr  vlist)))
    (control-start self)
    (unwind-protect 
      (if lbf (apply lbf args))
      (if (toggle-control self)
        (control-done self))
      )
    )
  )

 
(defun other-colour-function (self)
  (let (temp-colour)
    (setf temp-colour (wb::prompt-user-for-color))
    (set-drawing-style self :button-color temp-colour)
    ))

(defun button-color-menu-items ()
  (declare (special  *color-menu-list* *shade-menu-list*))
    (loop for c in 
          (if (wb:color-device-p)
            *color-menu-list* *shade-menu-list*)
          collect 
          (if (eq (cadr c) :prompt)
            (list (car c) `(other-colour-function))
            (list (car c) `(set-drawing-style :button-color ,(cadr c))))))


;;; should the following three functions become methods for each button
;;; because of the different slot names for the elliptical button?

(defun button-size-function (self h w smaller)
  (let* ((increment 5)
         (elliptical-button (eq (type-of self) 'elliptical-button))
         (mheight (if elliptical-button 
                    (diameter-y self) (max-height self)))
         (mheight (if (eq mheight :viewport)
                   (height-of (viewport-of self)) mheight))
         (height (if h 
                     (if smaller  ;otherwise larger
                       (- mheight increment) (+ mheight increment))
                     mheight))
         (mwidth (if elliptical-button
                   (diameter-x self) (max-width self)))
         (mwidth (if (eq mwidth :viewport)
                   (width-of (viewport-of self)) mwidth))
         (width (if (eq mwidth :viewport)
                  :viewport
                  (if w                                
                    (if smaller   ;otherwise larger
                      (- mwidth increment) (+ mwidth increment))
                    mwidth))))
    (erase-view self)
    (cond (elliptical-button    ;not larger than the viewport
           (progn (if (and h (< height (height-of (viewport-of self))))
                    (setf (diameter-y self) height))
                   (if (and w (< width (width-of (viewport-of self))))
                     (setf (diameter-x self) width))))
          (T (progn (if (and h (< height (height-of (viewport-of self))))
                      (setf (max-height self) height))      
                     (if (and w (< width (width-of (viewport-of self))))
                       (setf (max-width self) width)))))
    (draw-view self)))

(defun button-size-function2 (self h w)
  (let* ((elliptical-button (eq (type-of self) 'elliptical-button))
        (mheight (if elliptical-button (diameter-y self) (max-height self)))
        (mwidth (if elliptical-button (diameter-x self) (max-width self)))
        (height (if h (wb::prompt-user :prompt-string 
                                       (format NIL "Enter height (currently ~s):" 
                                               (if (eq mheight :viewport)
                                                 (height-of (viewport-of self))
                                                 mheight))
                                 :result-type 'integer 
                                 :read-type :eval)))
        (width (if w (wb::prompt-user :prompt-string 
                                      (format NIL "Enter width (currently ~s): " 
                                              (if (eq mwidth :viewport)
                                                (width-of (viewport-of self))
                                                mwidth))
                                :result-type 'integer
                                :read-type :eval))))
    (erase-view self)
    (cond (elliptical-button    ;not larger than the viewport
           (progn (if (and h (< height (height-of (viewport-of self))))
                    (setf (diameter-y self) height))
                   (if (and w (< width (width-of (viewport-of self))))
                     (setf (diameter-x self) width))))
          (T (progn (if (and h (< height (height-of (viewport-of self))))
                      (setf (max-height self) height))      
                     (if (and w (< width (width-of (viewport-of self))))
                       (setf (max-width self) width)))))
    (draw-view self)
    ))

(defun button-size-function3 (self h w)
  (erase-view self)
  (cond ((eq (type-of self) 'elliptical-button)
         (progn (if h (setf (diameter-y self) (height-of (viewport-of self))))
                (if w (setf (diameter-x self) (width-of (viewport-of self))))))
        (T (progn (if h (setf (max-height self) (height-of (viewport-of self))))
                  (if w (setf (max-width self) (width-of (viewport-of self)))))))
  (draw-view self))

(defun button-size-menu-items ()
  (let* ((button-size-menu-subitems1 
          `(("smaller" (button-size-function T T T))
            ("larger" (button-size-function T T NIL))
            ("prompt" (button-size-function2 T T))
            ("viewport" (button-size-function3 T T))
            ))
         (button-size-menu-subitems2 
          `(("smaller" (button-size-function NIL T T))
            ("larger" (button-size-function NIL T NIL))
            ("prompt" (button-size-function2 NIL T))
            ("viewport" (button-size-function3 NIL T))
            ))
         (button-size-menu-subitems3 
          `(("smaller" (button-size-function T NIL T))
            ("larger" (button-size-function  T NIL NIL))
            ("prompt" (button-size-function2 T NIL))
            ("viewport" (button-size-function3 T NIL))
            )))
    
    `(("Size" nil "" :sub-items ,button-size-menu-subitems1)
      ("Width" nil "" :sub-items ,button-size-menu-subitems2)
      ("Height" nil "" :sub-items ,button-size-menu-subitems3))))

;does control-mixin have any subclasses other than button subclasses?
(defmethod style-menu-items :around ((self control-mixin))
  (let ((result (call-next-method)))
      (add-menu-items self `(( "Button Color" nil "" :sub-items ,(button-color-menu-items))
                             ( "Button Size" nil "" :sub-items ,(button-size-menu-items)))
                      result)
      ))
         

#|
(defmethod draw-view :before ((self button-color-mixin) &key viewport)
  (if (has-draw-style-p self :button-color)
    (let ((bg-color (draw-style self :button-color)))   
      (with-exposed-viewports self viewport vp 
        (unless (wb:eq-colors bg-color (wb:canvas-background-color (window-of vp)))
          (fill-viewport vp :color bg-color)
          )))))
 |#

  

  
#|



(defmethod ctrl-left-button-fn ((self control-mixin) 
                                &key viewport )
  (declare (ignore viewport )))

(defmethod ctrl-middle-button-fn ((self control-mixin) 
                                  &key viewport )
  (declare (ignore viewport )))

(defmethod ctrl-right-button-fn ((self control-mixin) 
                                 &key viewport)
  (declare (ignore viewport )))

(defmethod shift-left-button-fn ((self control-mixin) 
                                 &key viewport )
  (declare (ignore viewport)))
 

(defmethod shift-middle-button-fn ((self control-button) 
                                   &key viewport )
  (declare (ignore viewport )))

(defmethod shift-right-button-fn ((self control-button) 
                                  &key viewport )
  (declare (ignore viewport )))

|#
