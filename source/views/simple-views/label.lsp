;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               label.lisp
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
;;;     C.B. Hurley 1988-1991 George Washington University
;;;     R.W. Oldford 1988-1991
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :views)

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '( get-text data-label
            label set-text
            )))
;;;----------------------------------------------------------------------------------
(defclass label (linkable-mixin justification-mixin orientation-mixin simple-view )
  ((text  
    :initarg :text
    :accessor text-of 
    :initform nil
    :documentation "text for printing")
   (clip-label? 
    :initarg :clip-label? 
    :accessor clip-label-p
    :initform nil
    :documentation "text for printing")
   (style-keys :initform '(:font :color) :allocation :class)
   (middle-menu :allocation :class :initform nil))
  (:default-initargs :font *default-label-font*
                     :justification :center
                     :orientation :horizontal
                     :linkable? nil))

(defmethod private-styles-of ((self label))
  (or (slot-value self 'private-styles)
      (setf (slot-value self 'private-styles)
            '(:font ))))
#|
(defclass linkable-label (label ) ()
  (:default-initargs :viewed-elements :expand-viewed-object
     :linkable? t))
|#

(defclass data-label (label) ()
   (:default-initargs 
     :linkable-styles '(:highlight? :invisible? :color)
     :linkable? t
     :viewed-elements :single))

(defclass group-label (label ) ()
   (:default-initargs :viewed-elements :expand-viewed-object
     :linkable? t))

;;;----------------------------------------


(defmethod draw-view :before ((self label) 
                              &key text  viewport)
  (declare (ignore viewport))
  
  (when (eq text :prompt)
    ;;(format *quail-query-io* "~&New text for ~S :" self)
    ;;(setq text (string (read *quail-query-io*))))
    (erase-view self)
    (setq text (wb:prompt-user :result-type 'string
                               :prompt-string "Enter text")))
  (if text (setf (text-of self) text))
  )


;; adjusted to take color 11/95
(defmethod draw-view ((self label) &key viewport color)
  (let ((just (justification-of self))
        (dir (orientation-of self))
        (font (draw-style self :font))
        (clip (clip-label-p self))
        (col (or color (draw-style self :color)))
        (string (get-text self))
        size)
   
    
      
    (if (null dir)
      (let ((vp (car (viewports-of self))))
        (setq dir (if (> (height-of vp) (width-of vp))
                    :vertical
                    :horizontal))))
     (unless (wb:canvas-font-p font)
       (let ((vp (car (viewports-of self))))
       (setq size
             (max 6
                  (min 16
                       (if (eq dir :horizontal)
                         (truncate (min (* 0.5  (height-of vp)) (/ (width-of vp) 
                                                                   (max 3 (length string)))))
                         (truncate (min (* 0.5  (width-of vp)) 
                                        (/ (height-of vp) (max 3 (* 1.2 (length string))))))))))
      (setq font
            (wb:canvas-make-font
               :name nil
               :size
               size))))
    (with-exposed-viewports self viewport vp
      (if (> (min (height-of vp) (width-of vp)) 5)
      (wb:canvas-draw-string (window-of vp)
                               string
                               :region (wb-region vp) :font font
                               :orientation dir
                               :clip? clip
                               :justification just
                               :color col)))))



(defmethod set-text ((self label) text)
  (if (viewports-of self)
    (progn
      (erase-view self)
      (setf (text-of self) text)
      (draw-view self))
    (setf (text-of self) text)
    ))

(defmethod text-links-of ((self label))
(loop for v in (link-bounds-of self)
      when (typep v 'label) collect v))


(defmethod update-text-links ((self label))
  (let ((views (text-links-of self))
        (s (get-text self)))

    (loop for v in views 
            unless (string-equal s (get-text v)) do
              (set-text v s))))
      
(defmethod get-menu-items ((self label) (slot-name (eql 'middle-menu)))
  `(( "NewText"  (draw-view :text :prompt))))
        

(defmethod style-menu-items ((self label))
  (font-menu-items))
                                  
   
(defmethod highlight-operation ((self label))
  ;; maybe only this if linkable?
  :boole-xor
 )

(defmethod get-text ((self label))
  (let ((text (text-of self))
        (vo (viewed-object-of self)) str)
    (cond ((stringp text)
           text)
          ((and (functionp text) (setq str (funcall text vo)))
           (princ-to-string str))
          ((and text (listp text))
           (format nil "~{~A ~}" text))
          ((and text (symbolp text))
           (princ-to-string text))
          ((stringp vo)
           vo)
          ((symbolp vo)
           (princ-to-string vo))
          ((dataset-p vo)
           (dataset-name vo))
          
          (t ""))))



(defmethod list-legal-justifications ((self label))
  '((:left :top) (:left :center) (:left :bottom)
    (:center :top) (:center :center) (:center :bottom)
    (:right :top) (:right :center) (:right :bottom)))
    
    
