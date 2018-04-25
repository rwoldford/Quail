;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               labels                              
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (C) 1994 Statistical Computing Laboratory, University Of Waterloo
;;;
;;; 
;;;  Authors:
;;;     C.B. Hurley 1994.
;;;     R.W. Oldford 1994.
;;;
;;;
;;;
;;; Make a label. It is not drawn automatically.
;;;

(setq lab (label :text "Hello world"))
      
;;; To draw it:
(draw-view lab)

;;; Erase it
(erase-view lab)

;;; and draw it again
(draw-view lab)

;;; Now in a different window 
(draw-view lab :viewport (make-viewport))


;;; or


(setq lab1 (label :text "Hello world" :draw? t))

(set-text lab1 "A new title.")

;;; A label has drawing styles for
(style-keys-of lab1)
;;; Like all views it also has drawing style :highlight? and :invisible?


;;; To check on the current style values do
(drawing-style-of lab1)

;;; To obtain on a particular style value do
(draw-style lab1 :color)

;;; To change the color do:
(set-drawing-style lab1 :color wb:*yellow-color*)
(set-drawing-style lab1 :color :prompt)

(set-view-font lab1
               :name "Helvetica"
               :size 12
               :style :bold)
(set-view-font lab1
               :size :bigger
               :style :italic)
(set-view-font lab1
               :name "Times"
               :size :smaller
               :style :bold)

;;; or via set-drawing-style
;;;

(set-drawing-style lab1 :font :smaller)
(set-drawing-style lab1 :font :bigger)

;;; Many styles can be changed with a single command:

(set-drawing-style lab1 :font :smaller :color wb:*red-color*)

;;;
;;; Make the label invisible
;;;

(set-drawing-style lab1 :invisible? T)
(set-drawing-style lab1 :invisible? NIL)
(set-drawing-style lab1 :invisible? :toggle)


;;; A label can have horizontal or vertical orientation:

(set-orientation   lab1 :vertical :draw? t)
(set-orientation  lab1 :horizontal :draw? t)

(set-orientation  lab1 :toggle :draw? t)

;;; It can also have various justifications:

(list-legal-justifications lab1)

(loop for j in (list-legal-justifications lab1) do
      (set-justification  lab1 j :draw? t)
      (sleep .3))

;;; If the text is too long, some of the text may not be drawn:

(setq lab1 (label :text "(loop for j in (list-legal-justifications lab1) do
      (set-justification  lab1 j :draw? t)
      (sleep .3))" :draw? t))

;;; In this case, it may be better to use a text-view,
;;; which permits the text to be spread over many lines:


(setq te1 (text-view :text "(loop for j in (list-legal-justifications lab1) do
      (set-justification  lab1 j :draw? t)
      (sleep .3))" :draw? t))

;;; Notice, when the window is made smaller, the text  simply wraps around
;;; onto the next line.

;;; With text-views, one can change drawing styles just as for labels,
;;; but the orientation and justification cannot be changed.


