;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               barss                              
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (C) 1995 Statistical Computing Laboratory, University Of Waterloo
;;;
;;; 
;;;  Authors:
;;;     C.B. Hurley 1995.
;;;     R.W. Oldford 1995.
;;;
;;;
;;;
;;; Make a bar and draw it.
;;;

(setq bb (bar :draw? t))

;;; A bar has drawing styles for (style-keys-of lab1)
;;; Like all views it also has drawing style :highlight? and :invisible?

;;; See (edit-file "q:Examples;Views;Basics;drawing-styles.lsp") for some examples of how to
;;; examine and change drawing styles. 

(draw-style bb :fill?)

;;; Actually, different portions of a bar can have different drawing styles.
;;; One way to do this is to first set up the bar with  viewed elements
;;; being a list of items.
;;; For example:

(setq bb (bar :draw? t :viewed-elements '(a b c)))

;;; Now make bb a solid green bar..
(set-drawing-style bb :color wb:*green-color* :fill? t)

;;; To change the color of part of bb do:

(set-drawing-style bb :element 'b :color wb:*white-color* :test #'eql)

;;; Now bb is two parts green, one part white, but the two green parts
;;; are drawn contiguously.

;;; To make bb drawn as green-white-green do

(set-collect-styles-p bb nil :draw? t)

;;; Now set the c-part of bb to "gold"

(set-drawing-style bb :element 'c :color wb:*orange-color* :test #'eql)

;;; For a more accurate rendition of "gold"...
(set-drawing-style bb :element 'c :color :prompt :test #'eql)

;;; And if you prefer, change the direction of striping...

(set-orientation bb :toggle :draw? t)

