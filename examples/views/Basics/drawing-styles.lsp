;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               drawing-styles                            
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
;;; Make a view. It is not drawn automatically.
;;;


(setq v (view))

;;; To draw the view...

;;; Since this is just a vanilla view, all you get is an empty viewport.

(draw-view v)


;;; The basic view can be highighted or not, invisible or not.
;;; These properties of a view are called its drawing style.
;;; Some kinds of views have more detailed drawing styles,
;;; but highlight and invisible styles are common to all views.

;;; To examine its drawing style do

(drawing-style-of v)

;;; To highlight it do

(set-drawing-style v :highlight? t)

;;; The value of *default-highlight-color* determines the highlight color.

;;; One can of course change this..

(setq *default-highlight-color* wb:*orange-color*)

;;; Or, for something more exotic,

(setq *default-highlight-color* (wb:prompt-user-for-color))

;;; Let's make a view that actually produces something on the screen!
(setq v (point-symbol :draw? t))  ; or any other kind of view.

;;; By default, only views of type plot are automatically drawn, so we need :draw? to
;;; draw the view.

;;; All views have drawing styles highlight and invisible.
;;; These values can also be set from the view's middle menu.
;;; See (edit-file "q:Examples;Views;Basics;mouse.lsp") for details on the menus.

(set-drawing-style v :highlight? t)
(set-drawing-style v :highlight? nil)
(set-drawing-style v :highlight? :toggle)

(set-drawing-style v :invisible? t)
(set-drawing-style v :invisible? nil)
(set-drawing-style v :invisible? :toggle)

;;; Any drawing style which takes values t or nil (for on and off)
;;; can also be toggled, as above.

;;; Notice an invisble view can still be highlit.



;;; Most simple views have a drawing style for color.
;;; To get a list of the style keys of v do 

(style-keys-of v)

;;; Like all views it also has drawing style :highlight? and :invisible?.


;;; To check on the current style values do
(drawing-style-of v)

;;; To obtain on a particular style value do
(draw-style v :color)

;;; To change the color do:
(set-drawing-style v :color wb:*yellow-color*)
(set-drawing-style v :color :prompt)


;;; Drawing styles can also be set from the view's middle menu.

;;;--------multiple drawing styles---------------------------------
;;; Some views have multiple drawing styles, which means different parts
;;; of the view can be colored ( highlit, invisible..) individually.

(list-view-classes 'multiple-draw-style-mixin)


;;; Let's make one of these.

(setq v (bar :draw? t))

(set-drawing-style v :fill? t :color wb:*white-color*)

;;; To give different portions of a bar  different drawing styles.
;;; first set up the bar with  viewed elements
;;; being a list of items.
;;; For example:

(setq v (bar :draw? t :viewed-elements '(a b c)))

;;; Now make v a solid green bar..
(set-drawing-style v :color wb:*green-color* :fill? t)

;;; To change the color of part of v do:

(set-drawing-style v :element 'b :color wb:*white-color* :test #'eql)

;;; Now v is two parts green, one part white, but the two green parts
;;; are drawn contiguously.

;;; To make v drawn as green-white-green do

(set-collect-styles-p v nil :draw? t)

;;; Now set the c-part of v to "gold"

(set-drawing-style v :element 'c :color wb:*orange-color* :test #'eql)

;;; For a more accurate rendition of "gold"...
(set-drawing-style v :element 'c :color :prompt :test #'eql)

;;; And if you prefer, change the direction of striping...

(set-orientation v :toggle :draw? t)

;;; Voila, the Irish flag!

