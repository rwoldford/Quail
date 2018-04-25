;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               display.lsp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;  
;;;
;;;  Authors:
;;;     R.W. Oldford 1993
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :quail)

(export  '(display))

(defmethod display ((model generalized-linear-model) &rest grid-plot-args
                    &key
                    (draw? T)
                    (color wb::*white-color*)
                    (title NIL)
                    &allow-other-keys)
  (unless title
    (setf title
          (format NIL "Model summary: ~s." (class-name (class-of model)))))
  (let*
    ((link (link-of model))
     (formula (formula-of model))
     (family (family-of model))
     (formula-label (label :viewed-object "Formula:" :draw? NIL))
     (link-label (label :viewed-object "Link:" :draw? NIL))
     (family-label (label :viewed-object "Family:" :draw? NIL))
     (formula-value (text-view :viewed-object (literal-of formula) :draw? NIL))
     (link-value (text-view :viewed-object (name-of link) :draw? NIL))
     (family-value (text-view :viewed-object (name-of family) :draw? NIL))
     (view-layout (grid-layout :subviews (list formula-label formula-value
                                               link-label link-value
                                               family-label family-value
                                               )
                               :nrows 3  :box-views? NIL
                               :gap-x 0.1 :gap-y 0.1)))
         (apply #'grid-plot :interior-view view-layout :gap-x 0.1 :gap-y 0.1
                :title title
                :draw? draw?
