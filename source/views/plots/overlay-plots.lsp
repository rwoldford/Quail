;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               overlay-plots.lisp
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
;;;     C.B. Hurley 1992 George Washington University
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------
(in-package :views)
(eval-when (:compile-toplevel :load-toplevel :execute) (export '( overlay-plot )))




(defun overlay-plot (&rest keyword-pairs
                            &key 
                            (initform-fn #'get-batch-inits)
                            (cases #'list-cases) (title t)
                             (colors (mapcar #'second *color-menu-list*))
                             (plot-fn #'2d-plot) (draw? t) (by :prompt)
                             case-views-from
                            &allow-other-keys)
  
  
  
  (setq keyword-pairs (append (apply initform-fn keyword-pairs) keyword-pairs))
  
  (disable-keyword keyword-pairs :initform-fn)
  (disable-keyword keyword-pairs :type) 
  (let* ( (dataset (getf keyword-pairs :data))
         (groups (get-batches :batches (getf keyword-pairs :batches)
                              :by by
                              :order-levels (getf keyword-pairs :order-levels)
                              :level-function (or (getf keyword-pairs :level-function)
                              (getf keyword-pairs :level-functions))
                              :dataset dataset :cases cases)) 
         (p (apply plot-fn
                   :title (cond ((eq title t) (list :data dataset))
                                ((legal-label-construct-p title )
                                 
                                (append (sublabel-arg-list title)
                                                           (list :data dataset)))
                                (t title))
                               
                   :top-label (loop repeat (length groups) 
                                    collect (view :type 'data-label))
                   :data (first groups)
                   :color (first colors)
                   :draw? nil
                   :case-views-from case-views-from
                   keyword-pairs))
         (fs (mapcar #'clone-view-fn (interior-views-of p)))) 
    (loop with int = (interior-views-of p)
          for l in (top-labels-of p)
          for b in groups
          for c in colors
          do
          (loop for i in int
                 do (text-unlink i l))
                
          (setf (viewed-object-of l) b)
          (setf (text-of l) (dataset-name b))
          (set-drawing-style l :color c))
   
    (loop  with i = (interior-view-of p)
           with var-args = (var-info i nil)
            for b in (cdr groups)
           for c in (cdr colors) do
           
           (loop for f in fs
                 for new =
                 (apply  f :data b 
                         :cases cases
                         :case-views-from case-views-from
                          var-args ) do
                 
                 (set-drawing-style  new :color c :draw? nil)
                 (layer-subview p new i :draw? nil :clip-region :max)))
    (if draw? (draw-view p))
    p))



#|


(setq foo (list (sub-cases (car (list-drills assay)))
                              (sub-cases (fifth (list-drills assay)))))
(length foo)
(overlay-plot :data vw::assay
             :batches (list (sub-cases (car (list-drills assay)))
                              (sub-cases (fifth (list-drills assay))))
            ;; :colors (list wb:*white-color* wb:*light-gray-color*)
           )

(overlay-plot :data vw::assay
             :batches (list (car (list-drills assay))
                              (fifth (list-drills assay)))
             :cases #'sub-cases
            ;; :colors (list wb:*white-color* wb:*light-gray-color*)
           )


try scatmat
varinfor for scatmat




|#
