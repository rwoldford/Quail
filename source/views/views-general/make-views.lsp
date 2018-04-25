;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               make-views.lisp
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


(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(make-view-constructor
           make-view-constructor-with-doc
           make-view-constructor-fn 
           make-view-constructor-doc-string
           choose-view-from-menu *view-constructors*
           )))


(defun make-view-constructor-fn (default-class &rest default-args)
  "Returns a function which constructs a view ~
   of default class default-class. ~
   Default-args are passed on to the view construction."
  
  #'(lambda(&rest args)
      (let* ((type-arg (getf  args :type :none))
             (view-class (unless (eql :none type-arg)
                           (and (symbolp type-arg)
                                (or (get type-arg 'view-class)
                                    (and (subtypep type-arg 'view) type-arg)))))
             (new-args (append (if (eql :none type-arg) args
                                   (disable-keyword args :type))
                               default-args))
             (new-view (if (functionp type-arg) (apply type-arg new-args) )))
        
        (cond ((and new-view (typep new-view default-class)) new-view)
              ((and view-class (subtypep view-class default-class)
                    (fboundp type-arg))
               (apply (symbol-function type-arg) new-args))
              ((and view-class (subtypep view-class default-class))
               (apply #'make-instance view-class new-args))
              ((and (fboundp type-arg)
                    (setq new-view (apply (symbol-function type-arg) new-args))
                    (typep new-view default-class))
               new-view)
              (t (apply #'make-instance default-class
                        new-args))))))





(defun make-view-constructor-doc-string(default-class)
  (declare (ignore default-args))
  (format nil 
          "This function constructs and returns a ~A."
          default-class ))




(defvar *view-constructors* nil "A list of symbols naming functions which construct views")

(defun make-view-constructor(function-name default-class  doc &rest default-args)
  "Creates a function named function-name which constructs a view~
   of default class default-class. ~
   Default-args are passed on to the view construction. ~
   Function is exported"
  
  (eval-when (:compile-toplevel :load-toplevel :execute) (export `(,function-name)))
  (setf (documentation function-name 'function) doc)
  (setf (symbol-function function-name) 
        (apply #'make-view-constructor-fn default-class default-args))
  (setf (get function-name 'view-class) default-class)
  (pushnew function-name *view-constructors*)
  (symbol-function function-name))


(defun make-view-constructor-with-doc(function-name default-class &rest default-args)
  "Creates a function named function-name which constructs a view~
   of default class default-class. ~
   Default-args are passed on to the view construction. ~
   A documentation string for function-name is built.~
   Function is exported"
  
  (let ((doc (make-view-constructor-doc-string
              default-class)))
    
    (apply #'make-view-constructor 
           function-name default-class doc default-args)))



(defun choose-view-from-menu(&key (superclass 'view) menu-list
                                  (prompt-string "Select a view class"))
  "Gives the user a list of view classes which inherit ~
   from superclass  to choose from.~
   Superclass can also be a list of class names whose first ~
   member is and or or. " 
  (setq menu-list (or menu-list (select-view-menu-list superclass)))
  (if (= (length menu-list) 1)
    (cadar menu-list)
    (choose-views-from-menu :superclass superclass :prompt-string prompt-string
                            :menu-list  menu-list
                            :nmax 1)))



(defun choose-views-from-menu(&key (superclass 'view) menu-list
                                   (prompt-string "Select a view class") nmax)
  "Gives the user a list of view classes which inherit ~
   from superclass  to choose upto nmax views from. ~
   Superclass can also be a list of class names whose first ~
   member is and or or. "
  (setq menu-list (or menu-list (select-view-menu-list superclass)))
  (if (eq nmax 1)
    (loop for res = (cadar (wb:prompt-for-items menu-list :prompt-text prompt-string
                                                :item-function #'first))
          until res
          finally (return res))
    
    (loop append 
          (wb:prompt-for-items menu-list 
                               :prompt-text prompt-string
                               :item-function #'first
                               :selection-type :disjoint)
          into result-list
          until (or (null nmax) 
                    (and nmax (>= (length result-list) nmax)))
          finally (return (if (and nmax (> (length result-list) nmax))
                            (mapcar #'second (subseq result-list 0 nmax))
                            (mapcar #'second result-list))))))





(make-view-constructor-with-doc 'view 'view )
(make-view-constructor-with-doc 'point-symbol 'point-symbol )
(make-view-constructor-with-doc 'box 'point-symbol :symbol :box)
(make-view-constructor-with-doc 'circle 'point-symbol :symbol :circle)
(make-view-constructor-with-doc 'cross 'point-symbol :symbol :cross)
(make-view-constructor-with-doc 'diamond  'point-symbol :symbol :diamond )
(make-view-constructor-with-doc 'star 'point-symbol :symbol :star)
(make-view-constructor-with-doc 'poly-star 'point-symbol :symbol :poly-star)
(make-view-constructor-with-doc 'triangle 'point-symbol :symbol :triangle)
(make-view-constructor-with-doc 'bar 'bar )
(make-view-constructor-with-doc 'box 'box )
(make-view-constructor-with-doc 'pie 'pie )
(make-view-constructor-with-doc 'function-view 'function-view )
(make-view-constructor-with-doc 'label 'label )
(make-view-constructor-with-doc 'linkable-label 'label :linkable? t)
(make-view-constructor-with-doc 'group-label 'group-label )
(make-view-constructor-with-doc 'line-segment 'line-segment )
(make-view-constructor-with-doc  'line-segments 'line-segment )
(make-view-constructor-with-doc 'line 'line )
(make-view-constructor-with-doc 'horizontal-line 'line :orientation :horizontal )
(make-view-constructor-with-doc 'vertical-line 'line :orientation :vertical )
(make-view-constructor-with-doc 'text-view 'text-view )


(make-view-constructor-with-doc 'control-button 'control-button )
(make-view-constructor-with-doc 'rounded-button 'rounded-button )
(make-view-constructor-with-doc 'signpost-button 'signpost-button )
(make-view-constructor-with-doc 'elliptical-button 'elliptical-button )
(make-view-constructor-with-doc 'bar-slider 'bar-slider )
(make-view-constructor-with-doc 'needle-slider 'needle-slider )
(make-view-constructor-with-doc 'double-bar-slider 'double-bar-slider )
(make-view-constructor-with-doc 'double-needle-slider 'double-needle-slider )
(make-view-constructor-with-doc 'axis 'axis )
(make-view-constructor-with-doc 'fringe-view '1d-point-cloud :case-view '(:type oriented-line
                                                                          :orientation :vertical) )
(make-view-constructor-with-doc 'boxplot-view 'boxplot-view )
(make-view-constructor-with-doc 'interval-view 'interval-view )
(make-view-constructor-with-doc 'fitted-line 'fitted-line )
(make-view-constructor-with-doc 'histogram-view 'histogram-view )
(make-view-constructor-with-doc 'lines 'lines )
(make-view-constructor-with-doc 'simple-lines 'simple-lines )
(make-view-constructor-with-doc 'point-cloud '2d-point-cloud )
(make-view-constructor-with-doc '2d-point-cloud '2d-point-cloud )
(make-view-constructor-with-doc '2d-image '2d-image )
(make-view-constructor-with-doc 'line-segments-per-case 'line-segments-per-case )
(make-view-constructor-with-doc '1d-point-cloud '1d-point-cloud )
(make-view-constructor-with-doc 'smoothed-2d-point-cloud 'smoothed-2d-point-cloud )
(make-view-constructor-with-doc 'simple-smooth 'simple-smooth )
(make-view-constructor-with-doc 'smooth 'smooth )
(make-view-constructor-with-doc 'rotating-point-cloud 'rotating-cloud )
(make-view-constructor-with-doc 'simple-lines 'simple-lines )
(make-view-constructor-with-doc 'arrow 'arrow )
(make-view-constructor-with-doc 'view-layout 'view-layout )
(make-view-constructor-with-doc 'grid-layout 'grid-layout )
(make-view-constructor-with-doc 'table-layout 'table-layout )
(make-view-constructor-with-doc '1way-layout '1way-layout :format :row )
(make-view-constructor-with-doc 'row-layout  'grid-layout :format :row)
(make-view-constructor-with-doc 'col-layout  'grid-layout :format :col)
(make-view-constructor-with-doc 'batch-layout 'batch-layout )
(make-view-constructor-with-doc '1d-layout '1d-layout )
(make-view-constructor-with-doc 'xy-layout 'xy-layout )
(make-view-constructor-with-doc 'bar-chart 'bar-chart )
(make-view-constructor-with-doc 'pairs-layout 'pairs-layout )
(make-view-constructor-with-doc 'view-layers 'view-layers )

(make-view-constructor-with-doc 'scroll-bar 'scroll-bar )
(make-view-constructor-with-doc 'scrolling-display 'scrolling-display )

