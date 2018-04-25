;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               var-selection.lisp
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
;;;     C.B. Hurley 1995 Maynooth
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------
(in-package :views)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(xy-menu-label xyz-menu-label single-xy-selection-list
          single-xyz-selection-list multi-xy-selection-list
          select-xyz-variables select-xy-variables)))


(defclass xy-menu-label(marked-menu-label)
  ((left-menu :allocation :class :initform nil)))

(defclass xyz-menu-label(marked-menu-label)
  ((left-menu :allocation :class :initform nil))
  )


(defclass single-xy-selection-list (display-list-selection-mixin variate-display-list)
  ()
  (:default-initargs :display-selection (list nil nil)))

(defclass single-xyz-selection-list (single-xy-selection-list)
  ()
  (:default-initargs :display-selection (list nil nil nil)))



(defclass multi-xy-selection-list (display-list-selection-mixin variate-display-list)
  ()
  (:default-initargs :display-selection (list nil nil)))


;;;----------------------------------------------------------------------------------

(defmethod get-menu-items  ((self xy-menu-label) 
                           (slot-name (eql 'left-menu)))
  `(("X"  "X")
    ("Y" "Y")
    ))

(defmethod get-menu-items  ((self xyz-menu-label) 
                           (slot-name (eql 'left-menu)))
  `(("X"  "X")
    ("Y" "Y")  ("Z" "Z")
    ))


(defmethod default-left-fn  ((self xy-menu-label) 
                                &key viewport ) 
   (invert-view self :viewport viewport)
  (let ((mark (wb:menu  (menu-of self 'left-menu))))
    (invert-view self :viewport viewport)
  (if mark
  (select-menu-label self :mark mark))
  ))

(defmethod default-left-fn  ((self xyz-menu-label) 
                                &key viewport )
  (declare (ignore viewport))
  (let ((mark (wb:menu  (menu-of self 'left-menu))))
  (if mark
  (select-menu-label self :mark mark))))






;;;----------------------------------------------------------------------------------

(defmethod selected-data-of ((self single-xy-selection-list))
  (let ((d (display-selection-of self)))
    (mapcar #'(lambda(x) (if x (viewed-object-of x))) d)))

(defmethod display-selection-function ((self single-xy-selection-list))
  #'(lambda(sub)
      (default-left-fn sub :viewport (car (viewports-of sub))) 
      (let ((mark (mark-of sub)))
        (if (equal mark "X")
          (setf (first (display-selection-of self)) sub) 
          (setf (second (display-selection-of self)) sub))
        (loop for s in (subviews-of self) 
              when (and (not (eql s sub)) (equal (mark-of s) mark)) do
              (deselect-menu-label s)))
      ))
    
(defmethod display-selection-function ((self single-xyz-selection-list))
  #'(lambda(sub)
      (default-left-fn sub :viewport (car (viewports-of sub)))
      (let ((mark (mark-of sub)))
        (if (equal mark "X")
          (setf (first (display-selection-of self)) sub) 
          (if (equal mark "Y") (setf (second (display-selection-of self)) sub)
              (setf (third (display-selection-of self)) sub)))
        (loop for s in (subviews-of self) 
              when (and (not (eql s sub)) (equal (mark-of s) mark)) do
              (deselect-menu-label s)))))


(defmethod selected-data-of ((self multi-xy-selection-list))
  (let ((d (display-selection-of self)))
    (list (mapcar #'viewed-object-of (first d))
          (mapcar #'viewed-object-of (second d)))))


(defmethod display-selection-function ((self multi-xy-selection-list))
  #'(lambda(sub)
      (let ((ansx (first (display-selection-of self)))
            (ansy (second (display-selection-of self))))
            (let ((old-mark (mark-of sub)) 
                  mark)
              (default-left-fn sub :viewport (car (viewports-of sub)))
              (if (equal (setq mark (mark-of sub)) old-mark)
                (progn 
                  (deselect-menu-label sub)
                  (if (equal mark "X")
                    (setf (first (display-selection-of self))
                          (delete sub ansx)) 
                    (setf (second (display-selection-of self)) 
                          (delete sub ansy))))
                (if (equal mark "X")
                    (setf (first (display-selection-of self))
                          (append ansx (list sub))) 
                    (setf (second (display-selection-of self)) 
                          (append ansy (list sub)))))))))


(defun select-xy-variables (data
                   &key prompt-text
                   (selection-type :single)
                   (selection-item-type 'xy-menu-label)
                   selection-display-type)
  "This pops up a window so the user can choose X and Y variables ~
   from data. ~
   If selection-type is :single (the default) only one X and one Y variable ~
   may be selected. Otherwise multiple Xs and Ys are allowed. ~
   Further control of selection behavior is possible by giving a suitable ~
   class name  for :selection-display-type, and of menu label appearance ~
   through the :selection-item-type keyword. "

  (if (null prompt-text)
    (setq prompt-text
          (if (eq selection-type :single)
            "Choose X and Y" "Choose Xs and Ys")))
  (if (null selection-display-type)
    (setq selection-display-type
          (if (eq selection-type :single)
            'single-xy-selection-list
            'multi-xy-selection-list)))
  (select-from-list data :prompt-text prompt-text
                    :selection-type selection-type
                    :selection-item-type selection-item-type
                    :selection-display-type selection-display-type))


(defun select-xyz-variables (data
                   &key prompt-text
                   (selection-type :single)
                   (selection-item-type 'xyz-menu-label)
                   (selection-display-type 'single-xyz-selection-list))
  "This pops up a window so the user can choose X, Y and Z variables ~
   from data. ~
   Further control of selection behavior is possible by giving a suitable ~
   class name  for :selection-display-type, and of menu label appearance ~
   through the :selection-item-type keyword. "


  (if (null prompt-text)
    (setq prompt-text
          (if (eq selection-type :single)
            "Choose X, Y, Z" "Choose X, Y, Zs")))
  
  (select-from-list data :prompt-text prompt-text
                    :selection-type selection-type
                    :selection-item-type selection-item-type
                    :selection-display-type selection-display-type))
