;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                              header-box.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;
;;;
;;;  Authors:
;;;     C.B. Hurley 1992 George Washington University
;;;     R.W. Oldford 1992-4
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :views)
(eval-when (:compile-toplevel :load-toplevel :execute) (export '(header-box make-view-paragraph header-box-view
          ;;make-help-views-arg-element
          )))

(defclass header-box (boxed-view-mixin compound-view)
  ()
  (:default-initargs :color q::*help-foreground-color*
    :style-keys '(:color))
  )

(defmethod construct-sub-views ((self header-box)
                                &rest args
                                &key
                                (left "")
                                (middle "")
                                (right "")
                                (left-font wb:*help-lisp-title-font*)
                                (middle-font wb:*help-small-title-font*)
                                (right-font wb:*help-normal-title-font*))
  (declare (ignore args))
  (let ((left-label (label :draw? NIL
                           :justification :left
                           :font left-font
                           :color q::*help-foreground-color*))
        (middle-label (label :draw? NIL
                             :justification :centre
                             :font middle-font
                             :color q::*help-foreground-color*))
        (right-label (label :draw? NIL
                            :justification  :right
                            :font right-font
                            :color q::*help-foreground-color*)))
    (setf (text-of left-label) (format NIL "~a" left))
    (setf (text-of middle-label) (format NIL "~a" middle))
    (setf (text-of right-label) (format NIL "~a" right))
    (setf (sub-views-of self)
          (list left-label middle-label right-label))))

(defmethod init-position-subviews ((self header-box)
                                   &key
                                   (left-xmin 0.05)
                                   (left-xmax 0.3)
                                   (middle-xmin 0.31)
                                   (middle-xmax 0.7)
                                   (right-xmin 0.71)
                                   (right-xmax 0.95))
  (let ((subs (sub-views-of self)))
    (place-subview self (first subs)  (make-region left-xmin left-xmax 0 1))
    (place-subview self (second subs) (make-region middle-xmin middle-xmax 0 1))
    (place-subview self (third subs)  (make-region right-xmin right-xmax 0 1))))


(defun header-box-view (&rest initargs
                              &key (left nil left?)
                              (middle nil middle?)
                              (right nil right?)
                              (left-font wb:*help-lisp-title-font*)
                              (middle-font wb:*help-small-title-font*)
                              (right-font wb:*help-normal-title-font*)
                              &allow-other-keys)
  "Returns a header-box view ~
   with text left left-justified horizontally, middle centred, and right ~
   right-justified."
  (cond
   ((and left? middle? right?)
    (apply #'make-instance 'header-box
           :left left :middle middle :right right 
           :left-font left-font :middle-font middle-font
           :right-font right-font
           initargs))
   ((and left? right?)
    (apply #'make-instance 'header-box :left left :right right 
           :left-font left-font :middle-font middle-font
           :right-font right-font
           :left-xmax 0.5
           :middle-xmin 0.5
           :middle-xmax 0.55
           :right-xmin 0.55
           :right-xmax 0.95
           initargs))
   ((and left? middle?)
    (apply #'make-instance 'header-box :left left :middle middle 
           :left-font left-font :middle-font middle-font
           :right-font right-font
           :left-xmax 0.45
           :middle-xmin 0.50
           :middle-xmax 0.95
           :right-xmin 0.95
           :right-xmax 1.0
           initargs))
   (left?
    (apply #'make-instance 'header-box :left left 
           :left-font left-font :middle-font middle-font
           :right-font right-font
           :left-xmax 0.95
           :middle-xmin 0.95
           :middle-xmax 0.97
           :right-xmin 0.97
           :right-xmax 1.0
           initargs))
   ((and right? middle?)
    (apply #'make-instance 'header-box :middle middle :right right 
           :left-font left-font :middle-font middle-font
           :right-font right-font
           :left-xmin 0.0
           :left-xmax 0.10
           :middle-xmin 0.25
           :middle-xmax 0.6
           :right-xmin 0.6
           :right-xmax 0.95
           initargs))
   (middle?
    (apply #'make-instance 'header-box :middle middle 
           :left-font left-font :middle-font middle-font
           :right-font right-font
           :left-xmin 0.0
           :left-xmax 0.10
           :middle-xmin 0.15
           :middle-xmax 0.85
           :right-xmin 0.9
           :right-xmax 1.0
           initargs))
   (right?
    (apply #'make-instance 'header-box :right right 
           :left-font left-font :middle-font middle-font
           :right-font right-font
           :left-xmin   0.0
           :left-xmax   0.1
           :middle-xmin 0.1
           :middle-xmax 0.2
           :right-xmin  0.5
           :right-xmax  0.95
           initargs)))
  )

(defun make-view-paragraph (&key
                            (title "")
                            (body "")
                            (title-font wb:*help-key-font*)
                            (body-font wb:*help-small-text-font*))
  "Produce a help document paragraph ~
   having title title and contents body."
  (let ((title-label (label :draw? NIL
                            :justification '(:left :top)
                            :font title-font
                            :color q::*help-foreground-color*))
        (paragraph (text-view :draw? NIL
                              :justification '(:left :top)
                              :font body-font
                              :color q::*help-foreground-color*)))
    
    (setf (text-of title-label) (format NIL (format NIL "~a" title)))
    (setf (text-of paragraph) (format NIL (format NIL "~a" body)))
    (list title-label paragraph)))

#| not used yet

(defun make-help-views-arg-element (&key (title NIL title?)
                                         (items NIL items?)
                                         (title-font wb:*help-key-font*)
                                         (items-font wb:*help-small-text-font*))
  (let ((title-label (label :draw? NIL
                            :justification '(:left :top)
                            :font title-font
                            :color q::*help-foreground-color*))
        (title-description (text-view :draw? NIL
                                      :justification '(:left :top)
                                      :font items-font
                                      :color q::*help-foreground-color*))
        (views NIL))
    
    
    
    
    (setf (text-of title-label)
          (format NIL (format NIL "~a" (if title?
                                         title
                                         ""))))
    (setf (text-of title-description)
          (format NIL
                  (format NIL "~a argument(s):" (if title?
                                                  title
                                                  ""))))
    (setf views (list title-label title-description))
    (when items?
      (loop
        for i in items
        do
        (let
          ((arg-label (label :draw? NIL
                             :justification '(:left :top)
                             :font items-font
                             :color q::*help-foreground-color*))
           (description (text-view :draw? NIL
                                   :justification '(:left :top)
                                   :font items-font
                                   :color q::*help-foreground-color*)))
          (setf (text-of arg-label) (format NIL (format NIL "~a" (car i))))
          (if (cdr i)
            (if (> (length i) 2)
              (setf (text-of description)
                    (format NIL
                            (format NIL "Default is ~a.  ~a" (second i) (third i))))
              (setf (text-of description)
                    (format NIL (format NIL "~a" (second i)))))
            (setf (text-of description) ""))
          (nconc views (list arg-label description)))))
    (setf views (append (list title-label title-description) views))
    (format *terminal-io* "~&in make-help-views-arg-element ~%~
                           views = ~a" views)
    views)
  )
|#
