;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               view-layers.lisp
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
;;;     C.B. Hurley 1993 George Washington University
;;;     
;;;
;;;
;;;------------------------------------------------


(in-package :views)
(eval-when (:compile-toplevel :load-toplevel :execute) (export '(view-layers)))


(defclass view-layers(compound-view) 
  ((link-bounds-x? :initform t :initarg :link-bounds-x? :accessor link-bounds-x-p)
   (link-bounds-y? :initform t :initarg :link-bounds-y? :accessor link-bounds-y-p))
  )
    
(defmethod construct-sub-views ((self view-layers) &rest keyword-pairs
                                     &key nsubviews subviews subview-type)
  
  (labels ((repeat-list(list n)
             (if (and list n)
               (loop while (< (length list) n)
                     do (setq list (append list list))
                     finally (return (subseq list 0 n)))
               list))
           (local-make-view(arg &optional type) 
             (if (view-p arg)
               arg
               (apply #'view 
                      (append (if type (list :type type)) 
                              (subview-arg-list arg)
                              keyword-pairs)))))

    (if (and (null nsubviews) (numberp subviews))
      (rotatef nsubviews subviews))
    (if (and subviews (not (listp subviews))) 
      (setq subviews (list subviews)))

     (setq subviews
          (cond ((and subviews (listp subviews))
                 (repeat-list subviews nsubviews))
                ((and (numberp nsubviews) (null subview-type))
                 (repeat-list 
                  (choose-views-from-menu :prompt-string  "Select layer views" 
                                          :superclass '(or simple-view compound-view)
                                           :nmax nsubviews) 
                              nsubviews))
                ((numberp nsubviews) (make-list nsubviews))
                ((and (null subviews) (null subview-type))
                 (choose-views-from-menu :superclass '(or simple-view compound-view)
                                         :prompt-string  "Select layer views" ))
                (t nil)))
                 
     
      (loop for key in '(:subviews :subview-type)
          do (disable-keyword keyword-pairs key))
      
       (setf (subviews-of self)
        (loop for s in subviews 
              collect
              (if (legal-view-construct-p s)
                (local-make-view s subview-type)
                (quail-error "Illegal subview argument ~A" s))))
       (constrain-bounds self)))



(defmethod init-position-subviews ((self view-layers)
                                   &key )
  (let ((br (bounding-region-of self)))
    (setf (sub-view-locns-of self)
          (loop repeat (length (subviews-of self))
                collect
                (make-region br)))))
                


(defmethod constrain-bounds ((self view-layers) &key draw? (link-bounds-x? :ignore)
                             (link-bounds-y? :ignore))
  (unless (eq link-bounds-x? :ignore)
    (setf (link-bounds-x-p self) link-bounds-x?))
  (unless (eq link-bounds-y? :ignore)
    (setf (link-bounds-y-p self) link-bounds-y?))
  (if (link-bounds-x-p self)
    (link-view-bounds (subviews-of self) :x))
  (if (link-bounds-y-p self)
    (link-view-bounds (subviews-of self) :y))

  (when (or (link-bounds-x-p self) (link-bounds-y-p self))
    (remap-to-viewports self :erase? draw? :draw? draw?)))
