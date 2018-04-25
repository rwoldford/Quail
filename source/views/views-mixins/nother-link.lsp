;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               link.lisp
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

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(linkable-mixin 
           link-views link-highlit-view unlink-views link-view unlink-view link-to-selected
           get-link-table fast-link2-views)))



(defclass linkable-mixin ()
  ((linked-views
    :initform nil
    :initarg :linked-views
    :accessor linked-views-of)
   (link-table
    :initform nil
    :accessor link-table-of)))

(defgeneric get-link-table(view)
  (:documentation "Returns a link table from the hierarchy of view"))

(defgeneric fast-link2-views(view1 view2 &key draw? link-table)
  (:documentation "Links the two views. Does not compare the viewed elements "))

;;;----------------------------------------------------------------------------------

(defmethod link-table-of ((self view)) nil)
(defmethod linked-views-of ((self view)) nil)

(defmethod get-link-table((self view))
  (some #'get-link-table (subviews-of self)))

(defmethod get-link-table((self linkable-mixin))
  (link-table-of self))

(defmethod viewed-elt-test ((self linkable-mixin))
  (let ((tab (link-table-of self)))
    (if tab (link-table-test-of tab)
        (call-next-method))))

#|
(defmethod set-drawing-style :after ((self linkable-mixin)
                                     &key (draw-links? t) element styles (draw? t) (erase? draw?))
  
  (if (and (linked-views-of self) draw-links?)
    (let ((linked-views (linked-views-of self))
          (test (viewed-elt-test self)))
      (if element
        (loop for v in linked-views 
              do (set-drawing-style v 
                        :draw-links? nil :element element
                        :test test
                        :styles styles
                        :draw? draw? :erase? erase?))
        (let ((lvo (list-viewed-elements self)))
          (when lvo
            (loop for v in linked-views do 
                  (loop for vo in (cdr lvo) do
                        (set-drawing-style v 
                               :draw-links? nil :element vo
                               :test test
                               :draw? nil :styles styles))
                  (set-drawing-style v 
                         :draw-links? nil :element (car lvo) :test test
                        :draw? draw? :erase? erase? :styles styles))))))))
|#
;; on 2/22/95 changed code so that iteration over (list-viewed-elements self) is done
;; on the linked view if necessary. This version will be more efficient because
;; simple-style-mixins will not need to do this iteration.

(defmethod set-drawing-style :after ((self linkable-mixin)
                                     &key (draw-links? t) element styles (draw? t) (erase? draw?))
  
  (if (and (linked-views-of self) draw-links?)
    (let ((linked-views (linked-views-of self))
          (test (viewed-elt-test self)))
      (if element
        (loop for v in linked-views 
              do (set-drawing-style v 
                        :draw-links? nil :element element
                        :test test
                        :styles styles
                        :draw? draw? :erase? erase?))
        (loop for v in linked-views do 
                 (set-drawing-style v 
                         :draw-links? nil :from self :test test
                        :draw? draw? :erase? erase? :styles styles))
                  ))))

;;; Added do-nothing primary FEB 02,1998
(defmethod set-highlight-style  ((self linkable-mixin) hi-val
                                     &key (draw-links? t) element  (draw? t) (er
ase? draw?))
   (declare (ignore self hi-val draw-links? element draw? erase?))
   (call-next-method))


(defmethod set-highlight-style :after ((self linkable-mixin) hi-val
                                     &key (draw-links? t) element  (draw? t) (erase? draw?))
  
  (if (and (linked-views-of self) draw-links?)
    (let ((linked-views (linked-views-of self))
          (test (viewed-elt-test self)))
      (if element
        (loop for v in linked-views 
              do (set-highlight-style v hi-val
                        :draw-links? nil :element element
                        :test test
                        :draw? draw? :erase? erase?))
        (loop for v in linked-views do 
                 (set-highlight-style v hi-val
                         :draw-links? nil :from self :test test
                        :draw? draw? :erase? erase?))
                  ))))


(defmethod highlight-operation ((self linkable-mixin))
  :default
 )
;;------------------------------------------------------------------------------------

(defun make-the-link (v1 v2) 
  (push v1 (linked-views-of v2))
  (push v2 (linked-views-of v1)))

(defun break-all-links (v)
  (loop for link in (linked-views-of v)
        do (setf (linked-views-of link) 
                 (delete v (linked-views-of link))))
  (setf (linked-views-of v) nil))



;;----------------------------------------------------------------------------------------

(defun link-to-selected(view)
  "Links view to the selected views."
  (let ((selected (copy-list *selected-views* )))
    (deselect-all)
  (apply #'link-views view selected)))


(defun link-views (&rest views ) 
   (let* ((table (or (loop for v in views thereis (and  (typep v 'link-table) v))
                     (use-link-table)))
            (one-per-case (descendant-views-of-type views 'one-per-case-mixin))
           (d (car one-per-case)))  
      (when one-per-case 
       (link-view d :draw? t :link-table table)
       (loop for di in (cdr one-per-case)
             when (eq (viewed-object-of di)
                      (viewed-object-of d))
             do
             (loop for a in (subviews-of d)
                   for b in (subviews-of di) do
             (fast-link2-views a b :draw? t :link-table table))))
           
      (loop for v in views when (typep v 'view) do
            (link-view v :draw? t :link-table table))))

(defmethod link-view ((self view) &key (draw? t) (link-table (use-link-table)) (fast? t))
   (if fast?
    (let* ((one-per-case (descendant-views-of-type  self 'one-per-case-mixin))
         (d (car one-per-case)))
    (when one-per-case 
      (link-view d :draw? draw? :link-table link-table :fast? nil)
      (loop for di in (cdr one-per-case)
            when (eq (viewed-object-of di)
                     (viewed-object-of d))
            do
            (loop for a in (subviews-of d)
                  for b in (subviews-of di) do
                  (fast-link2-views a b :draw? draw? :link-table link-table))))))
           
      (loop for v in (subviews-of self) do
        (link-view v :draw? draw? :link-table link-table :fast? nil)))

                    
  

(defmethod link-view ((self linkable-mixin) &key (draw? t) (link-table (use-link-table)))
  (let ((my-table (link-table-of self)))
    (unless (and my-table (eql my-table link-table))
      
      (if (and my-table   (not (eql my-table link-table)))
        (unlink-view  self))
      (foreground-link-table link-table)
      ;;(if (and draw? (viewports-of self)) (erase-view self))
      (unless (eql (link-table-of self) link-table)
        (link-table-link-view link-table self))
      (when (and draw? (linked-views-of self) (drawing-styles-from-links self)
                   (viewports-of self))
        (draw-view self :erase? t)))))

(defun link-highlit-views (&rest views ) 
   (let* ((table (or (loop for v in views thereis (and  (typep v 'link-table) v))
                     (use-link-table)))
          )  
      
           
      (loop for v in views when (typep v 'view) do
            (link-highlit-view v :draw? t :link-table table))))

(defmethod link-highlit-view ((self view) &key (draw? t) (link-table (use-link-table)) )
   
           
      (loop for v in (subviews-of self) do
        (link-highlit-view v :draw? draw? :link-table link-table )))

                    
  

(defmethod link-highlit-view ((self linkable-mixin) &key (draw? t) (link-table (use-link-table)))
  (if (draw-style self :highlight?)
  (let ((my-table (link-table-of self)))
    (unless (and my-table  (eql my-table link-table))
      
      (if (and my-table   (not (eql my-table link-table)))
        (unlink-view self))
      (foreground-link-table link-table)
      ;;(if (and draw? (viewports-of self)) (erase-view self))
      (unless (eql (link-table-of self) link-table)
        (link-table-link-view link-table self))
      (when (and  draw? (linked-views-of self) (drawing-styles-from-links self)
                   (viewports-of self))
        (draw-view self :erase? t))))))





(defmethod drawing-styles-from-links ((self single-draw-style-mixin))
  (let ((test (viewed-elt-test self))
        (old-style (drawing-style-of self))
        (links (linked-views-of self) )
        (velt (car (list-viewed-elements self) ))
        (draw-styles-from-links )
        (changed? nil))
    (flet ((get-element-draw-style (i v)
           (or (nth i draw-styles-from-links)
             (let ((new (element-draw-style v  velt test)))
               (setf draw-styles-from-links (nconc draw-styles-from-links (list new)))
               new))))
               
    (loop for pair  in  old-style
          for key = (car pair) 
          for new-val =
          (or
           (loop with val
                 for v in links
                 for i upfrom 0
                 for vstyle = (get-element-draw-style i v)
                 thereis (and vstyle
                              (setq val (or (draw-style vstyle key :default :none) :null))
                              (not (eq val :none)) 
                              val))
           :none)
          when (eq new-val :null) do (setq new-val nil)
          unless (or (eq new-val :none) (eql new-val (cdr pair)) 
                     (and (eql key :font) (null (cdr pair)))) do
          (setf (cdr pair) (new-style-value key new-val pair)) (setf changed? t)))
    changed?))




(defmethod drawing-styles-from-links ((self multiple-draw-style-mixin))
  (let ((test (viewed-elt-test self))
        (links (linked-views-of self) )
        (draw-styles-from-links nil)
        (changed? nil))

    (flet ((get-element-draw-style (i v velt)
           (or (nth i draw-styles-from-links)
             (let ((new (element-draw-style v  velt test)))
               (setf draw-styles-from-links (nconc draw-styles-from-links (list new)))
               new))))
    
    (loop for vo in (list-viewed-elements self) 
          for old-styles in (drawing-styles-of self)
          do (loop for pair in  old-styles 
                   for key = (car pair)
                   for new-val =
                   (or
                    (loop with val
                          for v in links
                          for i upfrom 0
                          for vstyle = (get-element-draw-style i v  vo )
                          thereis (and vstyle
                              (setq val (or (draw-style vstyle key :default :none) :null))
                              (not (eq val :none)) 
                              val))
                    :none)
                    when (eq new-val :null) do (setq new-val nil) 
                   unless (or (eq new-val :none) (eql new-val (cdr pair))) do
                   (setf changed? t)
                   (setf (cdr pair) (new-style-value key new-val pair)))
          (setf draw-styles-from-links nil)
          )
    changed?)))

;;----------------------------------------------------------------------------------------


(defun unlink-views (&rest views)
  (loop for v in views do
        (unlink-view v)))
  
(defmethod unlink-view ((self view))
  (loop for v in (subviews-of self) do
        (unlink-view v )))

(defmethod unlink-view ((self linkable-mixin))
  (break-all-links self)
  (if (link-table-of self)
    (link-table-remove-view (link-table-of self) self)))



(defmethod fast-link2-views((view1 view) (view2 view)
                          &key (draw? t) link-table)
  (declare (ignore draw? link-table)))


(defmethod fast-link2-views((s1 linkable-mixin) (s2 linkable-mixin)
                            &key (draw? t) (link-table (use-link-table))) 
  (if (and (link-table-of s1) (not (eql (link-table-of s1) link-table)))
    (background-link-table  (link-table-of s1)))
  (if (and (link-table-of s2) (not (eql (link-table-of s2) link-table)))
    (background-link-table  (link-table-of s2)))
  (foreground-link-table link-table)

  
  (cond ((and (link-table-of s1) (link-table-of s2))
         nil)
        ((link-table-of s1)
         (loop for v in (linked-views-of s1) do
               (make-the-link v s2))
         (make-the-link s1 s2)
         (link-table-add-view link-table s2)
          )
        ((link-table-of s2)
         (loop for v in (linked-views-of s2) do
               (make-the-link v s1))
         (make-the-link s1 s2)
         (link-table-add-view link-table s1)
          )
       (t
         (make-the-link s1 s2)
         (link-table-add-view link-table s1)
         (link-table-add-view link-table s2)
          ))
  (if (and draw? (drawing-styles-from-links s1)  (viewports-of s1)) (draw-view s1 :erase? t))
  (if (and  draw? (drawing-styles-from-links s2) (viewports-of s2)) (draw-view s2 :erase? t))

  )
;; Added do-nothing primary FEB 02, 1998
(defmethod set-highlight-style-on  ((self linkable-mixin)
                                          &key (draw-links? t)  elements on)
   (declare (ignore self draw-links? elements on))
   (call-next-method))



(defmethod set-highlight-style-on :after ((self linkable-mixin) 
                                          &key (draw-links? t)  elements on)
  
  (if (and (linked-views-of self) draw-links?)
    (let ((linked-views (linked-views-of self))
          )
      (if (typep self 'multiple-draw-style-mixin)
        (loop for e in elements
              for d in (drawing-styles-of self)
              for hi = (draw-style d :highlight?)
              when (and e hi) do
              (loop for v in linked-views 
                    do (set-highlight-style v hi
                                            :draw-links? nil :element e
                                               :draw? t)))
       (when on
         (loop for v in linked-views do 
              (set-highlight-style-on v  :on t
                                   :draw-links? nil :elements elements 
                                   :draw? t)))))))






