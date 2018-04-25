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
           link-views link-highlit-view unlink-views link-view unlink-view 
           get-link-table fast-link2-views)))



(defclass linkable-mixin ()
  ((linked-views
    :initform nil
    :initarg :linked-views
    :accessor linked-views-of)
   (private-styles
    :initform nil
    :initarg :private-styles
    :accessor private-styles-of)
   (link-table
    :initform nil
    :accessor link-table-of)
   (linkable? :initform t :initarg :linkable? :accessor linkable-view-p)))

(defgeneric get-link-table(view)
  (:documentation "Returns a link table from the hierarchy of view"))

(defgeneric fast-link2-views(view1 view2 &key draw? link-table)
  (:documentation "Links the two views. Does not compare the viewed elements "))


(defgeneric linkable-view-p(view)
  (:documentation "Is a view linkable?"))
;;;----------------------------------------------------------------------------------

(defmethod link-table-of ((self view)) nil)
(defmethod linked-views-of ((self view)) nil)
(defmethod linkable-view-p ((self view)) nil)

(defmethod get-link-table((self view))
  (some #'get-link-table (subviews-of self)))

(defmethod get-link-table((self linkable-mixin))
  (link-table-of self))

#|
(defmethod viewed-elt-test ((self linkable-mixin))
  (let ((tab (link-table-of self)))
    (if tab (link-table-test-of tab)
        (call-next-method))))
|#

;; on 2/22/95 changed code so that iteration over (list-viewed-elements self) is done
;; on the linked view if necessary. This version will be more efficient because
;; simple-style-mixins will not need to do this iteration.


(defmethod set-linkable-styles ((self linkable-mixin) styles)
  (when styles
    (setf (private-styles-of self) 
          (set-difference (list-style-keys (drawing-style-of self)) styles))))


(defmethod set-drawing-style :after ((self linkable-mixin)
                                     &key test (draw-links? t) element  styles (draw? t) (erase? draw?) highlit?)
  
  (if (and (linked-views-of self) draw-links?)
    (let ((linked-views (linked-views-of self))
          (my-private-styles (private-styles-of self))) 
      (if my-private-styles
        (setq styles 
              (loop for key in styles by #'cddr
                    for val in (cdr styles) by #'cddr 
                    unless (member key my-private-styles)
                    collect key and collect val)))
      (if (or element highlit?)
        (let* ((ltest (or test (viewed-elt-test self)))
               (elements (cond (element
                                (loop for ve in (list-viewed-elements self)
                                      when (funcall ltest ve element)
                                      collect ve))
                               ((and (eq highlit? t) (typep self 'multiple-draw-style-mixin))
                                (loop for ve in (list-viewed-elements self)
                                        for d in (drawing-styles-of self)
                                        when (draw-style d :highlight?)
                                        collect ve))
                               ((eq highlit? t)
                                (if (draw-style self :highlight?)
                                    (list-viewed-elements self)))
                               ((typep self 'multiple-draw-style-mixin)
                                (or (loop for ve in (list-viewed-elements self)
                                        for d in (drawing-styles-of self)
                                      when (draw-style d :highlight?)
                                      collect ve)
                                      (list-viewed-elements self)))
                               (t (list-viewed-elements self)))))
          (when elements
            (if (eql elements (list-viewed-elements self)) (setq elements nil))
            (loop with test = (link-table-test-of (link-table-of self))
                  for v in linked-views
                  for v-private-styles = (private-styles-of v)
                  for v-new-styles = (if v-private-styles 
                                       (loop for key in styles by #'cddr
                                             for val in (cdr styles) by #'cddr
                                             unless (member key v-private-styles)
                                             collect key and collect val)
                                       styles)
                  do (set-drawing-style v 
                                        :draw-links? nil :elements elements
                                        :from self
                                        :test test
                                        :styles v-new-styles
                                        :draw? draw? :erase? erase?))))
        (loop with test = (link-table-test-of (link-table-of self))
              for v in linked-views 
              for v-private-styles = (private-styles-of v)
              for v-new-styles = (if v-private-styles 
                                   (loop for key in styles by #'cddr
                                         for val in (cdr styles) by #'cddr
                                         unless (member key v-private-styles)
                                         collect key and collect val)
                                   styles)
              
              do 
              (set-drawing-style v 
                                 :draw-links? nil :from self :test test
                                 :draw? draw? :erase? erase? 
                                 :styles v-new-styles))
        ))))

;;; Added do-nothing primary FEB 02,1998
(defmethod set-highlight-style  ((self linkable-mixin) hi-val
                                 &key (draw-links? t) element  (draw? t) (erase? draw?))
   (declare (ignore self hi-val draw-links? element  erase?))
   (call-next-method))


(defmethod set-highlight-style :after ((self linkable-mixin) hi-val
                                       &key (draw-links? t) test element  (draw? t) (erase? draw?))
  
  (if (and (linked-views-of self) draw-links?)
    (let ((linked-views (linked-views-of self))
          (my-private-styles (private-styles-of self)))
      (if (not (member :highlight? my-private-styles))
        (if element
          (let* ((ltest (or test (viewed-elt-test self)))
                 (elements (loop for ve in (list-viewed-elements self)
                                 when (funcall ltest ve element)
                                 collect ve)))
            (loop with test = (link-table-test-of (link-table-of self))
                  for v in linked-views 
                  for v-private-styles = (private-styles-of v)
                  unless (member :highlight? v-private-styles)
                  do (set-highlight-style v hi-val
                                                :draw-links? nil :elements elements
                                                :from self
                                                :test test
                                                :draw? draw? :erase? erase?)))
          
          (loop with test = (link-table-test-of (link-table-of self)) 
                for v in linked-views 
                for v-private-styles = (private-styles-of v)
                unless (member :highlight? v-private-styles) do
                (set-highlight-style v hi-val
                                     :draw-links? nil :from self :test test
                                     :draw? draw? :erase? erase?))
          )))))


(defmethod highlight-operation ((self linkable-mixin))
  :default
 )
;;------------------------------------------------------------------------------------


(defun link-views (&rest views ) 
  "Link the views. ~
   More specifically, the linkable descendants of views are added ~
   to a link table.~
   If an argument is a link table, than that link table is used,~ 
   otherwise the user is prompted for a link table, if more than ~
   one link table is available." 
   (let* ((table (or (loop for v in views thereis (and  (typep v 'link-table) v))
                     (use-link-table)))
            
           )  
            
      (loop for v in views when (typep v 'view) do
            (link-view v :draw? t :link-table table))))

(defgeneric link-view (view &key draw? type link-table styles)
  (:documentation
   "Link the view. ~
    More specifically, the linkable descendants of view are added ~
    to a link table.~
    
    (:key ~
    (:arg draw? t Specifies if the view should be redrawn, with drawing styles
     from its linked views. ) ~
    (:arg link-table :default  Should be a link table. If is not provided,~
    the user is prompted for a link table, if more than ~
    one link table is available.)~
    (:arg type  NIL If provided, only linkable views of this type are linked..)
     (:arg styles  NIL If provided, only these drawing styles  are linked..) )"
   ))

(defgeneric unlink-view (view &key)
  (:documentation
   "Unlinks the view. ~
    More specifically, the linkable descendants of view are removed ~
    from a link table."))


(defmethod link-view ((self view) &key (draw? t) type styles
                      (link-table (use-link-table)) (to-views :all)
                      (fast? t))
  (if (and fast? (eq :all to-views))
    (let* ((one-per-case (descendant-views-of-type  self 'one-per-case-mixin))
           (d (car one-per-case)))
      (when (and one-per-case (not (typep link-table 'asymmetric-link-table)))
        (link-view d :draw? draw? :type type :link-table link-table :fast? nil :styles styles)
        (if type
          (loop for di in (cdr one-per-case)
              when 
              (eq (viewed-object-of di)
                  (viewed-object-of d))
              do
              (loop for a in (subviews-of d)
                    for b in (subviews-of di) 
                    for aelts = (list-viewed-elements a)
                    when (and (linkable-view-p a) (linkable-view-p b) 
                              (typep a type) (typep b type) 
                              aelts (eq aelts (list-viewed-elements b))) do
                    (fast-link2-views a b :draw? draw? :link-table link-table :styles styles)))

        (loop for di in (cdr one-per-case)
              when 
              (eq (viewed-object-of di)
                  (viewed-object-of d))
              do
              (loop for a in (subviews-of d)
                    for b in (subviews-of di) 
                    for aelts = (list-viewed-elements a)
                    when (and aelts (eq aelts (list-viewed-elements b))) do
                    (fast-link2-views a b :draw? draw? :link-table link-table :styles styles)))))))
           
      (loop with to = (if (eq to-views :old) (link-table-views-of link-table) to-views)
            for v in (subviews-of self) do
        (link-view v :draw? draw? :type type :link-table link-table :fast? nil :to-views to :styles styles)))

                    
  

(defmethod link-view ((self linkable-mixin) &key  styles (draw? t)  type  (to-views :all) (link-table (use-link-table)))
  (when (and (linkable-view-p self) (or (null type) (typep self type)))
    (if styles (set-linkable-styles self styles))
  (let ((my-table (link-table-of self)))
    (unless (and my-table (eql my-table link-table))
      
      (if (and my-table   (not (eql my-table link-table)))
        (unlink-view  self))
      (foreground-link-table link-table)
      ;;(if (and draw? (viewports-of self)) (erase-view self))
      (unless (eql (link-table-of self) link-table)
        (link-table-link-view link-table self to-views))
      (when (and draw? (linked-views-of self) (drawing-styles-from-links self)
                   (viewports-of self))
        (draw-view self :erase? t)))))
  (loop with to = (if (eq to-views :old) (link-table-views-of link-table) to-views)
            for v in (subviews-of self) do
        (link-view v :draw? draw? :type type :link-table link-table :fast? nil :to-views to :styles styles)))

(defun link-highlit-views (&rest views ) 
   (let* ((table (or (loop for v in views thereis (and  (typep v 'link-table) v))
                     (use-link-table)))
          )  
      
           
      (loop for v in views when (typep v 'view) do
            (link-highlit-view v :draw? t :link-table table))))

(defmethod link-highlit-view ((self view) &key (draw? t) (link-table (use-link-table)) )
   
           
      (loop for v in (subviews-of self) do
        (link-highlit-view v :draw? draw? :link-table link-table )))

                    
(defun unlink-highlit-views (&rest views ) 
   (loop for v in views when (typep v 'view) do
            (unlink-highlit-view v)))

(defmethod unlink-highlit-view ((self view))
   
           
      (loop for v in (subviews-of self) do
        (unlink-highlit-view v )))

  

(defmethod link-highlit-view ((self linkable-mixin) &key (draw? t) (link-table (use-link-table)))
  (if (and (linkable-view-p self) (draw-style self :highlight?))
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
        (draw-view self :erase? t)))))
  (loop for v in (subviews-of self) do
        (link-highlit-view v :draw? draw? :link-table link-table )))





(defmethod drawing-styles-from-links ((self single-draw-style-mixin))
  (let ((test (link-table-test-of (link-table-of self)))
        (old-style (drawing-style-of self))
        (links (linked-views-of self) )
        (velt (car (list-viewed-elements self) ))
        (private-styles (private-styles-of self))
        (draw-styles-from-links )
        (changed? nil))
    (flet ((get-element-draw-style (i v)
           (or (nth i draw-styles-from-links)
             (let ((new (element-draw-style v  velt test)))
               (setf draw-styles-from-links (nconc draw-styles-from-links (list new)))
               new))))
               
    (loop with new-val 
          for pair  in  old-style
          for key = (car pair) 
          unless (member key private-styles)
          do (setq new-val 
                   (or
                    (loop with val
                          for v in links
                          for styles-v = (private-styles-of v)
                          for i upfrom 0
                          for vstyle = (get-element-draw-style i v)
                          thereis (and (not  (member key styles-v)) 
                                       vstyle
                                       (setq val (or (draw-style vstyle key :default :none) :null))
                                       (not (eq val :none)) 
                                       val))
                    :none))
          (when (eq new-val :null) (setq new-val nil))
          (unless (or (eq new-val :none) (eql new-val (cdr pair)) 
                     (and (eql key :font) (null (cdr pair)))) 
          (setf (cdr pair) (new-style-value key new-val pair)) (setf changed? t))))
    changed?))




(defmethod drawing-styles-from-links ((self multiple-draw-style-mixin))
  (let ((test (link-table-test-of (link-table-of self)))
        (links (linked-views-of self) )
        (private-styles (private-styles-of self))
        (draw-styles-from-links nil)
        (changed? nil))

    (flet ((get-element-draw-style (i v velt)
           (or (nth i draw-styles-from-links)
             (let ((new (element-draw-style v  velt test)))
               (setf draw-styles-from-links (nconc draw-styles-from-links (list new)))
               new))))
    
    (loop for vo in (list-viewed-elements self) 
          for old-styles in (drawing-styles-of self)
          do (loop with new-val 
                   for pair in  old-styles 
                   for key = (car pair)
                    unless (member key private-styles) do
        
                   (setq new-val 
                   (or
                    (loop with val
                          for v in links
                           for styles-v = (private-styles-of v)
                        
                          for i upfrom 0
                          for vstyle = (get-element-draw-style i v  vo )
                          thereis (and (not  (member key styles-v)) vstyle
                              (setq val (or (draw-style vstyle key :default :none) :null))
                              (not (eq val :none)) 
                              val))
                    :none))
                    (when (eq new-val :null)  (setq new-val nil) )
                   (unless (or (eq new-val :none) (eql new-val (cdr pair))) 
                   (setf changed? t)
                   (setf (cdr pair) (new-style-value key new-val pair))))
          (setf draw-styles-from-links nil)
          )
    changed?)))

;;----------------------------------------------------------------------------------------


(defun unlink-views (&rest views)
  (loop for v in views do
        (unlink-view v)))
  
(defmethod unlink-view ((self view) &key type)
  (loop for v in (subviews-of self) do
        (unlink-view v :type type)))




(defmethod unlink-view ((self linkable-mixin) &key type)
  (if (and (or (null type) (typep self type))
           (link-table-of self))
    (link-table-remove-view (link-table-of self) self))
  (loop for v in (subviews-of self) do
        (unlink-view v :type type)))


(defmethod unlink-highlit-view ((self linkable-mixin) )
  (if (and (link-table-of self) (draw-style self :highlight?))
    (link-table-remove-view (link-table-of self) self))
  (loop for v in (subviews-of self) do
        (unlink-highlit-view v  )))



(defmethod fast-link2-views((view1 view) (view2 view)
                          &key type (draw? t) (link-table (use-link-table)) styles)
  (let ((list1 (descendant-views-of-type view1 `(and linkable-mixin ,type)))
        (list2 (descendant-views-of-type view2 `(and linkable-mixin ,type))))
    (when (= (length list1)  (length list2)) 
  (loop for v1 in list1
        for v2 in list2 do
        (fast-link2-views v1 v2 :link-table link-table :styles styles :draw? draw?)))))


(defmethod fast-link2-views((s1 linkable-mixin) (s2 linkable-mixin)
                            &key styles (draw? t) (link-table (use-link-table))) 
  (set-linkable-styles s1 styles) 
  (set-linkable-styles s2 styles)
   (if (and (link-table-of s1) (not (eql (link-table-of s1) link-table)))
    (background-link-table  (link-table-of s1)))
  (if (and (link-table-of s2) (not (eql (link-table-of s2) link-table)))
    (background-link-table  (link-table-of s2)))
  (foreground-link-table link-table)

  (let ((view-test (link-table-test-of link-table)))
  (cond ((and (link-table-of s1) (link-table-of s2))
         nil)
        ((linked-views-of s1)
         (loop for v in (linked-views-of s1) 
               when (or (null view-test) (funcall view-test v s2)) do
               (make-double-links v s2))
         (when (or (null view-test) (funcall view-test s1 s2))
           (make-double-links s1 s2))
         (link-table-add-view link-table s2)
          )
        ((linked-views-of s2)
         (loop for v in (linked-views-of s2) 
               when (or (null view-test) (funcall view-test v s1)) do
               (make-double-links v s1))
         (when (or (null view-test) (funcall view-test s1 s2))
           (make-double-links s1 s2))
         (link-table-add-view link-table s1)
          )
       (t
        (when (or (null view-test) (funcall view-test s1 s2))
           (make-double-links s1 s2))
         (make-double-links s1 s2)
         (link-table-add-view link-table s1)
         (link-table-add-view link-table s2)
          )))
  (if (and draw? (drawing-styles-from-links s1)  (viewports-of s1)) (draw-view s1 :erase? t))
  (if (and  draw? (drawing-styles-from-links s2) (viewports-of s2)) (draw-view s2 :erase? t))

  )










