;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               batch-plots.lisp
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
;;;  EXPERIMENTAL
;;;----------------------------------------------------------------------------------
(in-package :views)
(eval-when (:compile-toplevel :load-toplevel :execute) (export '(table-plot 1d-batch-plot 2d-batch-plot )))



(defmethod select-view-menu-list((view-symbol (eql 'table-entry)))
  '(("label" linkable-label)
                            ("rectangle" rectangle-with-text)))


(defun table-plot (&rest keyword-pairs
                         &key (text-fn #'(lambda(vo)
                                           (length (list-cases vo))))
                         (subview-superclass 'table-entry)
                         (initform-fn #'get-batch-inits)
                         (gap-x 0) (gap-y 0)
                         subviews
                         left-view   bottom-view
                         bottom-label    (top-label :default) (left-label :default)
                         (title :default)
                         (format :default)
                         link-bounds-x? link-bounds-y?
                         &allow-other-keys)
  
  
  
  (setq keyword-pairs (append (apply initform-fn keyword-pairs) keyword-pairs))
  
  (disable-keyword keyword-pairs :initform-fn)
  (let* ((byvars (getf keyword-pairs :batches))
         (x  (if (listp byvars) (second byvars)))
         (y  (if (listp byvars) (first byvars) byvars))
         (data (getf keyword-pairs :data))
         (name (or (dataset-name data) "")))
    (setq format (if (eq format :default)
                     (if (and x y) :grid :col) format))

    (apply #'batch-plot :subview-superclass subview-superclass
           :format format
           :gap-x gap-x :gap-y gap-y
           :subviews (or subviews `((:text ,text-fn)))
           :left-view left-view 
           :bottom-view bottom-view
           :bottom-label bottom-label :orientation :horizontal
           :link-bounds-x? link-bounds-x? :link-bounds-y? link-bounds-y?
           :top-label (if (eq top-label :default)
                        (not (eq format :col))
                        top-label)
           :left-label (if (eq left-label :default)
                        (not (eq format :row))
                        left-label)
           :title (if (eq title :default) 
                    (cond 
                     ((and x y)
                      (format nil "~A:~A by ~A" name x y ))
                     (y (format nil "~A:~A" name y))
                     (t name))
                    title)
           :size t
           keyword-pairs)))



(defun 1d-batch-plot (&rest keyword-pairs
                            &key 
                            (initform-fn #'get-batch-inits)
                            (gap-x 0.02)  (gap-y 0.02)
                            (subview-superclass '1d-view)
                            (format :default)
                            (orientation :vertical)
                            (left-view :default)  (bottom-view :default) 
                             (bottom-label :default) (left-label :default)
                             (top-label :default) (right-label :default)
                            
                            (title :default)
                            &allow-other-keys)
  
  
  
  (setq keyword-pairs (append (apply initform-fn keyword-pairs) keyword-pairs))
  
  (disable-keyword keyword-pairs :initform-fn)
  (let* ((byvars (getf keyword-pairs :batches))
         (x  (if (listp byvars) (second byvars)))
         (y  (if (listp byvars) (first byvars) byvars))
         (data (getf keyword-pairs :data))
         (name (or (dataset-name data) ""))
         oargs p dview)
    (setq oargs
          (if (eq orientation :vertical)
            (list 
             :format (setq format (if (eq format :default)
                                    (if (and x y) :grid  :row)
                                    format))
             :left-view (if (eq left-view :default) t left-view)
             :bottom-view (if (eq bottom-view :default) nil bottom-view))
            
            (list 
             :format (setq format (if (eq format :default)
                                    (if (and x y) :grid  :col)
                                    format))
             :left-view (if (eq left-view :default) nil left-view)
             :bottom-view (if (eq bottom-view :default) t bottom-view))))
    
    
    (setq p (apply #'batch-plot :subview-superclass subview-superclass
                   :gap-x gap-x :gap-y gap-y
                   :common-vars? t :orientation orientation
                   :top-label (if (eq top-label :default)
                        (not (eq format :col))
                        top-label)
                   :left-label (if (eq left-label :default)
                        (not (eq format :row))
                        left-label)

                   :bottom-label (if (eq bottom-label :default)
                                   (label) bottom-label)
                   :right-label (if (eq right-label :default)
                                   nil right-label)
                   
                   :title (if (eq title :default) 
                            (cond 
                             ((and x y)
                              (format nil "~A:~A by ~A" name x y ))
                             (y (format nil "~A:~A" name y))
                             (t name))
                            title)
                   :draw? nil
                   (append oargs keyword-pairs)))
    (setq dview 
            (car (or (subviews-of-type (interior-view-of p) 'd-view)
                     (subviews-of (interior-view-of p)))))

    
    (when (eq bottom-label :default)
      (let ((lab (bottom-label-of p)))
        (when lab
          (setf (text-of lab) (coord-string dview))
            (text-link dview lab))))
      
      
     
    (draw-view p)
    p
    
    ))

(defun 2d-batch-plot (&rest keyword-pairs
                            &key 
                            (initform-fn #'get-batch-inits)
                            (gap-x 0.02)  (gap-y 0.02)
                            (subview-superclass '2d-view)
                            (format :default)
                            (top-label :default) (right-label :default)
                              (bottom-label :default) (left-label :default)
                           
                            (title :default)
                            &allow-other-keys)
  
  
  
  (setq keyword-pairs (append (apply initform-fn keyword-pairs) keyword-pairs))
  
  (disable-keyword keyword-pairs :initform-fn)
  (let* ((byvars (getf keyword-pairs :batches))
         (x  (if (listp byvars) (second byvars)))
         (y  (if (listp byvars) (first byvars) byvars))
         (data (getf keyword-pairs :data))
         (name (or (dataset-name data) ""))
         p dview)
    
    (setq format (if (eq format :default)
                                    (if (and x y) :grid  :row)
                                    format))
    
    (setq p (apply #'batch-plot :subview-superclass subview-superclass
                   :gap-x gap-x :gap-y gap-y :format format
                   :common-vars? t
                   :top-label (if (eq top-label :default)
                        (not (eq format :col))
                        top-label)
                   :left-label (if (eq left-label :default)
                        (not (eq format :row))
                        left-label)
                    :bottom-label (if (eq bottom-label :default)
                                   (label) bottom-label)
                     :right-label (if (eq right-label :default) 
                                   (label :orientation :vertical) right-label)
                   
               
                   :title (if (eq title :default) 
                            (cond 
                             ((and x y)
                              (format nil "~A:~A by ~A" name x y ))
                             (y (format nil "~A:~A" name y))
                             (t name))
                            title)
                   :draw? nil
                   keyword-pairs))
    (setq dview 
            (car (or (subviews-of-type (interior-view-of p) 'd-view)
                     (subviews-of (interior-view-of p)))))

    (when (eq right-label :default)
      
      
      
      (let ((lab (right-label-of p)))
        (setf (text-of lab) (coord-string-y dview))
        (text-link dview (text-of lab) :y))
      (let ((lab (bottom-label-of p)))
        (setf (text-of lab) (coord-string-x dview))
        (text-link dview (text-of lab) :x)))

       
    (draw-view p)
    p
    
    ))



#|


;; do an improved batch-plot function

(table-plot :data (car (list-drills assay))
            :cases #'sub-cases
            :batches '(alteration)
           )



(1d-batch-plot :data (car (list-drills assay))
            :cases #'sub-cases
            :format :col :orientation :horizontal
            :batches '(alteration)
            :justification :bottom :size t
           )

(1d-batch-plot :data (car (list-drills assay))
            :cases #'sub-cases
            :batches '(alteration lithology)
           )


(2d-batch-plot :data (fourth (list-drills assay))
            :cases #'sub-cases
           :batches '(alteration))

(2d-batch-plot :data (fourth (list-drills assay))
            :cases #'sub-cases :format :col
            :subview-type '2d-point-cloud
           :subviews '(:x gold :y silver)
           :batches '(alteration))

|#
