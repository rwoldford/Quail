;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               batch-layout.lisp
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
(eval-when (:compile-toplevel :load-toplevel :execute) (export '( batch-mixin batch-layout batch-sub-views get-batches )))

(defgeneric batch-sub-views (view-layout &key &allow-other-keys))


(defclass batch-mixin () 
  ((batches :initform nil :initarg :batches :accessor batches-of)
   (by-vars :initform nil :initarg :by :accessor by-vars-of)
   (batch-levels :initform nil  :initarg :batch-levels :accessor batch-levels-of))
  ( :default-initargs :by :prompt
    :initform-fn #'get-batch-inits :common-vars? t 
    )) 
 
(defclass batch-layout (batch-mixin grid-layout) 
  ()
  (:default-initargs  :subview-superclass nil
    :link-bounds-x? t :link-bounds-y? t :format :grid 
    :subview-constructor #'batch-sub-views))

(defun get-batches(&key by level-function batches dataset cases order-levels)
  "Returns a list of batches (sub-datasets) from its arguments.~
   If BATCHES is already a list of datasets ,~
   then BATCHES is returned.~
   Otherwise, if BATCHES is not a function, a function is formed ~
   by applying data-subsets-fn to BATCHES.
   The function when applied to the dataset should yield~
   the batches."
  (cond 
        ((functionp batches)
         (funcall batches dataset :cases cases))
        ((list-of-datasets-p  batches)
         (values batches (list (length batches))))
        ((and batches (listp batches) (every  #'listp batches))
         (values batches (list (length batches))))
        (by (funcall (data-subsets-fn by :level-function level-function
                                      :order-levels order-levels) 
                    dataset  :cases cases))
        (batches batches)
        ((functionp cases) (funcall cases dataset))
        (dataset (list-cases dataset))
        (t nil)))


(defmethod view-title-string ((self batch-mixin)) 
  (let* ((batches (by-vars-of self))
        (vo (viewed-object-of self))
      ;;  (s (car (subviews-of-type   self 'batch-layout))) 
         (vars  (list-variates vo))
         (name (dataset-name vo))
         ;; (subbatches (if s (batches-of s)))
         )
    (unless (listp batches)
      (setq batches (list batches)))
    ;;(unless (listp subbatches)
    ;;  (setq subbatches (list subbatches)))
   ;; (setq batches (append batches subbatches)) 
    (cond ((and  batches (listp batches) 
           (subsetp  batches vars :test #'eq-variates))

           (if name  (format nil "~A: ~{~A~^ by ~}" name batches)
               (format nil "~{~A~^ by ~}"  batches)))
           (name name)
           (t nil))))   

(defmethod change-variable ((self batch-mixin) &rest keyword-args)
  (let ((dv (car (subviews-of-type self 'd-view))))
    (if dv   
      (apply #'change-variable-when-vars dv keyword-args)
      (let ((dv (loop for v in (subviews-of self)
                      thereis (and
                               (compute-applicable-methods  #'change-variable  (list v) )
                               v))))
      (if dv
        (apply #'change-variable dv keyword-args))))))


(defmethod batch-level-names-of ((self batch-mixin) )
  (let ((batch-levels (batch-levels-of self)))
    (if (and (listp batch-levels) (numberp (car batch-levels)))
      (loop for b in batch-levels
            for code  from (char-code #\A)
            collect (loop with ch = (code-char code)
                         for i from 1 to b
                         collect (format nil "~A-~A" ch i)))
      batch-levels )))

(defmethod batch-level-counts-of ((self batch-mixin) )
  (let ((batch-levels (batch-levels-of self)))
    (if (and (listp batch-levels) (numberp (car batch-levels)))
      batch-levels
      (loop for b in batch-levels
            collect (length b)))))
   

(defun get-variable-args(key-pairs)
  (loop for k in (list :var :function :transform :x :y :z
                       :x-function :y-function :z-function
                       :x-transform :y-transform :z-transform
                       :vars :functions :transforms)
        for val = (getf  key-pairs k :no-key-val)
        unless (eql val :no-key-val)
        collect k and collect val))
 
(defmethod batch-sub-views ((self view-layout) 
                            &rest keyword-pairs 
                            &key subviews (format :none)
                            subview-type subview-superclass 
                            data cases batches  by level-function level-functions
                            order-levels common-vars? 
                            (justification :center) vars
                            orientation rows nrows cols ncols
                            )
  
    
    
    (let (xval yval batch-levels subview-arg var-arg)
      (setq var-arg (get-variable-args keyword-pairs  ))
                 
       (flet ((set-subview-type()
           (or subview-type
               (setq subview-type 
                     (choose-views-from-menu 
                      :prompt-string  "Select layout views" 
                      :superclass (or subview-superclass 
                                      (cond ((or (member :var var-arg)
                                                 (and (member :vars var-arg)
                                                 (= 1 (length vars))))
                                             '1d-view)
                                            ((or (and (member :x var-arg) (member :y var-arg) (member :z var-arg))
                                                 (and (member :vars var-arg)
                                                 (= 3 (length vars))))
                                             '3d-view)
                                            ((or (and (member :x var-arg) (member :y var-arg))
                                                 (and (member :vars var-arg)
                                                 (= 2 (length vars))))
                                             '2d-view)
                                            
                                            ((member :vars var-arg)
                                             'd-view)
                                            (t 'd-view))))))))
    (unless (or subviews  subview-type)
      (set-subview-type))
        (multiple-value-setq (batches batch-levels) 
          (get-batches :by by :batches batches :dataset data 
                       :cases cases :order-levels order-levels :level-function (or level-function level-functions)))
        (if (slot-exists-p self 'batch-levels)
          (setf (slot-value self 'batch-levels)
                (or batch-levels (list (length batches)))))
        (if (slot-exists-p self 'batches)
          (setf (slot-value self 'batches) batches))
        
        (if (and (eql format :grid) (or rows nrows cols ncols))
          (setq format :none))
       
        (when (and (symbolp format) (not (eql format :none)) (typep self 'grid-layout))
          (setq yval (first batch-levels)
                xval (cdr batch-levels))
          (if (listp yval)
            (setq yval (length yval))
            (unless (numberp yval)
              (setq yval 1)))
          (setq xval
                (loop with m = 1
                      for x in xval do
                      (if (listp x)
                        (setq x (length x)))
                      (if (numberp x)
                        (setq m (* m x)))
                      finally (return m)))
          
          
          (when (not (eql :grid format))
            (setq yval (* yval xval)
                  xval 1))
          
          (when (eq format :row)
            (rotatef xval yval))
          
          (when (and (eql :grid format) (eq orientation :vertical))
            (if (= xval 1)  (rotatef xval yval)))
          
          (if (null orientation)
            (setq orientation
                  (if (> yval xval)
                    :horizontal
                    :vertical)))
          (setf (layout-format-of self) (list yval  xval))
          )
        
        (setq subview-arg (append
                           (list :orientation (or orientation :horizontal) 
                                 :justification justification)
                           var-arg))
        
        (flet ((add-data(s d)
                 (let* ((dlist
                         (cond ((dataset-p d)
                                (list :data d))
                               ((and d (list-of-datasets-p d))
                                (list :data data :cases d))
                               (t (list :data d))))
                        (add (append dlist  subview-arg)))
                    (cond ((typep s 'view) s)
                         ((and (listp s) (getf s :type))
                          (append s add))
                         ((listp s)
                          (set-subview-type)
                          (append s add))
                         (t 
                          (append (list :type s) add)
                            )))))
          (when batches
            (setq subviews
                  (cond 
                   ((and subviews (= (length subviews) (length batches)))
                    (loop for vo in batches
                          for s in subviews
                          collect
                          (cond ((legal-view-construct-p s)
                                 (add-data s vo))
                                ((listp s)
                                 (loop for si in s
                                       collect (add-data si vo)))
                                (t
                                 (add-data nil vo)))))
                   
                   
                   (t (loop for vo in batches collect
                            (add-data nil  vo )))))
            )
                 
                 
          
          (apply #'default-layout-sub-views self :subviews subviews
                 :common-vars? common-vars? 
                 :subview-type subview-type 
                 keyword-pairs)))))




(defun all-tuples (args)
  (let ((a (first args)) 
        b)
    (unless (null a)
      (setq b (all-tuples (cdr args)))
      (cond   ((null b)
               (mapcar #'list a))


              (t (loop for ai in a append
        (loop for bi in b collect (cons ai bi))))))))

(defmethod batch-string-x ((self batch-mixin))
  nil)

(defmethod batch-string-y ((self batch-mixin))
  nil)

(defun short-string-float(z s)
  (if (> (length (format nil "~A" z)) s)
    (let ((format (format nil "~A~AF" #\~ s)))
       (format nil format z))
    z))
      
  
(defmethod batch-string-x ((self batch-layout))
  (let ((tups (all-tuples (batch-level-names-of self)))
        (c (ncols-of self))
        (r (nrows-of self))
        result)
    (setq result
          (loop for j from 0 below c
          for colj = (loop repeat r
                           for i from j by c
                           collect (nth i tups))
            collect (loop for c1 in (first colj)
                          for k upfrom 0
                        when (every #'(lambda(e) (equal c1 (elt e k)))
                                    (cdr colj))
                        collect (if (floatp c1)
                                     (short-string-float c1 6)
                                     c1))))
    
    (unless (every #'null result)
    (loop for r in result
          collect (format nil "~{~A~^ ~}"  r)))))
    




(defmethod batch-string-y ((self batch-layout))
  (let ((tups (all-tuples (batch-level-names-of self)))
        (c (ncols-of self))
        result)
    (setq result 
          (loop for rest-rows on tups by #'(lambda(l) (nthcdr c l))
                for rowi1 = (car rest-rows)
                for rest-rowi = (subseq rest-rows 1 c)
                collect (loop for r1 in rowi1
                              for j upfrom 0
                              when (every #'(lambda(e) (equal r1 (elt e j)))
                                          rest-rowi) 
                              collect (if (floatp r1)
                                        (short-string-float r1 6)
                                        r1))))
    
    (unless (every #'null result)
      (loop for r in result
            collect (format nil "~{~A~^ ~}"  r)))))

(defmethod margin-string-bottom ((self batch-layout))
  (or (batch-string-x self)
      (and (<= (nrows-of self) 2) (>= (ncols-of self) 2)
           
           (let* ((tups (all-tuples (batch-level-names-of self)))
                  (c (ncols-of self))
                  (n (length tups ))
                  result)
             (when tups
             (setq result
                   (loop for j from (- n c) below n
                         for c1 = (nth j tups)
                         collect (if (floatp c1)
                                   (short-string-float c1 6)
                                   c1)))
             
             (unless (every #'null result)
               (loop for r in result
                     collect (format nil "~{~A~^ ~}"  r))))))
      (let ((s (car (subviews-of self))))
        (typecase s
          (batch-mixin
           (let ((strings (batch-string-x s)))
             (loop with new-s
                   for s in strings
                   for i upfrom 0
                   collect
                   (if (or (null s)
                           (equal s ""))
                     (or
                      (loop for s1 in (cdr (subviews-of self))
                            thereis (and (typep s1 'batch-mixin) 
                                         (setq new-s (nth i (batch-string-x s1) ))
                                        (not (equal new-s ""))
                                        new-s))
                      "")
                     s))))
          (2d-view (list (coord-string-y s) "versus" (coord-string-x s)))
          (histogram-view (list (free-coord-string  s) "of" (coord-string s)))
          (1d-view (coord-string s))
          (t (margin-string-bottom s))))))


(defmethod margin-string-left ((self batch-layout))
  (or (batch-string-y self)
      (and (<= (ncols-of self) 2) (>= (nrows-of self) 2)
           (let ((tups (all-tuples (batch-level-names-of self)))
                 (c (ncols-of self))
                 result)
             (when tups
               (setq result 
                   (loop  for rowi1 in tups by  #'(lambda(l) (nthcdr c l))
                          collect (loop for r1 in rowi1
                                        collect (if (floatp r1)
                                                  (short-string-float r1 6)
                                                  r1))))
             
             (unless (every #'null result)
               (loop for r in result
                     collect (format nil "~{~A~^ ~}"  r))))))

       (let ((s (caar (layout-views-of self))))
         (typecase s 
           (batch-mixin
           (let ((strings (batch-string-y s)))
             (loop with new-s
                   for s in strings
                   for i upfrom 0
                   collect
                   (if (or (null s)
                           (equal s ""))
                     (or
                      (loop for (s1) in (cdr (layout-views-of self))
                            thereis (and (typep s1 'batch-mixin) 
                                         (setq new-s (nth i (batch-string-y s1) ))
                                        (not (equal new-s ""))
                                        new-s))
                      "")
                     s))))
           (2d-view (list (coord-string-y s) "versus" (coord-string-x s)))
          (histogram-view (list (free-coord-string  s) "of" (coord-string s)))
          (1d-view (coord-string s))
          (t (margin-string-left s))))))

(defmethod margin-string-right ((self batch-layout))
  (let ((tups (all-tuples (batch-level-names-of self)))
            (c (ncols-of self))
            result)
     (when (<= c 2)
      
        (setq result 
              (loop  for rowi1 in (nthcdr (- c 1) tups)  by  #'(lambda(l) (nthcdr c l))
                    collect (loop for r1 in rowi1
                                       collect (if (floatp r1)
                                            (short-string-float r1 6)
                                            r1))))
        
        (unless (every #'null result)
          (loop for r in result
                collect (format nil "~{~A~^ ~}"  r))))))


(defmethod coord-string-x ((self batch-mixin))
  (let ((s (car (subviews-of self))))
    (if s (coord-string-x s))))

  
(defmethod coord-string-y ((self batch-mixin))
  (let ((s (car (subviews-of self))))
    (if s (coord-string-y s))))

(defmethod margin-string-top ((self batch-layout))
  
  (or (let ((tups (all-tuples (batch-level-names-of self)))
        (c (ncols-of self))
        (r (nrows-of self))
        result)
        (when (<= r 2)
    (setq result
          (loop for j from 0 below c
          for c1 = (nth j tups)
                           collect (if (floatp c1)
                                     (short-string-float c1 6)
                                     c1)))
    
    (unless (every #'null result)
    (loop for r in result
          collect (format nil "~{~A~^ ~}"  r)))))

      (let ((s (caar (layout-views-of self))))
        (typecase s
          (batch-layout 
           (let ((strings (batch-string-x s)))
             (loop with new-s
                   for s in strings
                   for i upfrom 0
                   collect
                   (if (or (null s)
                           (equal s ""))
                     (or
                      (loop for (s1) in (cdr (layout-views-of self))
                            thereis (and (typep s1 'batch-layout) 
                                         (setq new-s (nth i (batch-string-x s1) ))
                                        (not (equal new-s ""))
                                        new-s))
                      "")
                     s))))
          (2d-view (list (coord-string-y s) "versus" (coord-string-x s)))
          (histogram-view (list (free-coord-string  s) "of" (coord-string s)))
          (1d-view (coord-string s))
          (t (margin-string-top s))))))


