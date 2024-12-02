;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               d-view.lisp
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
;;;     C.B. Hurley 1988-1991 George Washington University
;;;     R.W. Oldford 1988-1991
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :views)

(eval-when (:compile-toplevel :load-toplevel :execute)
(export '( case-coords active-status-p invalid-status-p ignore-status-p)))



          
   

(defmethod ncases ((self d-view))
  (length (cases-of self)))

(defmethod construct-sub-views :around ((self d-view) &key cases  variates)
  (declare (ignore cases  variates))
  (if (cases-of self)
      (call-next-method)))

(defmethod init-position-subviews :around ((self d-view) &key)
  (if (cases-of self)
    (call-next-method)))

(defmethod case-access-fn ((self d-view) var)
  ;; returns a function to access  value from a case
  (let ((vars (variates-of self))
        (val-fn (value-fn-of self)))
    (if ( functionp var) 
      var
      #'(lambda (c) (funcall val-fn c var :vars vars)))))







      
(defun case-coords (cases f tr)
  "Returns a list of coords by applying function f ~
   to each case, and transforming the resulting ~
   list (vector) by applying transform tr. "
   
  (let ((dlist (if f
                 (loop for c in cases
                     collect (funcall f c))
                 cases))) 
    (cond ((or (null tr) (eql tr #'identity))
           dlist)
          ((arrayp tr)
           (loop for i from 0 below (length cases)
              collect
              (loop for dj in dlist
                    for j upfrom 0
                    sum (* dj (aref tr i j)))))
          ((functionp tr)
           (funcall tr dlist))
          (dlist dlist)
          (t nil))))
    





(defmethod plot-coords-of ((self d-view) &key (cases (cases-of self)))
  (coords-of self :cases cases))

(defmethod get-cache-coords ((self d-view))
  (let ((coords (coords-cache-of self))
        (vo (cases-of self)))
    (if (and coords (= (length coords) (length vo))
             (or (and (listp (car coords)) (= (length (car coords)) (ndims self)))
                 (typep self '1d-view)))
      (if (null (case-status-of self))
        (compute-case-status self))
      (progn
        (setq coords (compute-coords self :cases vo))
        (setf (coords-cache-of self) coords)
        (compute-case-status self)))
    coords))
        


(defmethod compute-coords ((self 1d-view) &key (cases (cases-of self)))
  ;; returns list of case coords used in plot
  (let ((var (variate-of self)))
    (if (eql :iseq var)
      (setq cases
            (if (eql cases (cases-of self))
              (loop for i from 0 below (length cases) collect i)
              (loop with vo = (cases-of self) 
                    for c in cases collect (position c vo))))
      (if (and (listp var) (eql :seq (car var)))
        (setq cases
              (if (eql cases (cases-of self))
                (cdr var)
                (loop with vo = (cases-of self) 
                      for c in cases collect (nth (position c vo) (cdr var))))))))
  (case-coords cases (function-of self) (transform-of self)))


#|
(defmethod compute-coords ((self d-view) &key (cases (cases-of self)))
  ;; returns list of case coords used in plot
  (let ((vo (cases-of self)))
    (apply #'mapcar #'list
           (loop for f in (functions-of self)
                 for tr in (transforms-of self)
                 for v in (vars-of self)
                 collect 
                 (if (eql :iseq v)
                   (case-coords 
                    (if (eql cases vo)
                     (loop for i from 0 below (length cases) collect i)
                     (loop for c in cases collect (position c vo)))
                    f tr)
                   (case-coords cases f tr))))))
|#
(defmethod compute-coords ((self d-view) &key (cases (cases-of self)))
  ;; returns list of case coords used in plot
  (let ((vo (cases-of self)))
    (apply #'mapcar #'list
           (loop for f in (functions-of self)
                 for tr in (transforms-of self)
                 for v in (vars-of self)
                 collect 
                 (case-coords
                  (cond ((eql :iseq v)
                         (if (eql cases vo)
                           (loop for i from 0 below (length cases) collect i)
                           (loop for c in cases collect (position c vo))))
                        ((and (listp v) (eql :seq (car v)))
                         (if (eql cases vo) (cdr v)
                             (loop 
                               for c in cases collect (nth (position c vo) (cdr v)))))
                          (t cases))
                 f tr)))))



(defmethod coords-of ((self d-view) &key (cases (cases-of self)))
  (if (and (cases-of self) (has-variates-p self))
    (if (eq cases (cases-of self))
      (get-cache-coords self)
      (loop with viewed-object = (cases-of self)
            with cache = (get-cache-coords self)
            for c in cases
            for p = (position c viewed-object)
            collect (if p (nth p cache))))))

(defmethod coord-function ((self d-view) v f)
  (unless (null v)
       (if (or (eql v :iseq) (and (listp v) (eql (car v) :seq)))
        (get-function f)
      (if (and f (not (eql #'identity f)))
        #'(lambda (x) (funcall (get-function f)
                               (funcall  (case-access-fn self v) x)))
        (case-access-fn self v)))))




(defmethod function-of ((self 1d-view))
  ;; returns a function which when applied to a case yields  coord
  (coord-function self (variate-of self) (func-of self)))
  
(defmethod x-function-of ((self d-view))
  (coord-function self (x-variate-of self) (x-func-of self)))

(defmethod y-function-of ((self d-view))
  (coord-function self (y-variate-of self) (y-func-of self)))

(defmethod z-function-of ((self d-view))
  (coord-function self (z-variate-of self) (z-func-of self)))
 



(defmethod functions-of ((self d-view)) 
  (loop for v in (vars-of self)
        for f in (funcs-of self)
        collect (coord-function self v f)))





(defmethod x-coords-of ((self d-view) &key (cases (cases-of self)))
  (loop for c in (coords-of self :cases cases)
        collect (first c)))

(defmethod y-coords-of ((self d-view) &key (cases (cases-of self)))
  (loop for c in (coords-of self :cases cases)
        collect (second c)))

(defmethod z-coords-of ((self d-view) &key (cases (cases-of self)))
  (loop for c in (coords-of self :cases cases)
        collect (third c)))

;;===================================================================================================
              

(defmethod select-case-status ((self d-view) case)
  (let ((p (position case (cases-of self) :test #'eq-dataset)))
    (if p  (elt (case-status-of self)  p))))

(defmethod active-members ((self d-view) list)
  (if (null (case-status-of self))
    (coords-of self)
    
    (loop for s in (case-status-of self)
          for i in list
          when (active-status-p s) collect i)))

(defmethod inactive-members ((self d-view) list)
  (if (null (case-status-of self))
        (coords-of self))
  (loop for s in (case-status-of self)
        for i in list
        unless (active-status-p s) collect i))

(defmethod make-case-status ((self d-view))
  (make-list (length (cases-of self)) :initial-element t))

(defmethod valid-coord-test ((self d-view) c)
  (and c (every #'realp c)))

(defmethod valid-coord-test ((self 1d-view) c)
  (realp c))

#-:sbcl-linux(eval-when (:compile-toplevel :load-toplevel :execute) (proclaim '(inline active-status-p invalid-status-p ignore-status-p)))

#+:sbcl-linux(proclaim '(sb-ext:maybe-inline active-status-p)) ;25NOV2024
(defun active-status-p (s)
  "Checks for active case status "
  (eq s t))

#+:sbcl-linux(proclaim '(sb-ext:maybe-inline invalid-status-p)) ;25NOV2024
(defun invalid-status-p (s)
  "Checks for invalid case status "
  (or (eq s :invalid)  (eq s :invalid-ignore)))

#+:sbcl-linux(proclaim '(sb-ext:maybe-inline ignore-status-p)) ;25NOV2024
(defun ignore-status-p (s)
  "Checks for ignore case status"
  (or (eq s :ignore)  (eq s :invalid-ignore)))

(defmethod invalid-cases-p ((self d-view))
  (some #'invalid-status-p (case-status-of self)))

(defmethod compute-case-status ((self d-view))
  (let* ((status (case-status-of self)))
    (setq status (or status (make-case-status self)))
    (loop for c in (coords-cache-of self)
          for s in status
          for i upfrom 0 
          do
          (cond ((eq s :ignore) nil)
                ((eq s :invalid-ignore) 
                 (if (valid-coord-test self c)
                 (setf (elt status i) :ignore)))
                ((valid-coord-test self c)
                   (unless (eq t s) (setf (elt status i) t)))
                (t (setf (elt status i) :invalid))))
    (setf (case-status-of self) status)
    
     ))


;;;----------------------------------------------------------------------------------


(defmethod compute-bounding-region ((self d-view) )
  (smallest-bounding-region self))

(defmethod smallest-bounding-region ((self d-view) )
  (if (and (cases-of self) (car (cases-of self))
           (x-variate-of self) (y-variate-of self))
    (loop for (x y) in (plot-coords-of self )
          for s in (case-status-of self)
          when (active-status-p s)
          minimize x into x-min and maximize x into x-max and
          minimize y into y-min and maximize y into y-max 
          finally 
          
          (return (progn
                    
                    (when (= x-min x-max) 
                      (let ((fudge (* 0.1  (abs x-max))))
                        (if (zerop fudge) (setq fudge 1))
                        (incf x-max fudge)
                        (decf x-min fudge)))
                    (when (= y-min y-max) 
                      (let ((fudge (* 0.1  (abs y-max))))
                        (if (zerop fudge) (setq fudge 1))
                        (incf y-max fudge)
                        (decf y-min fudge)))
                    (make-region x-min x-max y-min y-max))))
    
    (make-region)))

(defmethod smallest-bounding-region ((self 1d-view))
  (let ((br (make-region)))
    (if (and (cases-of self) (car (cases-of self))
             (variate-of self))
      (loop  for val in (plot-coords-of self )
             for s in (case-status-of self)
             when (active-status-p s)
             minimize val into min and maximize val into max 
             finally  
             (progn 
               (when (= min max) 
                      (let ((fudge (* 0.1  (abs max))))
                        (if (zerop fudge) (setq fudge 1))
                        (incf max fudge)
                        (decf min fudge)))
               (ecase (orientation-of self) 
                 (:horizontal 
                  (setf (left-of br) min) 
                  (setf (right-of br) max))
                 (:vertical 
                  (setf (bottom-of br) min) 
                  (setf (top-of br) max)) ))))
    br))



;;;====================================================================================


(defmethod partition-viewed-objects ((self d-view) partition-fn 
                                     &key styles (test #'equal))
  ;; returns a list of viewed-objects categorized by applying 
  ;; function partition-fn  to the viewed objects
  ;; if styles are given the styles are arranged in parallel
  
  (let (case-vals part-vals ngroups)
    
    (loop for v in (cases-of self)
          for s in (case-status-of self)
          for val = (and (active-status-p s) (funcall partition-fn  v))
          collect val into vals
          when val collect val into val-non-nils
          finally (setq case-vals vals 
                        part-vals (remove-duplicates val-non-nils :test test)))
    (setq ngroups (length part-vals))
    
    (let* ((sub-vos (make-list ngroups))
           (sub-coords (make-list ngroups))
           (sub-styles (make-list ngroups)))
      (setq styles (or styles (make-list (length case-vals))))
      
      (loop with tv for x in case-vals
            for c in (coords-of self)
            for a1 in (cases-of self)
            for a2 in styles
            when (and x (setq tv (position x part-vals :test test))) do
            (push a1 (elt sub-vos tv))
            (push c (elt sub-coords tv))
            (push a2 (elt sub-styles tv)))
      (values sub-vos sub-coords sub-styles))))
        


#|
(defmethod group-viewed-objs ((self d-view) test ngroups &optional styles)
  ;; returns a list of viewed-objects categorized by applying 
  ;; function test to the coords
  ;; which returns an index 0 to ngroups-1
  ;; if styles are given the styles are arranged in parallel
  
  (let* ((sub-vos (make-list ngroups))
         (coords (plot-coords-of self))
         (sub-styles (make-list ngroups)))
    (setq styles (or styles (make-list (length coords))))
    
    (loop with tv for x in coords
          for s in (case-status-of self)
          for a1 in (cases-of self)
          for a2 in styles
          when (and (active-status-p s) (setq tv (funcall test x))) do
          (push a1 (elt sub-vos tv))
          (push a2 (elt sub-styles tv)))
    (values sub-vos sub-styles)))
|#

(defmethod group-viewed-objs ((self d-view) test ngroups &optional styles)
  ;; returns a list of viewed-objects categorized by applying 
  ;; function test to the coords
  ;; which returns an index 0 to ngroups-1
  ;; if styles are given the styles are arranged in parallel
  
  (let* ((sub-vos (make-list ngroups))
         (coords (plot-coords-of self))
         (sub-styles (make-list ngroups)))
    (setq styles (or styles (make-list (length coords))))
    
    (loop with tv for x in coords
          for s in (case-status-of self)
          for a1 in (cases-of self)
          for sty in styles
          when (and (active-status-p s) (setq tv (funcall test x))) do
          (push a1 (elt sub-vos tv))
          (loop for a2 in sty do (push a2 (elt sub-styles tv))))
    (values sub-vos sub-styles)))

(defmethod group-and-sort-viewed-objs ((self d-view) test ngroups &optional styles)
  ;; returns a list of viewed-objects categorized by applying 
  ;; function test to the coords
  ;; which returns an index 0 to ngroups-1
  ;; if styles are given the styles are arranged in parallel
  
  (let* ((sub-vos (make-list ngroups))
         (coords (plot-coords-of self))
         (sub-styles (make-list ngroups))
         (group-coords (make-list ngroups)))
    (setq styles (or styles (make-list (length coords))))
    
    (loop with tv for x in coords
          for s in (case-status-of self)
          for a1 in (cases-of self)
          for sty in styles
          when (and (active-status-p s) (setq tv (funcall test x))) do
          (push x (elt group-coords tv))
          (push a1 (elt sub-vos tv))
          (loop for a2 in sty do (push a2 (elt sub-styles tv))))

    (loop for c in group-coords
          for sty in sub-styles
          for vo in sub-vos
          for r = (mapcar #'list c vo sty) do
          (setq r (sort r #'< :key #'car))
          collect (mapcar #'second r) into sorted-vo
          collect  (mapcar #'third r) into sorted-sty
          finally (return (values sorted-vo sorted-sty)))))


(defmethod summarize ((self 1d-view)  function &key predicate &allow-other-keys )
  
  (let* ((coords (active-members self (plot-coords-of self))))
    (cond ((null predicate) nil)
          ((functionp predicate) 
           (setq coords (remove-if-not predicate coords)))
          (t (setq coords
                   (loop for c in coords 
                         for p in predicate when p collect c))))
    (funcall function coords)))

(defmethod summarize ((self 2d-view)  function &key variate predicate)
  (let* ((coords (active-members self (plot-coords-of self)))
         (x? (or (null variate) (eq variate (x-variate-of self))))
         (y? (or (null variate) (eq variate (y-variate-of self))))
         (xcoords (if x? (mapcar #'first coords)))
         (ycoords (if y? (mapcar #'second coords))))
    
    (cond ((null predicate) nil)
          ((functionp predicate) 
           (setq xcoords (remove-if-not predicate xcoords))
           (setq ycoords (remove-if-not predicate ycoords)))
          (t (setq xcoords
                   (loop for c in xcoords 
                         for p in predicate when p collect c))
             (setq ycoords
                   (loop for c in ycoords 
                         for p in predicate when p collect c))))
    
    (cond
     ((and x? y?) (funcall function xcoords ycoords))
     (x? (funcall function xcoords))
     (y? (funcall function ycoords))
     (t nil))))

(defmethod summarize :around 
  ((self d-view)  function &key (predicate (default-predicate self))
   variate)
  (let ((subs 
         (if (listp predicate)
           (loop for s in (subviews-of self) 
                 for p in predicate
                 collect (if p s) )
           (loop for s in (subviews-of self) 
                 collect (if (funcall predicate s)  s)))))
    (call-next-method self function :variate variate
                      :predicate subs)))

(defmethod default-predicate ((self d-view))
  #'(lambda (v) (not (draw-style v :invisible?))))


(defmethod coord-string-x (view)
  (declare (ignore view))
  )

(defmethod coord-string-y (view)
  (declare (ignore view))
  )

(defmethod coord-string-z (view)
  (declare (ignore view))
  )


(defmethod coord-string (view)
  (declare (ignore view))
  )


(defmethod coord-strings (view)
  (declare (ignore view))
  )

  
(defun plot-axis-string (a f tr &optional vars)
  (let (s)
    (setq s
          (cond ((or (eql a #'identity) (eql a 'identity)) "")
                ((typep a 'number)
                 (if vars (princ-to-string (elt (variate-names vars) a))
                     (variate-string-of a)))
                ((functionp a) (format nil "~@(~A~)" (function-string a)))
                ((listp a) 
                 (if (eql :seq (car a))
                   :Seq
                 (format nil "~{ ~@(~A~) ~}" 
                         (mapcar #'variate-string-of a))))
                (t
                 (format nil "~@(~A~)" (variate-string-of a)))
                ))
    (unless (or (eql f #'identity) (eql f 'identity) (null f))
      (setq s (format nil "~@(~A~) ~A" (function-string f) s)))
    
       (cond ((or (null tr) (eql tr #'identity) (eq :none tr) )
           s)
          ((or (symbolp tr) (functionp tr) )
           (format nil "~@(~A~) ~A" (function-string tr) s))
          (t (format nil "~A - ~A" (projector-name tr)  s)))))

   

(defmethod coord-string ((self 1d-view))
  (plot-axis-string (variate-of self) (func-of self) (transform-of self)
                    (variates-of self)))

(defmethod free-coord-string ((self 1d-view))
  NIL)

(defmethod coord-string-x ((self 1d-view))
  (if (eq (orientation-of self) :horizontal)
  (coord-string self)
  (free-coord-string self)))

(defmethod coord-string-y ((self 1d-view))
  (if (eq (orientation-of self) :vertical)
  (coord-string self)
  (free-coord-string self)))


(defmethod coord-string-x ((self d-view))
  (plot-axis-string (x-variate-of self) (x-func-of self) (x-transform-of self)
                    (variates-of self)))

(defmethod coord-string-y ((self d-view))
  (plot-axis-string (y-variate-of self) (y-func-of self) (y-transform-of self)
                    (variates-of self)))

(defmethod coord-string-z ((self 3d-view))
  (plot-axis-string (z-variate-of self) (z-func-of self) (z-transform-of self)
                    (variates-of self)))

(defmethod coord-strings ((self d-view))
  (loop for v in (vars-of self)
        for f in (funcs-of self)
        for tr in (transforms-of self)
        collect 
        (plot-axis-string v f tr )))
;;-------------------------------------------------------------------------------------

(defmethod original-bounds ((self d-view) &key draw?) 
  
  (reset-bounding-region self :draw? draw?) 
  )
#|
(defmethod reset-bounding-region ((self d-view) &key (draw? nil ) )
  
  (with-constrained-extents self  draw?
   (loop for v in (link-bounds-of self) 
          when (and (typep v 'd-view) (has-variates-p v))
          do (set-bounding-region v))
  ))
|#

(defmethod reset-bounding-region ((self d-view) &key (draw? nil ) )
  
  (with-constrained-extent self :x  draw?
    (loop for v in (link-bounds-x-of self) 
          when (and (typep v 'd-view) (has-variates-p v))
          do (set-bounding-region v :ignore-y? t)))
  (with-constrained-extent self :y  draw?
    (loop for v in (link-bounds-y-of self) 
          when (and (typep v 'd-view) (has-variates-p v))
          do (set-bounding-region v :ignore-x? t))))

(defmethod reset-bounding-region ((self 1d-view) &key (draw? nil ) )
  
  (if (eq (orientation-of self) :horizontal)
    (with-constrained-extent self :x  draw?
      (loop for v in (link-bounds-x-of self) 
            when (and (typep v 'd-view) (has-variates-p v))
            do (set-bounding-region v :ignore-y? t)))
    (with-constrained-extent self :y  draw?
      (loop for v in (link-bounds-y-of self) 
            when (and (typep v 'd-view) (has-variates-p v))
            do (set-bounding-region v :ignore-x? t)))))
 
(defmethod has-variates-p ((self view))
  nil)

(defmethod has-variates-p ((self 1d-view))
  (variate-of self))


(defmethod has-variates-p ((self 2d-view))
  (and (x-variate-of self) (y-variate-of self) ))

(defmethod has-variates-p ((self 3d-view))
  (and (x-variate-of self) (y-variate-of self) (z-variate-of self) ))

(defmethod has-variates-p ((self d-view))
  (vars-of self))

(defmethod ndims ((self d-view))
  (length (vars-of self)))

(defmethod ndims ((self 1d-view))
  1)

(defmethod ndims ((self 2d-view))
  2)

(defmethod ndims ((self 3d-view))
  3)



(defmethod make-layer-view ((self 2d-view) (layer symbol) &rest args)
  (let* ((data (getf   args :data :none ))
         (vo (getf args :viewed-object :none))
          (new-data (cond ((and (eq :none data) (eq :none vo))
                          (dataset-of self))
                         ((eq :none data)
                          vo)
                         (t data)))
          (new-args `(  :data ,new-data
                              ,@(if (typep self 'smooth-mixin)
                                  (list :smooth-par (smooth-par-of self)))
                              :flip-x? ,(flip-x-p self) :flip-y? ,(flip-y-p self)
                              :domain ,(let ((br (bounding-region-of self)))
                                         (list (left-of br) (right-of br)))
                              :x ,(or (getf args :x) (x-variate-of self))  
                              :y ,(or (getf args :y) (y-variate-of self))
                              :x-function ,(or (getf args :x-function) (x-func-of self))
                              :y-function ,(or (getf args :y-function) (y-func-of self))
                              :x-transform ,(or (getf args :x-transform) (x-transform-of self)) 
                              :y-transform ,(or (getf args :y-transform) (y-transform-of self))))
         lines-coords new-cases new-case-status
         )
    (if (eq new-data (dataset-of self))
      (setq new-args (list* :variates (variates-of self) new-args)))
    
    (setq new-cases
          (or (getf args :cases)
              (mapcar #'viewed-object-of (if (typep self 'compound-view)
                                           (some-subviews self :highlit? t)
                                           (highlighted-views)))
              (and (eq new-data (dataset-of self)) (cases-of self))))
    (if new-cases
      (setq new-args (list* :cases new-cases new-args)))
    (setq new-case-status
          (cond ((eq new-cases (cases-of self))
                 (copy-list (case-status-of self)))
                ((and (listp new-cases) (subsetp new-cases (cases-of self)))
                 (loop for d in new-cases collect (select-case-status self d)))
                (t nil)))
    (if new-case-status
      (setq new-args (list* :case-status new-case-status new-args)))
            
    
    (when (and (eql layer 'line-segment) (listp new-cases))
      (setq lines-coords 
            (plot-coords-of  self :cases new-cases))
      (if lines-coords
        (setq new-args (list* :lines-coords lines-coords new-args))))
    
      
    (apply #'view :type layer :draw? nil (append new-args args))))


(defmethod make-layer-view ((self 1d-view) (layer symbol) &rest args)
  (let* ((data (getf   args :data :none ))
         (vo (getf args :viewed-object))
         (new-data (cond ((and (eq :none data) (eq :none vo))
                          (dataset-of self))
                         ((eq :none data)
                          vo)
                         (t data)))
         (orient (orientation-of self))
         (new-args (list  :orientation orient
                         :domain (let ((br (bounding-region-of self)))
                                   (list (left-of br) (right-of br)))
                         :flip-x? (flip-x-p self) :flip-y? (flip-y-p self)
                         :data new-data
                         :var (or (getf args :var) (variate-of self)) 
                         :transform (or (getf args :transform) (transform-of self))))
         new-cases new-case-status)
    (if (eq new-data (dataset-of self))
      (setq new-args (list* :variates (variates-of self) new-args)))
    (setq new-cases
          (or (getf args :cases)
              (mapcar #'viewed-object-of (if (typep self 'compound-view)
                                           (some-subviews self :highlit? t)
                                           (highlighted-views)))
              (and (eq new-data (dataset-of self)) (cases-of self))))
    (if new-cases
      (setq new-args (list* :cases new-cases new-args)))
    (setq new-case-status
          (cond ((eq new-cases (cases-of self))
                 (copy-list (case-status-of self)))
                ((and (listp new-cases) (subsetp new-cases (cases-of self)))
                 (loop for d in new-cases collect (select-case-status self d)))
                (t nil)))
    (if new-case-status
      (setq new-args (list* :case-status new-case-status new-args)))
            

    (let ((f (getf args :function)))
      (unless (or f (eq layer 'function-view))
        (setq new-args 
              (append (list :function (func-of self)) new-args))))
    
    (apply #'view :type layer :draw? nil (append new-args args))))
    




 
(defmethod compute-default-clip-region ((view d-view) (on-view d-view))
  (bounding-region-of on-view))

(defmethod compute-default-clip-region ((view d-view) (on-view view))
  (bounding-region-of view))


(defmethod compute-ignore-x ((view 1d-view) )
  (if (eq (orientation-of view) :vertical)
    (not (linkable-bounds-x-p view))
    nil))

(defmethod compute-ignore-y ((view 1d-view) )
  (if (eq (orientation-of view) :horizontal)
    (not (linkable-bounds-y-p view)) nil))



(defmethod reshape-viewport ((self d-view) viewport  
                             &key new-location transform (draw? nil))

  ;; only one of transform or new-location should be provided
  
  (if (and  transform (null new-location) )
    (setq new-location (apply-transform transform viewport)))
    
  (if draw? (erase-view self :viewport viewport))              
  (setf (bounds-of viewport) (multiple-value-list (bounds-of new-location)))
    (map-to-viewport self viewport)
   (if draw? (draw-view self :viewport viewport :erase? nil)))



  
(defmethod subview-location-string ((self d-view) (subview view))
  (let ((c
         (car (coords-of self :cases (list (viewed-object-of subview))))))
    (if c (format nil "(~{~A~^,~})" c))))

(defmethod subview-location-string ((self 1d-view) (subview view))
  (let ((c
         (car (coords-of self :cases (list (viewed-object-of subview))))))
    (if c (format nil "~A" c))))
 


(defmethod description-string ((self d-view) )
  (if (has-variates-p self)
    (let ((v (coord-strings  self)))
    (format nil "~A: ~{~A~^,~}" (call-next-method) v ))
    (call-next-method)))

(defmethod use-x-axis-p ((self d-view))
  t)

(defmethod use-y-axis-p ((self d-view))
  t)

(defmethod use-x-axis-p ((self 1d-view))
  (eql (orientation-of self) :horizontal))

(defmethod use-y-axis-p ((self 1d-view))
  (eql (orientation-of self) :vertical))


(defmethod select-view-menu-list((view-symbol (eql '3d-view)))
  (loop for m in (view-menu-list '3d-view)
                     for y = (second m)
                     unless (or (eq y 'moving-cloud-mixin) (eq y '3d-one-per-case))
                     collect m))

(defmethod select-view-menu-list((view-symbol (eql '2d-view)))
  (loop with ans = (view-menu-list '2d-view) 
        for d in (list 'smoothed-2d-point-cloud 'lines 'smooth) do
        (setq ans (remove d ans :key #'second))
        finally (return ans)))


(defmethod select-view-menu-list((view-symbol (eql '1d-view)))
  (remove 'axis (view-menu-list '1d-view) :key #'second))

(defmethod select-view-menu-list((view-symbol (eql 'd-view)))
  (append (select-view-menu-list '1d-view)
          (select-view-menu-list '2d-view)
          (select-view-menu-list '3d-view)))
