;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               change-var.lisp
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

;;;----------------------------------------------------------------------------------




(defmethod x-variate-links-of ((self 2d-view))
  (if (link-vars-p self)
    (loop with x = (x-variate-of self)
          for v in (link-bounds-x-of self) 
          when (or (and (typep v '2d-view ) (link-vars-p v)
                        (eq-variate-exprs (x-variate-of v) x))
                   (and (typep v '1d-view ) (link-vars-p v)
                        (eq-variate-exprs (variate-of v) x)))
          collect v)
    (list self)))

(defmethod y-variate-links-of ((self 2d-view))
  (if (link-vars-p self)
    (loop with y = (y-variate-of self)
          for v in (link-bounds-y-of self) 
          when (or (and (typep v '2d-view ) (link-vars-p v)
                        (eq-variate-exprs (y-variate-of v) y))
                   (and (typep v '1d-view ) (link-vars-p v)
                        (eq-variate-exprs (variate-of v) y)))
          collect v)
    (list self)))

(defmethod x-variate-links-of ((self 3d-view))
  (if (link-vars-p self)
    (loop with x = (x-variate-of self)
          for v in (link-bounds-x-of self) 
          when (or (and (typep v '3d-view ) (link-vars-p v)
                        (eq-variate-exprs (x-variate-of v) x))
                   (and (typep v '2d-view ) (link-vars-p v)
                        (eq-variate-exprs (x-variate-of v) x))
                   (and (typep v '1d-view ) (link-vars-p v)
                        (eq-variate-exprs (variate-of v) x)))
          collect v)
    (list self)))

(defmethod y-variate-links-of ((self 3d-view))
  (if (link-vars-p self)
    (loop with y = (y-variate-of self)
          for v in (link-bounds-y-of self) 
          when (or (and (typep v '3d-view ) (link-vars-p v)
                        (eq-variate-exprs (y-variate-of v) y))
                   (and (typep v '2d-view ) (link-vars-p v)
                        (eq-variate-exprs (y-variate-of v) y))
                   (and (typep v '1d-view ) (link-vars-p v)
                        (eq-variate-exprs (variate-of v) y)))
          collect v)
    (list self)))

(defmethod z-variate-links-of ((self 3d-view))
  (if (link-vars-p self)
    (loop with z = (z-variate-of self)
          for v in (link-bounds-of self) 
          when (and (typep v '3d-view ) (link-vars-p v)
                        (eq-variate-exprs (z-variate-of v) z))
          collect v)
    (list self)))


(defmethod variate-links-of ((self 3d-view))
  (if (link-vars-p self)
    (union  (x-variate-links-of self)
            (union (y-variate-links-of self) (z-variate-links-of self)))))

(defmethod variate-links-of ((self d-view))
  NIL)


(defmethod variate-links-of ((self 2d-view))
  (union (x-variate-links-of self) (y-variate-links-of self)))


(defmethod variate-links-of ((self 1d-view))
  (if (link-vars-p self)
    (let ((var (variate-of self)))
      (if (eq (orientation-of self ) :horizontal)
        (loop  for v in (link-bounds-of self) 
               when (or (and (typep v '2d-view ) (link-vars-p v)
                             (eq-variate-exprs (x-variate-of v) var))
                        (and (typep v '1d-view ) (link-vars-p v)
                             (eq-variate-exprs (variate-of v) var)))
               collect v)
        (loop  for v in (link-bounds-y-of self) 
               when (or (and (typep v '2d-view ) (link-vars-p v)
                             (eq-variate-exprs (y-variate-of v) var))
                        (and (typep v '1d-view ) (link-vars-p v)
                             (eq-variate-exprs (variate-of v) var)))
               collect v)))
    (list self)))

;;;----------------------------------------------------------------------------------

(defmethod new-variable ((self d-view) &rest args &key  )
  
  (apply #'new-sub-views self args)
  (set-bounding-region self )
  )




(defmethod new-variable ((self 2d-view) &rest args &key 
                            x y x-function y-function
                            (x-transform :none) (y-transform :none))
  
  (let* ((old-nans? (invalid-cases-p self))
         new-nans? ignore-x? ignore-y?)

    (apply #'new-sub-views self args)
    (setq new-nans? (member :nan (case-status-of self)))
    (setq ignore-x? (and (null x) (null x-function) (eq :none x-transform) 
                         (null old-nans?) (null new-nans?)))
    (setq ignore-y? (and (null y) (null y-function) (eq :none y-transform) 
                         (null old-nans?) (null new-nans?)))
    (set-bounding-region self :ignore-x? ignore-x?
                         :ignore-y? ignore-y?)
    ))



(defmethod new-variable :before  ((self d-view) &key which
                                  var function (transform :none))
  
  (setf (coords-cache-of self) nil)
  (let ((p (and which (position which (vars-of self) :test #'eq-variate-exprs))))
    (when p
      (if var (setf (elt (vars-of self) p) var))
      (if function (setf (elt (funcs-of self) p) function))
      (unless (eq :none transform) (setf (elt (transforms-of self) p) transform)))))
  

(defmethod new-variable :before  ((self 1d-view) &key
                                     var function (transform :none))
                                     
  (if var (setf (variate-of self) var))
  (if function (setf (func-of self) function))
  (unless (eq :none transform) (setf (transform-of self) transform))
  )

(defmethod new-variable :before  ((self 2d-view) &key 
                                     x  x-function  (x-transform :none)
                                     y y-function (y-transform :none) )
                                     
  (if x (setf (x-variate-of self) x))
  (if y (setf (y-variate-of self) y))
  (if x-function (setf (x-func-of self) x-function))
  (if y-function (setf (y-func-of self) y-function))
  (unless (eq :none x-transform) (setf (x-transform-of self) x-transform))
  (unless (eq :none y-transform) (setf (y-transform-of self) y-transform))
  )

(defmethod new-variable :before  ((self 3d-view) &key 
                                    x  x-function  (x-transform :none)
                                     y y-function (y-transform :none) 
                                     z z-function (z-transform :none))
  (if x (setf (x-variate-of self) x))
  (if y (setf (y-variate-of self) y))
  (if z (setf (z-variate-of self) z))
  (if x-function (setf (x-func-of self) x-function))
  (if y-function (setf (y-func-of self) y-function))
  (if z-function (setf (z-func-of self) z-function))
  (unless (eq :none x-transform) (setf (x-transform-of self) x-transform))
  (unless (eq :none y-transform) (setf (y-transform-of self) y-transform))
  (unless (eq :none z-transform) (setf (z-transform-of self) z-transform))
  )



(defmethod new-variable :around ((self d-view) &key (draw? nil) )
  
  (if draw?  (erase-view self))
  (call-next-method)
  (init-position-subviews self)
  (if draw?
    (remap-to-viewports self :erase? nil)))



(defmethod new-sub-views ((self d-view) &rest args 
                          &key reposition? (relink? t) )
  (let* ((old-subs (subviews-of self))
         (link-table (if (and old-subs relink?)
                       (link-table-of (car old-subs)))))
    
    
    (loop for subview in old-subs 
          do 
          ;;(break-all-links subview)
          (loop for vp in (viewports-of self)
                for vp-sub = (select-viewport subview vp) 
                do
                (delete-viewport subview vp-sub)))
    
    
    (delete-all-subviews self)
    (apply #'construct-sub-views self args)
    
    (if (and relink? link-table)
      (loop for sub in (subviews-of self) do
            (link-view sub :link-table link-table :draw? nil)))
    (if reposition? (init-position-subviews self))))




;;===================================================================================================
(defmethod free-axis((self d-view) link)
  ;; which directions of self are unconstrained by link
  (cond ((eq link self) nil)
        ((eq link t) :both)
        ((typep link 'view)
         (let ((x? (member self (link-bounds-x-of link)))
               (y? (member self (link-bounds-y-of link))))
           (cond ((and x? y? ) nil)
                 (x? :y)
                 (y? :x)
                 (t :both))))
        (t nil)))

(defmethod change-variable  ((self d-view) &rest args
                                         &key (draw? t ) function )
  (when (eq function :prompt)
    (setq function (wb::prompt-user :result-type  t
                                    :prompt-string "Enter function"
                                    :read-type :read))
    
    (if (and (listp function) 
             (symbolp (car function)) (fboundp (car function)))
      (setq function (eval function) )))
  (apply #'new-variable self :draw? draw? :function function args))

(defmethod change-variable :around ((self d-view) &rest args
                                         &key (link? t))

  (cond ((null link?)  (apply #'new-variable self args))
        ((= 1 (length (link-bounds-of self)))
           (apply #'new-variable self  args))
        ((free-axis self link?)
         (call-next-method))
        (t  (apply #'new-variable self :draw? nil args))))


(defmethod change-variable ((self 2d-view) &rest args
                                 &key x y x-function y-function
                                 (x-transform :none) (y-transform :none) 
                                 (draw? t) (link? t))
  (if (eq x-function :prompt)
    (setq x-function (wb::prompt-user :result-type  t
                           :prompt-string "Enter X function"
                           :read-type :read)))
  (if (eq y-function :prompt)
    (setq y-function (wb::prompt-user :result-type  t
                           :prompt-string "Enter Y function"
                           :read-type :read)))
  (let ((new-x? (or x x-function (not (eq :none x-transform))))
        (new-y? (or y y-function (not (eq :none y-transform))))
        (old-nans? (invalid-cases-p self))
        (free-dir (free-axis self link?)))
    (flet ((pass-to-links ()
             (if new-x?
               (loop for v in (x-variate-links-of self) do
                     (if (typep v '2d-view )
                       (change-variable v :x x :x-function x-function 
                                             :x-transform x-transform 
                                             :draw? draw? :link? self )
                       (change-variable v :var x :function x-function 
                                               :transform x-transform 
                                               :draw? draw? :link? self ))))
             (if new-y?
               (loop for v in (y-variate-links-of self) do
                     (if (typep v '2d-view )
                       (change-variable v  :y y :y-function y-function
                                             :y-transform y-transform 
                                             :draw? draw? :link? self )
                       (change-variable v :var y :function y-function 
                                               :transform y-transform 
                                               :draw? draw? :link? self ))))))
      (cond
       ((and (eq free-dir :both)
             (or (and new-x? new-y?) old-nans? ))
        (with-constrained-extents self  draw? 
          (pass-to-links)))
       
       ((or (eq free-dir :both) (eq free-dir (if new-x? :x :y)))
        (with-constrained-extent self (if new-x? :x :y)  draw? 
          (pass-to-links)))

       ((and old-nans?
             (or (eq free-dir :both) (not (eq free-dir (if new-x? :x :y)))))
        (with-constrained-extent self (if new-x? :y :x)  draw? 
          (apply #'new-variable self :draw? nil :x-function x-function
                 :y-function y-function args)))

        (t (apply #'new-variable self :draw? nil :x-function x-function
                 :y-function y-function args)
           )
        ))))

(defmethod change-variable :around ((self 2d-view) &rest args
                                         &key (draw? t) (link? t) which var function (transform :none))
  (let ((find-var (and which (or var function (not (eq :none transform))))))
  (cond ((and find-var (eq-variate-exprs which (x-variate-of self)))
         (call-next-method self :which nil :link? link? :draw? draw?
                :x var :x-function function :x-transform transform))

        ((and find-var (eq-variate-exprs which (y-variate-of self)))
         (call-next-method self :which nil :link? link? :draw? draw?
                :y var :y-function function :y-transform transform))
        (t (apply #'call-next-method self :link? link? :draw? draw?  :which nil args)))))
     

(defmethod change-variable ((self 1d-view) &rest args
                            &key var function (transform :none) (draw? t) (link? t) )
   (if (eq function :prompt)
    (setq function (wb::prompt-user :result-type  t
                           :prompt-string "Enter function"
                           :read-type :read)))
   (let ((axis (axis-of-orientation self))
        (free-dir (free-axis self link?)))
    (flet ((pass-to-links()
             (loop for v in (variate-links-of self)
                   do
                   (cond 
                    ((and (eq axis :x) (typep v '2d-view ))
                     (change-variable v :x var :x-function function 
                                      :x-transform transform 
                                      :draw? draw? :link? self ))
                    ((and (eq axis :y) (typep v '2d-view ))
                     (change-variable v :y var :y-function function 
                                      :y-transform transform 
                                      :draw? draw? :link? self ))
                    
                    ((typep v '1d-view )
                     (change-variable v :var var :function function 
                                      :transform transform :draw? draw? :link? self )
                     )))))
      (if (or (eq free-dir :both) (eq free-dir axis))
        (with-constrained-extent self axis draw? (pass-to-links))
       (apply #'new-variable self :draw? nil :function function args)))))






          
          

     

