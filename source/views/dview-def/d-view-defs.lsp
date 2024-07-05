;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               d-view-defs.lisp
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

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(d-view 1d-view 2d-view 3d-view 
           variates-of
           variate-of 
           transform-of 
           function-of
           x-variate-of
           x-transform-of
           x-function-of
           y-variate-of 
           y-transform-of
           y-function-of
           z-variate-of 
           z-transform-of
           z-function-of
           link-bounds-of 
           coords-of plot-coords-of compute-coords
           summarize case-status-of
           orientation-of 
           coord-string coord-string-x coord-string-y coord-string-z 
           new-variable change-variable smallest-bounding-region
           new-case-status change-case-status
           activate-cases deactivate-cases activate-all-cases
           ncases set-link-cases-p set-link-vars-p default-dview-function
           default-dview-transform coord-strings
           )))

;;;----------------------------------------------------------------------------------
(defgeneric default-dview-function (variate subject)
  (:documentation "Returns the default  function applied to the~
                   value of variate"))

(defmethod default-dview-function((variate t) (subject t))
  
  #'identity)

(defgeneric default-dview-transform (variate data)
  (:documentation "Returns the default  transform applied to the~
                   value of variate"))

(defmethod default-dview-transform((variate t) (data t))
  
  #'identity)


#|

(defun coerce-data-args-for-dview(&key dataset data cases &allow-other-keys)
  (setq dataset (or dataset data))
   (setq cases
            (construct-viewed-object-list dataset cases))
  (list :data data :cases cases :dataset dataset ))
|#

(defun compose-initform-fns(fn1 fn2)
  #'(lambda(&rest args)
      (let ((ans1 (apply fn2 args)))
      (append (apply fn1 (append ans1 args)) ans1))))

(defclass d-view (data-menu-item-mixin data-extract-mixin flip-mixin) 
  ((vars
    ;;:initarg :vars :initform nil
    :accessor vars-of 
    :documentation "Function or variate names ")
   (functions 
    :accessor funcs-of
    ;;:initarg :functions :initform nil
    :documentation  "Functions applied to vars")
   (transforms 
   ;;:initarg :transforms  :initform nil
    :accessor transforms-of 
    :documentation  "transform  applied to coords")
   (coords-cache 
    :initform nil :initarg :coords
    :accessor coords-cache-of)
   (case-status 
    :initform nil :initarg :case-status
    :accessor case-status-of
    :documentation "allows some cases to be ignored when computing coords, and drawing subs")
   (bounding-region 
    :initarg  :plot-region
    :initform nil 
    :accessor bounding-region-of 
    :documentation "A region in the local coordinates of view used to define its bounds")
   (text-links
    :initform nil
    :initarg :text-links
    :accessor text-links-of )
   (link-cases?
    :initform t
    :initarg :link-cases?
    :accessor link-cases-p )
   (link-vars?
    :initform t
    :initarg :link-vars?
    :accessor link-vars-p )
   (value-fn :initarg :value-fn  :accessor value-fn-of)
   (middle-menu :allocation :class :initform nil)
   )
  (:default-initargs :initform-fn #'get-data-inits)
  (:documentation "Has a viewed-object which is a list of viewed-objects (cases)."))



(defclass 1d-view (orientation-mixin d-view)
  ((right-menu :allocation :class :initform nil)
   (middle-menu :allocation :class :initform nil))
  (:default-initargs :initform-fn #'get-data-inits-1
     :function nil :transform nil :orientation-menu? nil))
   
 
(defclass 2d-view (d-view)
  ((middle-menu :allocation :class :initform nil)
   (right-menu :allocation :class :initform nil))
  (:default-initargs :initform-fn #'get-data-inits-2
     :x-function nil :x-transform nil
     :y-function nil :y-transform nil))

(defclass 3d-view (d-view)
  ((middle-menu :allocation :class :initform nil))
  (:default-initargs :initform-fn #'get-data-inits-3
     :x-function nil :x-transform nil
     :y-function nil :y-transform nil
    :z-function nil :z-transform nil))




;;;----------------------------------------------------------------------------------   
(defgeneric set-link-cases-p (d-view val)
  (:documentation "Changes link-cases-p, which controls whether~
                   changes in case status are passed on to other views."))

(defgeneric set-link-vars-p (d-view val)
  (:documentation "Changes link-vars-p, which controls whether~
                   changes in variables are passed on to other views."))

(defgeneric ncases(d-view)
  (:documentation "Returns number of cases(cases)"))
  
(defgeneric coord-string (d-view)
  (:documentation "Returns a string describing coordinates"))


(defgeneric coord-strings (d-view)
  (:documentation "Returns a string describing coordinates"))

(defgeneric coord-string-x (d-view)
  (:documentation "Returns a string describing x coordinates"))

(defgeneric coord-string-y (d-view)
  (:documentation "Returns a string describing y coordinates"))

(defgeneric coord-string-z (d-view)
  (:documentation "Returns a string describing z coordinates"))
;;;----------------------------------------------------------------------------------
(defgeneric get-cache-coords (d-view )
  (:documentation "Computes and caches and returns the list of coords from data"))

(defgeneric coords-of (d-view &key cases)
  (:documentation "Returns list of coords from data"))

(defgeneric plot-coords-of (d-view &key cases &allow-other-keys)
  (:documentation "Returns list of coords used in plot~
                   not necessarily the same as  result of coords-of"))

(defgeneric compute-coords (d-view &key cases)
  (:documentation "Computes and returns list of coords from data"
                  ))


;;;----------------------------------------------------------------------------------



(defgeneric summarize (d-view  function &key variate predicate )
  (:documentation "Return result of applying function to coords for variate satisfying predicate.~
                   Variate is ignored for 1d-view"))
;;;----------------------------------------------------------------------------------



(defgeneric new-sub-views (d-view  &key &allow-other-keys)
  (:documentation "Remove current subviews and compute new ones. ~
                   Use the styles from old views and preserve links."))

(defgeneric remake-sub-views (d-view  &key &allow-other-keys)
  (:documentation "Compute new subviews . ~
                   Use the styles from old views and preserve links."))



 
;;;----------------------------------------------------------------------------------
(defgeneric new-variable (d-view &key &allow-other-keys)
  (:documentation "Change any of variate, function or transform"))

(defgeneric change-variable (d-view &key &allow-other-keys)
  (:documentation "Changes any of variate, function or transform~
                   in self and views with linked bounds"))

(defgeneric change-case-status (d-view cases status  &key &allow-other-keys)
  (:documentation "Change status of cases in self and links"))

(defgeneric new-case-status (d-view cases status  &key &allow-other-keys)
  (:documentation "Change status of cases"))

(defgeneric activate-cases (d-view  cases &key &allow-other-keys)
  (:documentation "Change status of cases to activate in self and links"))

(defgeneric activate-all-cases (d-view  &key &allow-other-keys)
  (:documentation "Change status of all cases to activate in self and links"))

(defgeneric deactivate-cases (d-view  cases &key &allow-other-keys)
  (:documentation "Change status of cases to inactive in self and links "))

;;;----------------------------------------------------------------------------------
(defgeneric compute-case-status (d-view)
  (:documentation "Computes the status for cases in d-view ~
                   Returns and stores a case status list"))


(defgeneric select-case-status (d-view case)
  (:documentation "Returns status of case "))

(defgeneric active-members (d-view list)
  (:documentation "Returns active members of list.~
                   List should have same length as viewed object "))

(defgeneric inactive-members (d-view list)
  (:documentation "Returns inactive members of list.~
                   List should have same length as viewed object "))

(defgeneric smallest-bounding-region (d-view)
  (:documentation "Returns the smallest region containing d-view"))

(defgeneric functions-of (d-view))
 
;(defgeneric coord-strings (d-view)) defined at line 168

;;;----------------------------------------------------------------------------------

(defmethod initialize-instance :before ((self d-view)
                                        &key vars functions transforms) 
  (unless (slot-boundp self 'vars)
    (setf (slot-value self 'vars)  vars))
  (unless (slot-boundp self 'functions)
    (setf (slot-value self 'functions)  (or functions (make-list (length (vars-of self))))))
  (unless (slot-boundp self 'transforms)
    (setf (slot-value self 'transforms)  (or transforms (make-list (length (vars-of self))))))) 

(defmethod initialize-instance :before ((self 1d-view)
                                        &key var function transform)
                                      
  (if var (setf (slot-value self 'vars) (list var)))
  (if function (setf (slot-value self 'functions) (list function)))
  (if transform
    (setf (slot-value self 'transforms) (list transform))))
  

(defmethod initialize-instance :before ((self 2d-view) &key 
                                        x x-function  x-transform
                                        y y-function y-transform) 
  (if (or x y)
    (setf (slot-value self 'vars) (list x y)))
  (if (or x-function y-function)
    (setf (slot-value self 'functions) (list x-function y-function)))
  (if (or x-transform y-transform)
    (setf (slot-value self 'transforms) (list x-transform y-transform))))


(defmethod initialize-instance :before ((self 3d-view) &key 
                                        x x-function  x-transform
                                        y y-function y-transform
                                        z z-function z-transform)
  (setf (slot-value self 'vars) (list x y z))
  (setf (slot-value self 'functions) (list x-function y-function z-function))
  (setf (slot-value self 'transforms) (list x-transform y-transform z-transform)))


;;;----------------------------------------------------------------------------------




  
(defmethod variate-of ((self 1d-view))
  (car (vars-of self)))

(defmethod func-of ((self 1d-view))
  (let ((f (slot-value self 'functions)))
    (or (car f)
        (if f
        (setf (car f)
            (default-dview-function 
              (car (vars-of self)) (car (cases-of self))))))))

(defmethod transform-of ((self 1d-view))
  (let ((tr (slot-value self 'transforms)))
    (or (car tr)
        (if tr 
          (setf (car tr)
            (default-dview-transform
              (car (vars-of self)) (dataset-of self)))))))
 

(defmethod (setf variate-of) ( new (self 1d-view))
  (if (and (func-of self)
                     (eq (func-of self) 
                         (default-dview-function (variate-of self)
                           (car (cases-of self)))))
              (setf (func-of self) nil))
  (if (and (transform-of self)
                     (eq (transform-of self) 
                         (default-dview-transform (variate-of self)
                           (dataset-of self))))
              (setf (transform-of self) nil))
  (setf (car (vars-of self)) new))

(defmethod (setf func-of) ( new (self 1d-view))
  (setf (car (funcs-of self)) new))

(defmethod (setf transform-of) ( new (self 1d-view))
  (setf (car (transforms-of self)) new))
;;;----------------------------------------------------------------------------------

  
(defmethod x-variate-of ((self d-view))
  (car (vars-of self)))

(defmethod y-variate-of ((self d-view))
  (cadr (vars-of self)))

(defmethod z-variate-of ((self d-view))
  (caddr (vars-of self)))

(defmethod funcs-of ((self d-view))
  (let ((vo (car (cases-of self)))
        (fs (slot-value self 'functions)))
    (loop for f in fs
          for v in (vars-of self)
          for i upfrom 0
          when (null f)
          do
          (setf (nth i fs) (default-dview-function v vo)))
    fs))

(defmethod transforms-of ((self d-view))
  (let ((vo (dataset-of self))
        (fs (slot-value self 'transforms)))
    (loop for f in fs
          for v in (vars-of self)
          for i upfrom 0
          when (null f)
          do
          (setf (nth i fs) (default-dview-transform v vo)))
    fs))
          


(defmethod x-func-of ((self d-view))
  (let ((f (slot-value self 'functions)))
    (or (car f)
       (if f
         (setf (car f)
            (default-dview-function 
              (car (vars-of self)) (car (cases-of self))))))))

(defmethod y-func-of ((self d-view))
  (let ((f (slot-value self 'functions)))
    (or (cadr f)
        (if f
        (setf (cadr f)
            (default-dview-function 
              (cadr (vars-of self)) (car (cases-of self))))))))



(defmethod z-func-of ((self d-view))
  (let ((f (slot-value self 'functions)))
    (or (caddr f)
        (if f
        (setf (caddr f)
            (default-dview-function 
              (caddr (vars-of self)) (car (cases-of self))))))))

(defmethod x-transform-of ((self d-view))
  (let ((f (slot-value self 'transforms)))
    (or (car f)
        (if f
        (setf (car f)
            (default-dview-transform
              (car (vars-of self)) (dataset-of self)))))))

(defmethod y-transform-of ((self d-view))
  (let ((f (slot-value self 'transforms)))
    (or (cadr f)
        (if f
        (setf (cadr f)
            (default-dview-transform
              (cadr (vars-of self)) (dataset-of self)))))))

(defmethod z-transform-of ((self d-view))
  (let ((f (slot-value self 'transforms)))
    (or (caddr f)
        (if f
        (setf (caddr f)
            (default-dview-transform
              (caddr (vars-of self)) (dataset-of self)))))))

(defmethod (setf vars-of) ( new (self d-view))
  (loop with vos = (dataset-of self)
        with vo = (car (cases-of self))
        for f in (funcs-of self)
        for tr in (transforms-of self)
        for v in (vars-of self)
        for i upfrom 0 
        when
        (and f
             (eq f 
                 (default-dview-function v
                   vo))) do
        (setf (nth i (funcs-of self)) nil)
        when
        (and tr
             (eq tr
                 (default-dview-transform v
                   vos))) do
        (setf (nth i (transforms-of self)) nil))
  (setf (vars-of self) new))


(defmethod (setf x-variate-of) ( new (self d-view))
  (if (and (x-func-of self)
                     (eq (x-func-of self) 
                         (default-dview-function (x-variate-of self)
                           (car (cases-of self)))))
              (setf (x-func-of self) nil))
  (if (and (x-transform-of self)
                     (eq (x-transform-of self) 
                         (default-dview-transform (x-variate-of self)
                           (dataset-of self))))
              (setf (x-transform-of self) nil))
  (setf (car (vars-of self)) new))

(defmethod (setf y-variate-of) ( new (self d-view))
  (if (and (y-func-of self)
                     (eq (y-func-of self) 
                         (default-dview-function (y-variate-of self)
                           (car (cases-of self)))))
              (setf (y-func-of self) nil))
  (if (and (y-transform-of self)
                     (eq (y-transform-of self) 
                         (default-dview-transform (y-variate-of self)
                           (dataset-of self))))
              (setf (y-transform-of self) nil))
  (setf (cadr (vars-of self)) new))

(defmethod (setf z-variate-of) ( new (self d-view))
  (if (and (z-func-of self)
                     (eq (z-func-of self) 
                         (default-dview-function (z-variate-of self)
                           (car (cases-of self)))))
              (setf (z-func-of self) nil))
  (if (and (z-transform-of self)
                     (eq (z-transform-of self) 
                         (default-dview-transform (z-variate-of self)
                           (dataset-of self))))
              (setf (z-transform-of self) nil))
  (setf (caddr (vars-of self)) new))

(defmethod (setf x-func-of) ( new (self d-view))
  (setf (car (funcs-of self)) new))

(defmethod (setf y-func-of) ( new (self d-view)) 
  (setf (cadr (funcs-of self)) new) )

(defmethod (setf z-func-of) (new (self d-view))
  (setf (caddr (funcs-of self)) new))

(defmethod (setf x-transform-of) ( new (self d-view))
  (setf (car (transforms-of self)) new))

(defmethod (setf y-transform-of) ( new (self d-view))
  (setf (cadr (transforms-of self)) new))

(defmethod (setf z-transform-of) (new (self d-view))
  (setf (caddr (transforms-of self)) new))


(defmethod set-link-cases-p ((self d-view) val)
  (if (eq val :toggle)
    (setf (link-cases-p self)  (not (link-cases-p self)))
    (setf (link-cases-p self) val)))

(defmethod set-link-vars-p ((self d-view) val)
  (if (eq val :toggle)
    (setf (link-vars-p self)  (not (link-vars-p self)))
    (setf (link-vars-p self) val)))



