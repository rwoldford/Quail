;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               function-view.lisp
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
;;;     C.B. Hurley 1988-1992 George Washington University
;;;     R.W. Oldford 1988-1991
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------

(in-package :views)

(eval-when (:compile-toplevel :load-toplevel :execute)
(export  '( function-view 
            compute-lines-coords
            set-function set-nlines )))

(defclass function-view (flip-mixin lines-mixin  simple-view )
  ((middle-menu :allocation :class :initform nil)
   (domain :initarg :domain :initform :prompt :accessor domain-of)
   (nlines :initarg :nlines :initform 20 :accessor nlines-of)
   (function 
    :initform :prompt
    :initarg :function
    :accessor function-of
    :documentation  "Function applied ")
   )
  (:documentation "function plotted in bounding region"))


(defgeneric set-function (function &key )
  (:documentation "sets the function plotted")) 

(defgeneric set-nlines (function &key )
  (:documentation "sets the number of line segments used "))


 

(defmethod set-bounding-region :after ((self function-view) 
                                       &key domain   &allow-other-keys)
  
  (when (or domain (eql (domain-of self) :prompt) )
    (setq domain (or domain (domain-of self)))
    (let ((br (bounding-region-of self)))
      (when (eql domain :prompt)
        (setq domain (loop for l in
                           (wb::collect-input  
                            (list (cons "from" "0.0")
                                  (cons "to" "1.0")) :prompt-text "Enter function domain.")
                           collect
                           (eval (read-from-string (cdr l)))))
        (setf (domain-of self) domain))
      (cond ((interval-p domain)
             (setf (left-of br) (min-of domain))
             (setf (right-of br) (max-of domain)))
            (t
             (setf (left-of br) (car domain))
             (setf (right-of br) (second domain))))
      
      (let ((lines-coords (compute-lines-coords self)))
        (loop for (x y) in lines-coords
              maximize y into top
              minimize y into bottom
              finally
              (setf (bottom-of br) bottom)
              (setf (top-of br) top))))))

(defmethod lines-coords-of :before ((self function-view))
  (unless (slot-value self 'lines-coords)
    (compute-lines-coords self)))

(defmethod compute-lines-coords ((self function-view) )
  (let* ((br (bounding-region-of self))
         (dom (domain-of self))
         (xmin (if dom (first dom) (left-of br)))
         (xmax (if dom (second dom) (right-of br)))
         (n (nlines-of self) )
         (f (function-of self))) 
    (setq f (get-function f))
         
    (setf (lines-coords-of self)
          (loop for x in (ordinates-for-fplot (list xmin xmax) (+ n 1))
                collect
                (list x (funcall f x))))))




(defun ordinates-for-fplot(x  &optional (n-ordinates 20))
  (loop for xi in x
        minimize xi into minx maximize xi into maxx
        finally (return 
                 (loop with xi = minx
                       with inc-x = (float (/ (- maxx minx) (- n-ordinates 1)))
                       for i from 1 to n-ordinates
                       collect xi
                       do (setq xi (min (+ xi inc-x) maxx))))))



(defmethod get-menu-items ((self function-view) (slot-name (eql 'middle-menu)))
  '( ("-" nil)
           ( "Function"  (set-function))
           ( "# Lines"  (set-nlines))
           
           ))



(defmethod function-of :before ((self function-view))
  (if (eq :prompt (slot-value self 'function))
    (let ((f (wb::prompt-user :type  t
                              :prompt-string "Enter function"
                              :read-type :read)))
      (setf (slot-value self 'function)
            (if (and (listp f) 
                     (symbolp (car f)) (fboundp (car f)))
              (eval f)  
              f)))))



(defmethod set-function ((self function-view) &key value (draw? t))
  
  (if draw? (erase-view self))
  (if (null value)
    (setf value (wb::prompt-user :type t ;;(or 'symbol)
                                 :prompt-string "Enter new function"
                                 :read-type :read)))
  (setf (function-of self) value)
  (compute-lines-coords self)
  (if draw? (draw-view self)))


(defmethod set-nlines ((self function-view) &key value (draw? t))
  
  (if draw? (erase-view self))
  (if (null value)
    (setf value (wb::prompt-user :type 'integer
                                 :prompt-string "Enter number of segments"
                                 :read-type :eval)))
  (setf (nlines-of self) value)
  (compute-lines-coords self)
  (if draw? (draw-view self)))



(defmethod coord-string-y ((self function-view))
  (let ((f (function-of self))  )
    (if (or (eql f 'identity) (eql f #'identity))
      (call-next-method)
    (princ-to-string (function-string f)))))



(defmethod make-layer-view ((self function-view) (layer symbol) &rest args)
     (apply #'view :type layer :draw? nil :domain (domain-of self) args))
         

(defmethod use-x-axis-p ((self function-view))
  t)

(defmethod use-y-axis-p ((self function-view))
  t)