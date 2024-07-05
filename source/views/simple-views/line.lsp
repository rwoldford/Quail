;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               line.lisp
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

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '( line line-wmenu 
            compute-line-endpoints 
            oriented-line oriented-line-wmenu
            set-line-slope set-line-intercept set-line-orientation)))


(defclass line (flip-mixin lines-mixin simple-view )
  (
   (slope :initarg :slope :initform nil  :accessor slope-of)
   (intercept :initarg :intercept :initform nil :accessor intercept-of))
  (:default-initargs :menu? t :orientation-menu? nil
    :documentation "line given by slope and intercept drawn in bounding region")
  )

(defclass line-menu-mixin  ()
  ((middle-menu :allocation :class :initform nil)))

(defclass line-wmenu  (line-menu-mixin line)
  ())


(defclass oriented-line (orientation-mixin line )
  ()
  (:default-initargs :orientation-menu? nil)
  (:documentation "a horizontal or vertical drawn at location in bounding region"))

(defclass oriented-line-wmenu  (line-menu-mixin oriented-line)
  ((middle-menu :allocation :class :initform nil)))







(defgeneric compute-line-endpoints (line )
 (:documentation "Use the bounding region to compute the end points of the line" ))

(defgeneric set-line-slope (line &key )
  (:documentation "sets the slope of line"))

(defgeneric set-line-intercept (line &key )
  (:documentation "sets the intercept of line"))

(defgeneric set-line-orientation (line &key )
  (:documentation "sets the orientation to horizontal or vertical"))
;;------------------------------------------------------------------------------


(defmethod initialize-instance :after  ((self line) &key orientation menu? intercept) 
  
  (if (eq (class-name (class-of self)) 'line)
    (cond ((and orientation menu?)
           (change-class self 'oriented-line-wmenu)
           (setf (orientation-of self) orientation)
           (setf (intercept-of self) intercept)
           (compute-line-endpoints self))
          (menu? (change-class self 'line-wmenu))
          (orientation 
           (change-class self 'oriented-line) 
           (setf (orientation-of self) orientation)
           (setf (intercept-of self) intercept)
           (compute-line-endpoints self))
          (t nil))))

                                    

(defmethod set-bounding-region :after ((self line) &key  &allow-other-keys) 
  (compute-line-endpoints self))

(defmethod lines-coords-of :before ((self line))
  (unless (slot-value self 'lines-coords)
    (compute-line-endpoints self)))

(defmethod compute-line-endpoints ((self line))
  (multiple-value-bind (l r b tp) (bounds-of (bounding-region-of self))
    (let* ((m (slope-of self))
           (c (intercept-of self))
           pl pr pb pt)
      (if (eql m :prompt)
        (setf (slope-of self)
              (setf m (wb::prompt-user :result-type t
                                 :read-type :eval
                                 :prompt-string "Enter new slope"))))
      (if (eql c :prompt)
        (setf (intercept-of self)
              (setf c (wb::prompt-user :result-type t
                                 :read-type :eval
                                 :prompt-string "Enter new intercept"))))
      
      (if (and (numberp m) (not (numberp c))) 
        (setf (intercept-of self) (setf c (/ (+ b tp) 2))))
      (setf (lines-coords-of self)
            (cond 
             ((eql m 0)
              (list (list l c) (list r c)))
             ((numberp m)
              (setq pb (if (<= l (/ (- b c) m) r) (list (/ (- b c) m) b)))
              (setq pt (if (<= l (/ (- tp c) m) r) (list (/ (- tp c) m) tp)))
              (setq pl (if (<= b (+ c (* m l)) tp) (list l (+ c (* m l) ))))
              (setq pr (if (<= b (+ c (* m r)) tp) (list r (+ c (* m r) ))))
              (remove-if #'null (list pl pb pt pr)))
             ((numberp c)
              (list (list c b) (list c tp)))
             (t
              (unless (= r l)
                (setf m (/ (- tp b) (- r l)))
                (setf (slope-of self) m)
                (setf (intercept-of self) (- b (* m l))))
              (list (list l b) (list r tp)))
             )))))
      
          


(defmethod get-menu-items ((self line-menu-mixin) (slot-name (eql 'middle-menu)))
  (let ((orientation 
         '(("Horizontal" (set-line-orientation :value :horizontal  ))
           ("Vertical" (set-line-orientation :value :vertical  )))))
         `( ("-" nil)
           ( "Slope"  (set-line-slope ))
           ( "Intercept"  (set-line-intercept ))
           ( "Orientation" nil "" :sub-items ,orientation)
           )))




(defmethod set-line-slope ((self line) &key value (draw? t))
  (if (null value)
    (setf value (wb::prompt-user :result-type t
                                 :read-type :eval
                                 :prompt-string "Enter new slope")))
  (if  (not (numberp value))
    (set-line-orientation self :value :vertical :draw? t)
    
    (let ((class-name (class-name (class-of self))))
      (if (and (typep self 'oriented-line)
               (eq (orientation-of self) :vertical))
        (setf (intercept-of self) nil))
      (cond ((eq class-name 'oriented-line)
             (change-class self 'line))
            ((eq class-name 'oriented-line-wmenu)
             (change-class self 'line-wmenu))
            (t nil))
      (if draw? (erase-view self))
      
      (setf (slope-of self) value)
      (compute-line-endpoints self)
      (if draw? (draw-view self)))))


(defmethod set-line-intercept ((self line) &key value (draw? t))
  (if draw? (erase-view self))
  (if (null value)
    (setf value (wb::prompt-user :result-type 'number
                                 :read-type :eval
                                 :prompt-string "Enter new intercept")))
  (setf (intercept-of self) value)
  (compute-line-endpoints self)
  (if draw? (draw-view self)))

(defmethod set-line-orientation ((self line) &key value (draw? t))
  (let ((class-name (class-name (class-of self))))
    (if (or (and (typep self '(not orientation-mixin))
                 (eq value :vertical))
            (and (typep self 'orientation-mixin)
                 (not (eq value (orientation-of self)))))
      (setf (intercept-of self) nil))
      
    (cond ((eq class-name 'line)
           (change-class self 'oriented-line))
          ((eq class-name 'line-wmenu)
           (change-class self 'oriented-line-wmenu))
          (t nil))
  (if draw? (erase-view self))
  
  (setf (orientation-of self) value)
  (compute-line-endpoints self)
  (if draw? (draw-view self))))




(defmethod compute-line-endpoints ((self oriented-line) )
  (let* ((br (bounding-region-of self))
         (xmin (left-of br))
         (xmax (right-of br))
         (ymin (bottom-of br))
         (ymax (top-of br))
         (locn (intercept-of self)))
     (setf (lines-coords-of self)
          (ecase (orientation-of self) 
            (:horizontal 
             (setf (intercept-of self)
                   (setq locn 
                         (cond ((eql locn :prompt)
                                (wb::prompt-user :result-type t
                                 :read-type :eval
                                 :prompt-string "Enter new y intercept"))
                               ((null locn)
                                (/ (+ ymin ymax) 2))
                               (t locn))))
             (list (list xmin locn)
                   (list xmax locn)))
            (:vertical 
             (setf (intercept-of self)
                   (setq locn 
                         (cond ((eql locn :prompt)
                                (wb::prompt-user :result-type t
                                 :read-type :eval
                                 :prompt-string "Enter new x intercept"))
                               ((null locn)
                                (/ (+ xmin xmax) 2))
                               (t locn))))

             
             (list (list locn ymin)
                   (list locn ymax)))))))



