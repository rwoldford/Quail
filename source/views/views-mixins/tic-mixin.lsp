;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               tic-mixin.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;
;;;  This file is part of the Views system, a toolkit for interactive statistical
;;;  graphics.
;;;  
;;;
;;;  Authors:
;;;     C.B. Hurley 1988-1991 George Washington University
;;;     R.W. Oldford 1988-1991
;;;     Originator: J.O. Pedersen 1985
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :views)

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(tic-mixin )))

(defclass tic-mixin ()
  ((format :accessor tic-format-of :initform nil :initarg :tic-format)
   (tic-interval :accessor tic-interval-of :initform nil )
   (ntics :initform (default-ntics)
          :initarg :ntics
          :accessor ntics-of )
   (tic-info :accessor tic-info-of :initform nil  )
   (tic-list  :initarg :tic-list :initform nil) 
   (internal-tics? :accessor internal-tics-p 
                   :initform nil 
                   :initarg :internal-tics?)))

(defun default-ntics () '(2 3 4 5 6 7 8))

(defun make-tic-info (&key (min 0.0) (max 1.0) (ntics 2) (increment 1.0))
  (list min max ntics increment))

(defmacro tic-min (ti) `(elt ,ti 0))
(defmacro tic-max (ti) `(elt ,ti 1))
(defmacro tic-inc (ti) `(elt ,ti 3))
(defmacro ntics (ti) `(elt ,ti 2))

(defun tic-interval-len (ti) (- (tic-max ti) (tic-min ti)))

(defmethod set-bounding-region :after ((self tic-mixin) 
                                       &key (pretty? t) 
                                       ignore-x? ignore-y? interval min max
                                       &allow-other-keys ) 
  (let ((dir (axis-of-orientation self)))
    
    (unless (or (and ignore-x? (eq dir :x)) (and ignore-y? (eq dir :y)))
      (setf (internal-tics-p self) (not pretty?))
      (if (and min max)
        (setf (extent-of self) (list min max)))
      (if interval
        (setf (extent-of self) interval))

      (get-tic-interval self)
      (if pretty?
        (expand-extent self (tic-interval-of self))))))

(defmethod get-tic-interval ((self tic-mixin) &optional min max)
  
  (let  (emin emax)
    (unless (and min max)
      (multiple-value-setq (emin emax) (extent-of self)))
    (setq min (or min emin))
    (setq max (or max emax))
    (let ((ti (tic-interval-of self))
          (tinf (slot-value self 'tic-info)))
      (if (and ti tinf    (= min (tic-min ti))
               (= max (tic-max ti)))
        ti
        (progn
          (choose-tics self min max)
          (choose-scale self min max))))))


(defmethod choose-tics ((self tic-mixin) min max)
  (setf (tic-info-of self) (if (internal-tics-p self)
                             (internal-tic-fn self  min max )
                             (tic-fn self  min max ))))


(defmethod choose-scale ((self tic-mixin)  min max)
  (setf (tic-interval-of self)
        (scale-fn  self   min max)))


(defmethod scale-fn ((self tic-mixin)  min max)
  (declare (ignore min max))
  (let ((ti (tic-info-of self)))
    (make-interval (tic-min ti) (tic-max ti))))


(defmethod tic-fn ((self tic-mixin) min max 
                   &optional (tics (ntics-of self)))
  
  (if (integerp tics) (setq tics (list tics)))
  
  (loop with shortest = (scale-axis  min max (first tics))
        for ntics in (cdr tics)
        for current = (scale-axis  min max ntics ) 
        when
        (< (tic-interval-len current) (tic-interval-len shortest)) 
        do (setq shortest current)
        finally (return shortest)))

(defun internal-scale-axis (min max ntics)
  (let* ((c (scale-axis  min max (+ 2 ntics)))
         (tinc (tic-inc c)))
    (loop while (< (tic-min c) min)
          do (incf (tic-min c) tinc)
          (decf (ntics c) 1))
    (loop while (> (tic-max c) max)
          do (decf (tic-max c) tinc)
          (decf (ntics c) 1))
    c))

(defmethod internal-tic-fn ((self tic-mixin) min max 
                            &optional (tics (ntics-of self)))
  
  (if (integerp tics) (setq tics (list tics)))
  (loop with longest = 
        (internal-scale-axis  min max (first tics))
        for ntics in (cdr tics)
        for c = (internal-scale-axis  min max ntics )
        do
        (if
          (> (tic-interval-len c) (tic-interval-len longest)) 
          (setq longest c))
        finally (return longest)))
   




(defun scale-axis ( min max ntics &optional
                        (round 
                         ;; Rounding Constants.  Notice that they are
                         ;; in decreasing order and end with 1.0
                         '(5.0 2.5 2.0 1.5 1.0))
                        power)
  "Scaling algorithm for plots.  NTICS is the desired number of ~
   tics.  Round is a list of acceptable scaling factors.  POWER ~
   is the power of ten to use.  Returns a TICINFO including ~
   NEWMAX, NEWMIN, INC, and NTICS."
  
  (when (= min max)
    (cond
     ((minusp max)
      (setf min (* 1.1 min))
      (setf max (* 0.9 max)))
     ((zerop max)
      (setf min -1)
      (setf max 1))
     (T
      (setf min (* 0.9 min))
      (setf max (* 1.1 max)))
     )
    )
  (if (and (< min 0)
           (> max 0)
           (= ntics 2))
    ;; failure of this algorithm!! ch
    (scale-axis2 min max round)
  (let* ((n-inc (1- ntics))
         (raw-inc (/ (float (- max min))  n-inc))
         mantissa index)
    
    ;; POWER is the power of ten
    (setq power (expt 10.0 (or power (floor (log raw-inc 10.0)))))
    
    ;; MANTISSA is the scale factor
    (setq mantissa (/ raw-inc power))
    (if (> mantissa (first round))
      (setq power (* 10.0 power)
            index (last round))
      (setq index (do ((mark round (cdr mark)))
                      ((null (cdr mark))
                       mark)
                    (if (> mantissa (second mark))
                      (return mark)))))
    
    ;; Find new max and new min
        (let ((new-max min)
          new-min inc factor lower-mult upper-mult)
      (loop (unless (< new-max max)
              (return (make-tic-info :min new-min :max new-max 
                                     :increment inc :ntics ntics)))
                                     
            (setq inc (* (first index) power))
            (setq factor (/ (- (+ max min) (* n-inc inc))  (* 2.0 inc)))
            (setq new-min (* inc (setq lower-mult (ceiling factor))))
                                       
            (if (> new-min min)
              (setq new-min (* inc (setq lower-mult (1- lower-mult)) )))
                               
            (if (and (>= min 0.0) (minusp new-min))
              (setq lower-mult 0 new-min 0.0))
            (setq upper-mult (+ lower-mult n-inc))
            (setq new-max (* inc upper-mult))
            (if (and (<= max 0.0)  (> new-max 0.0))
              (setq upper-mult 0 new-max 0.0 
                    lower-mult (- n-inc) new-min  (* inc lower-mult)))
            (if (eq round index)
              (setq index (last round) power (* 10.0 power))
              (setq index (do ((mark round (cdr mark)))
                              ((eq (cdr mark)
                                   index)
                               mark)))))
      ))))



(defun scale-axis2(min max round)
  (loop with newmin and newmax and wmin and wmax
        for r in round
        for i upfrom 0
        do
           (setq newmin (* r (floor min r)))
            (setq newmax (* r (ceiling max r)))
        (if (or (null wmin)
                (<= 0 (- newmax newmin) (- wmax wmin)))
          (setq wmin newmin wmax newmax))
        finally (return (make-tic-info :min wmin :max wmax 
                                     :increment (- wmax wmin) :ntics 2))))

(defmethod tic-info-of :before ((self tic-mixin))
  (unless (slot-value self 'tic-info)
    (get-tic-interval self)))

(defmethod (setf tic-list-of) (new-val (self tic-mixin) )
  (setf (slot-value self 'tic-list) new-val))

(defmethod tic-list-of ((self tic-mixin) )
  "Returns the tic list based on values of tic-info."
  (or (slot-value self 'tic-list)
      
      (let* ((tic-info (tic-info-of self))
             (ticinc (tic-inc tic-info)))
        (etypecase ticinc
          (list ticinc)
          (number (append
                   (do ((i 1 (+ i 1))
                        (x (tic-min tic-info) 
                           (+ x ticinc))
                        (result nil (push x result)))
                       ((>= i (ntics tic-info))
                        (nreverse result)))
                            (list (tic-max tic-info))))))))


#|
(defmethod get-tic-list ((self tic-mixin))
  (unless (tic-info-of self) (get-tic-interval self))
  (let ((tm (tic-method-of self)))
  (if  (listp tm) tm
     (funcall tm self))))

;;; old
(defun normalize-tic-list (tic-list &optional format
                                    (orientation :horizontal)
                                    (justification :bottom))
  (mapcar #'(lambda (tic)
              (let (value label)
                (if (listp tic)               
                  (setq value (car tic)
                        label (cadr tic))      
                  (setq value (setq label tic)))
                (cons value (if (numberp label)
                              (float-to-string label format
                                               orientation justification)
                              label))))
          tic-list))

(defun float-to-string (x &optional format 
                          (orientation :horizontal)
                          (justification :bottom))
  (if (= x (truncate x))
    (setq x (truncate x)))
  (cond
   (format (format NIL format x))
   (T
    (setf format
     (let ((abs-x (abs x)))
       (cond ((zerop abs-x) (format nil "~6,,,D" x))
             ((or (< abs-x 0.001) (>= abs-x 1.0E+7))
              (format nil "~6,3,,E" x))
             (t (format nil "~6,2,,F" x)))))
    (if (or (eq orientation :horizontal)
            (not (eq justification :right)))
      (string-left-trim '(#\Space) format)
      format)
    )
   )
  )
|#


(defun normalize-tic-list (tic-list &optional format
                                    (orientation :horizontal)
                                    (justification :bottom))
  (let ((default-format :decimal)
        normalized-tic-list
        )
    (cond
     ((stringp format)
      (setf normalized-tic-list
            (mapcar
             #'(lambda (tic)
                 (let (value label)
                   (if (listp tic)               
                     (setq value (car tic)
                           label (cadr tic))      
                     (setq value (setq label tic)))
                   (cons value (if (numberp label)
                                 (format NIL format
                                         (if (= label (truncate label))
                                           (truncate label)
                                           label))
                                 label))))
             tic-list)))
     (T
      ;; nice default format
      (setf format "~6,,,D")
      ;; collect up list and determine the best format to boot.
      (setf normalized-tic-list
            (mapcar #'(lambda (tic)
                        (let (value label abs-x)
                          (if (listp tic)               
                            (setq value (car tic)
                                  label (cadr tic))      
                            (setq value tic
                                  label tic))
                          ;; See if format should be changed
                          (when (numberp label)
                            (setf abs-x (abs label))
                            (cond
                             ((= label (truncate label))
                              (setq label (truncate label))
                              (when (> label 1.0E7)
                                  (setf default-format :exponential)
                                  (setf format "~6,3,,E")
                                  )
                              )
                             (T
                              (if (not (eq default-format :exponential))
                                (cond
                                 ((or (< abs-x 0.001) (>= abs-x 1.0E+7))
                                  (setf default-format :exponential)
                                  (setf format "~6,3,,E"))
                                 ((eq default-format :decimal)
                                  (setq default-format :Fixed-floating)
                                  (setq format "~6,2,,F"))))
                              )))
                          (cons value label)))
                    tic-list))
      
      (if (or (eq orientation :horizontal)
              (not (eq justification :right)))
        ;; trim leading space off each label 
        (setf normalized-tic-list
              (mapcar
               #'(lambda (tic)
                   (let (value label)             
                     (setq value (car tic)
                           label (cdr tic))
                     ;; Fix label if number
                     (if (numberp label)
                       (setf label
                             (string-left-trim '(#\Space)
                                               (format NIL format label))
                             )
                       )
                     (cons value label)))
               normalized-tic-list))
        ;; Run as plain
        (setf normalized-tic-list
              (mapcar #'(lambda (tic)
                          (let (value label)             
                            (setq value (car tic)
                                  label (cdr tic))  
                            ;; Fix label if number
                            (if (numberp label)
                              (setf label (format NIL format label))
                              )
                            (cons value label)))
                      normalized-tic-list))
        )
      )
     )
    normalized-tic-list)
  )




;;;
;;;  Note that there is now one of these for axis as well.
;;;  to take advantage of the justification
(defmethod tic-list-for-viewport ((self tic-mixin)  vp )
  (let* ((orientation (orientation-of self))
         (tic-list
         (normalize-tic-list (tic-list-of self)
                             (tic-format-of self)
                             orientation
                             ))
        (map (select-map-to-viewport self vp) )
        offset scale min max)
    (if (eql orientation :horizontal)
      (setf offset (x-shift map) scale (x-scale map)
            min (left-of vp) max (right-of vp))
      (setf offset (y-shift map) scale (y-scale map)
            min (bottom-of vp) max (top-of vp)))
    (loop for tic in tic-list
          for tic-pos-vp = (round (+ offset (* scale (car tic))))
          when (and (>= tic-pos-vp (- min 2)) (<= tic-pos-vp (+ 2 max)))
          collect (cons tic-pos-vp (cdr tic)))))
