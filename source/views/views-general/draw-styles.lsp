;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               draw-styles.lisp
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
(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(drawing-style-p make-drawing-style list-style-keys
           has-draw-style-p draw-style set-draw-style add-style new-style-value prompt-for-style )))  
           
           
(defgeneric has-draw-style-p (draw-style style-key)
  (:documentation "Returns non-nil if draw-style has a value for style-key "))
  

(defgeneric draw-style (draw-style style-key &key &allow-other-keys)
  (:documentation "Returns value of draw-style  for style-key. ~
                  It is an error if draw-style does not have such a value"))

(defgeneric set-draw-style (draw-style style-key value &key &allow-other-keys) 
  (:documentation "Sets value of draw-style  for style-key. ~
                  It is an error if draw-style does not have such a value"))


(defgeneric add-style (draw-style style-name value)
  (:documentation "Adds a style style-name with value to draw-style"))

(defgeneric  copy-style-values (ds1  ds2)
  (:documentation "Copy style values from ds1 to ds2"))
                                
(defgeneric new-style-value (style-name new pair)
  (:documentation "Returns new value for style name. "))

(defgeneric prompt-for-style (style)
  (:documentation "Returns new value for style "))

;;;----------------------------------------------------------------------------------
(defun make-drawing-style (&rest keyword-pairs
                                 &key  &allow-other-keys)
  
  (loop 
        for kw in keyword-pairs by #'cddr
        for kw-val in (cdr keyword-pairs) by #'cddr 
        unless (assoc kw result)
        collect (cons kw kw-val) into result
        finally (return result)))

(defun drawing-style-p(arg) 
  (and (listp arg) (consp (car arg))))


(defun copy-drawing-style (ds)
  (copy-alist ds))

(defmethod copy-style-values (ds1  ds2)
  (loop for pair in ds1 do
        (set-draw-style ds2 (car pair) (cdr pair))))

(defmethod draw-style (ds style-name &key (default nil supplied-p))
  (let ((pair (assoc style-name ds)))
    (if pair (cdr pair)
        (if supplied-p
          default
        (quail-error "~S does not have drawing style ~S" ds style-name)))))

(defmethod has-draw-style-p (ds style-name)
  (assoc style-name ds))



(defmethod new-style-value (style-name new  pair)
  (declare (ignore pair style-name))
  new)

(defmethod new-style-value ((style-name (eql :size)) new pair)
  (unless (numberp new)
    (setq new 
          (case new
            (:larger (+  (cdr pair) (if (>= (cdr pair) 3) 2 1))) 
            (:smaller (-  (cdr pair) (if (<= (cdr pair) 3) 1 2)))
            (t (cdr pair)))))
  (max 1 new))

(defmethod new-style-value ((style-name (eql :font)) new pair)
  (if (wb:canvas-font-p new) new
      (new-font-size (cdr pair) new)))

(defmethod new-style-value ((style-name (eql :width)) new pair)
  (if (numberp new) 
    new
    (case new
      (:fatter (1+ (cdr pair)))
      (:thinner (1- (cdr pair)))
      (:prompt (wb:prompt-user :type 'number 
                                       :read-type :eval
                                       :prompt-string 
                                       (format nil "Change size from ~A" (cdr pair))))
      (t (cdr pair)))))





(defmethod new-style-value (style-name (new (eql :toggle)) pair) 
  (declare (ignore style-name))
  (not (cdr pair)))


(defmethod new-style-value ((style-name (eql :highlight?)) new pair)
  (declare (ignore style-name))
  (case new
          (:toggle (not (cdr pair)))
          (t new)))

(defmethod new-style-value ((style-name (eql :orientation)) new pair)
  (case new
    (:toggle (if (eq :horizontal (cdr pair))
               :vertical :horizontal))
    (t new)))

(defmethod new-style-value ((style-name (eql :color)) new pair)
  (declare (ignore pair))
  (if (eq new :prompt)
    (wb:prompt-user-for-color)
    new))

(defmethod prompt-for-style (style)
  :prompt)

(defmethod prompt-for-style ((style-name (eql :color)))
  (wb:prompt-user-for-color))

(defmethod prompt-for-style ((style-name (eql :font)))
  (wb:prompt-user
   :prompt-string "Enter the new font size (1 to 127): "
   :type 'integer
   :read-type :eval))




(defmethod set-draw-style (ds style-name val &key)
  (let ((pair (assoc style-name ds)))
    (if pair (setf (cdr pair) (new-style-value style-name val pair)))))


(defmethod add-style (ds style-name value)
  (if  (has-draw-style-p ds style-name)
    (set-draw-style ds style-name value)
    (nconc ds (cons (cons style-name value) nil))))
;;------------------------------------------------------------------------------------------
(defun find-pairs (key-pairs keys)
  (if keys
    (loop for k in key-pairs by #'cddr
          for val in (cdr key-pairs) by #'cddr
          when (member k keys) 
          collect k and collect val)))


(defun list-style-keys(ds)
  (loop for d in ds
        collect (car d)))

 
