;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               text-link.lisp
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


(eval-when (:compile-toplevel :load-toplevel :execute) (export '(text-link)))

(defmethod update-label-viewed-object ((self label) new)
  (declare (ignore new)))

(defmethod text-link((self view) label &optional position)
  (declare (ignore position label)))

(defmethod text-unlink((self view) label)
  (declare (ignore label)))

(defmethod text-link((self d-view) (label label) &optional position)
  
  (setq position
        (cond 
         ((numberp position) position)
         ((eq position :x) 0)
         ((eq position :y) 1)
         ((eq position :z) 2)
         (t nil)))
  (if position
    (setf (viewed-object-of label) (extract-variate (dataset-of self)
                                                    (nth position (vars-of self)))))
  
  (if (null (text-links-of self))
    (setf (text-links-of self)
          (make-list (length (coord-strings  self)))))
  (if (and (numberp position)
           (< position (length (text-links-of self))))
    (push label (elt (text-links-of self) position))
   ;; (loop for position from 0 below (length (text-links-of self)) do
          (do ((position 0 (incf position)))
        ((= position (length (text-links-of self))))
      (push label (elt (text-links-of self) position)))))




(defmethod text-unlink((self d-view) label)
  
  (setf (text-links-of self) 
        (loop for l in (text-links-of self)
              collect (delete label l))))


(defmethod update-text-link((self d-view)  &optional position)
  (setq position
        (cond 
         ((numberp position) position)
          ((eq position :x) 0)
         ((eq position :y) 1)
         ((eq position :z) 2)
         (t 0)))
  

  (loop with string = (elt (coord-strings self) position)
        with v = (nth position (vars-of self))
        for l in (elt (text-links-of self) position) do
        (setf (viewed-object-of l) (extract-variate (dataset-of self)
                                                    v))
        
        (if (stringp (text-of l))
        (unless (string-equal string (get-text l))
           (set-text l string)
           (update-text-links l))
        (draw-view l :erase? t))))
            



(defmethod update-text-links((self d-view))
  
  
  (loop for  string in (coord-strings self)
        for labels in (text-links-of self) do
        (loop for l in labels
              do 
              (if (stringp (text-of l))
                (unless (string-equal string (get-text l))
                  (set-text l string)
                  (update-text-links l))
                (draw-view l :erase? t)))))


(defmethod change-variable :after ((self d-view) &key)
  (update-text-links self))


(defmethod change-case-status  :after ((self d-view) cases status  &rest args)
  (declare (ignore cases status args))
  (update-text-links self))
