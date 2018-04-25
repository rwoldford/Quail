;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               check-items-mcl.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;
;;;  This file is part of the Window-Basics package intended to provide a uniform
;;;  window environment for Common Lisp programming.
;;;  
;;;  For copyright information and history, see window-basics-copyright.lisp
;;;
;;;  Authors:
;;;     R.W. Oldford 1992
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------
;;;
;;;  History:
;;;
;;;  - Construct a uniform prompting facility.
;;;

(in-package :wb)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(check-items)))

(defun check-items (item-default-pairs
                    &key
                    (prompt-text "Set parameters.")
                    (select-text "OK")
                    (cancel-text "Cancel")
                    (columns 1)
                    )
  "Prompts user for value (T or NIL) of several items as given by the ~
   argument item-default-pairs.  Prompt-text, select-text, and cancel-text ~
   are strings which will appear in the message, select button, and cancel ~
   button, respectively.   Columns is the number of columns to be used in the ~
   display (default 1)."
  (setq item-default-pairs
        (loop
          for pair in item-default-pairs
          collect
          (cond
           ((consp pair) pair)
           (T (cons pair NIL)))))
               
  (let
    ((p1 (make-instance 'check-items 
                        :window-show NIL
                        :items item-default-pairs
                        :prompt-text prompt-text
                        :select-text select-text
                        :cancel-text cancel-text
                        :columns columns)))
    (modal-dialog p1)))

(defclass check-items (ccl:dialog)
  ((item-pairs
    :initarg :items
    :initform NIL)
   (prompt-text
    :initform "Set parameters."
    :initarg :prompt-text)
   (check-items :initform NIL)
   (columns :initform 1 :initarg :columns)
   )
  (:documentation "A dialog that allows the user to set ~
                   the logical value of several parameters at once.")
  )

(defmethod initialize-instance ((self check-items) &rest initargs)
  (apply #'call-next-method self 
         :window-show NIL
         :window-type :double-edge-box
         :view-position '(:top 100)
         initargs))

(defmethod initialize-instance :after ((self check-items)
                                       &rest initargs
                                       &key 
                                       (select-text "OK")
                                       (cancel-text "Cancel"))
  (declare (ignore initargs))
  (let*
    ((item-pairs (slot-value self 'item-pairs))
     (columns (slot-value self 'columns))
     (view-font (view-font self))
     (max-item-text-width
      (loop for i in item-pairs maximize
            (string-width (string (car i)) view-font)))
     (max-item-width (+ 10 max-item-text-width))
     (prompt-text (slot-value self 'prompt-text))
     (prompt-text-width (string-width prompt-text view-font))
     (text-height (multiple-value-bind (ascent descent widmax leading)
                                       (font-info view-font)
                    (declare (ignore widmax))
                    (+ ascent descent leading)))
     (text-box-x 10)
     (text-box-y 10)
     (text-box-width  (+ 5 prompt-text-width))
     (text-box-height (round (* 2 text-height)))
     (check-box-height (+  5 text-height))
     (button-height (+ 2 text-height))
     (button-width
      (+ 5 (max (string-width select-text view-font)
                (string-width cancel-text view-font))))
     (select-button-y (+ text-box-y 2 text-box-height))
     (cancel-button-y (+ select-button-y button-height 10))
     (n-items (length item-pairs))
     (col-width (+ 10 max-item-width))
     (nrows (ceiling (/ n-items columns)))
     (first-item-y (+ text-box-y text-box-height))
     (first-item-x 10)
     (button-x (+ 10 first-item-x (* columns col-width)))
     (view-size-width (max (+ button-x button-width 6)
                           (+ text-box-x text-box-width 6)))
     (view-size-height (max (+ first-item-y
                               (* nrows check-box-height)
                               4)
                            (+ cancel-button-y 
                               button-height
                               4)))
     (text-dialog (make-dialog-item
                   'static-text-dialog-item
                   (h-draw:make-point text-box-x text-box-y)
                   (h-draw:make-point text-box-width text-box-height)
                   prompt-text))
     (check-items
      (setf (slot-value self 'check-items)
            (loop
              for item in item-pairs
              with y-start = first-item-y
              with x-start = first-item-x
              with row = 0
              with col = 0
              collect
              (make-dialog-item
               'check-box-dialog-item
               (h-draw:make-point (+ x-start (* col col-width))
                                  (+ y-start (* row check-box-height)))
               (h-draw:make-point max-item-width check-box-height)
               (string (car item))
               #'(lambda (d) d)
               :dialog-item-enabled-p T
               :check-box-checked-p (cdr item))
              do (cond
                  ((>= row (1- nrows))
                   (incf col 1)
                   (setq row 0))
                  (T (incf row 1))))))
     (select-button
      (make-dialog-item
       'default-button-dialog-item
       (h-draw:make-point button-x select-button-y)
       (h-draw:make-point button-width button-height)
       select-text
       #'(lambda (d)
           (let*
             ((p (view-container d))
              (items (slot-value p 'item-pairs))
              (check-items (slot-value p 'check-items)))
             (loop for i in items
                   as ci in check-items
                   do
                   (setf (cdr i)
                         (check-box-checked-p ci)))
             (return-from-modal-dialog items)
             ))))
     (cancel-button
      (make-dialog-item
       'button-dialog-item
       (h-draw:make-point button-x cancel-button-y)
       (h-draw:make-point button-width button-height)
       cancel-text
       #'(lambda (d)
           (declare (ignore d))
           (return-from-modal-dialog (values)))))
     )
    (set-view-size self (h-draw:make-point view-size-width view-size-height))
    (add-subviews self text-dialog select-button cancel-button)
    (loop for ci in check-items
          do
          (add-subviews self ci))
    ))

#|
(setf items
      (check-items (list 'checkItOut!! "try this" "Or this" "or this"
                                       "Gold" "Silver" "Bronze")))
(setf items (check-items items))
(check-items (list 'checkItOut!! "try this" "Or this" "or this"
                                       "Gold" "Silver" "Bronze")
             :columns 2)
(check-items (list 'checkItOut!! "try this" "Or this" "or this"
                                       "Gold" "Silver" "Bronze")
             :columns 3)
(check-items (list 'checkItOut!! "try this" "Or this" "or this"
                                       "Gold" "Silver" "Bronze"
                                       "platinum" "Tin" "lead" "zinc"
                                       "iron")
             :prompt-text "Give the man a medal!"
             :select-text "DO IT"
             :cancel-text "Forget it." :columns 3)
|#
