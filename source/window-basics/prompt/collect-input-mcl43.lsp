;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               collect-input-mcl.lisp
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

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(collect-input)))

(defun collect-input (prompt-default-pairs
                      &key
                      (prompt-text "Enter a value for each field.")
                      (select-text "OK")
                      (cancel-text "Cancel")
                      (columns 1))
  "Prompts user for value of several items as given by the ~
   argument prompt-default-pairs.  Prompt-text, select-text, and cancel-text ~
   are strings which will appear in the message, select button, and cancel ~
   button, respectively.   Columns is the number of columns to be used in the ~
   display (default 1). ~
   (:returns An updated list of prompt-value-pairs.  Values are strings or NIL ~
   (if no value was specified))."
  (setq prompt-default-pairs
        (loop
          for pair in prompt-default-pairs
          collect
          (cond
           ((consp pair) pair)
           (T (cons pair NIL)))))
  (let*
    ((p1 (make-instance 'collect-input
           ;;:view-position NIL
           :window-show NIL
           :items prompt-default-pairs
           :prompt-text (format NIL prompt-text)
           :select-text select-text
           :cancel-text cancel-text
           :columns columns))
     (ans (modal-dialog p1)))
    (let ((w (car (ccl::windows))))
      (if (typep w 'canvas)
        (mapcar  #'ccl:menu-enable (mapcar #'cdr (slot-value w 'title-menus)))))
    ans))

(defclass collect-input (ccl:dialog)
  ((item-pairs
    :initarg :items
    :initform NIL)
   (prompt-text
    :initform "Set parameters."
    :initarg :prompt-text)
   (collect-input :initform NIL)
   (columns :initform 1 :initarg :columns)
   )
  (:documentation "A dialog that allows the user to set ~
                   the logical value of several parameters at once.")
  )

(defmethod initialize-instance ((self collect-input) &rest initargs)
  (apply #'call-next-method self :window-show NIL
         :window-type :double-edge-box
         :view-position '(:left 2)
         initargs))

(defmethod initialize-instance :after ((self collect-input)
                                       &rest initargs
                                       &key 
                                       (select-text "OK")
                                       (cancel-text "Cancel"))
  (declare (ignore initargs))
  (flet
    ((text-width (string font)
       (string-width string font)))
    (let*
      ((item-pairs (slot-value self 'item-pairs))
       (columns (slot-value self 'columns))
       (view-font (view-font self))
       (max-prompt-text-width
        (loop
          for i in item-pairs
          maximize
          (string-width
           (string (car i))
           view-font)))
       (max-input-text-width
        (loop
          for i in item-pairs
          maximize
          (string-width
           (string (cdr i))
           view-font)))
       (max-prompt-width (+ 10 max-prompt-text-width))
       (max-input-width (max 100  (+ 20 max-input-text-width)))
       (prompt-text (slot-value self 'prompt-text))
       (prompt-text-width (string-width prompt-text view-font))
       (text-height (multiple-value-bind (ascent descent widmax leading)
                                         (font-info view-font)
                      (declare (ignore widmax))
                      (+ ascent descent leading)))
       (text-box-x 20)
       (text-box-y 20)
       (text-box-width  (+ 5 prompt-text-width))
       (text-box-height (round (* 2.5 text-height)))
       (collect-input-height (+  5 text-height))
       (row-sep 10)
       (col-sep 30)
       (button-height (+ 2 text-height))
       (button-width
        (+ 5 (max (string-width select-text view-font)
                  (string-width cancel-text view-font))))
       (select-button-y (+ text-box-y 2 text-box-height))
       (cancel-button-y (+ select-button-y button-height 10))

       (n-items (length item-pairs))
       (inter-field-space 3)
       (col-width (+ col-sep max-prompt-width
                     max-input-width inter-field-space))
       (nrows (ceiling (/ n-items columns)))
       (first-item-y (+ text-box-y text-box-height))
       (first-item-x 10)
       (button-x (+ 50 first-item-x (* columns col-width)))
       (view-size-width (max 200
                             (+ button-x button-width 2)
                             (+ text-box-x text-box-width 2)))
       (view-size-height (max 100
                              (+ first-item-y
                                 (+ 2 (* (+ nrows 1)
                                         (+ row-sep collect-input-height)))
                                 2)
                              (+ cancel-button-y 
                                 button-height
                                 2)))
       (text-dialog (make-dialog-item
                     'static-text-dialog-item
                     (h-draw:make-point text-box-x text-box-y)
                     (h-draw:make-point text-box-width text-box-height)
                     prompt-text))
       
       (collect-prompts
        (loop
                for item in item-pairs
                with y-start = first-item-y
                with x-start = first-item-x
                with row = 0
                with col = 0
                collect
                (make-dialog-item
                 'static-text-dialog-item
                 (h-draw:make-point (+ x-start (* col col-width))
                             (+ y-start
                                (* row
                                   (+ row-sep collect-input-height))))
                 (h-draw:make-point max-prompt-width collect-input-height)
                 (string (car item))
                 #'(lambda (d) d)
                 :dialog-item-enabled-p T)
                do (cond
                    ((>= row (1- nrows))
                     (incf col 1)
                     (setq row 0))
                    (T (incf row 1)))))
       
       (collect-input
        (setf (slot-value self 'collect-input)
              (loop
                for item in item-pairs
                with y-start = first-item-y
                with x-start = first-item-x
                with row = 0
                with col = 0
                collect
                (make-dialog-item
                 'editable-text-dialog-item
                 (h-draw:make-point (+ x-start max-prompt-width 
                                inter-field-space (* col col-width))
                             (+ y-start (* row 
                                           (+ row-sep collect-input-height))))
                 (h-draw:make-point max-input-width collect-input-height)
                 (string (or (cdr item) ""))
                 #'(lambda (d) d)
                 :dialog-item-enabled-p T
                 :allow-returns T)
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
                (collect-input (slot-value p 'collect-input)))
               (loop for i in items
                     as ci in collect-input
                     with result
                     do
                     (setf result (dialog-item-text ci))
                     (if (zerop (length result))
                       (setf (cdr i) NIL)
                       (setf (cdr i) (dialog-item-text ci))))
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
      
      (set-view-size self (h-draw:make-point (+ 6 view-size-width) view-size-height))
      (add-subviews self text-dialog select-button cancel-button)
      (loop for ci in collect-input
            do
            (add-subviews self ci))
      (loop for ci in collect-prompts
            do
            (add-subviews self ci))
      )))

#|
(setf items
      (collect-input (list 'heycheckmeOut!! "try this" "Or this" "or this"
                                       "Gold" "Silver" "Bronze")))
(setf items (collect-input items))
(collect-input (list 'heycheckmeOut!! "try this" "Or this" "or this"
                                       "Gold" "Silver" "Bronze")
             :columns 2)
(collect-input (list 'heycheckmeOut!! "try this" "Or this" "or this"
                                       "Gold" "Silver" "Bronze")
             :columns 3)
(collect-input (list 'heycheckmeOut!! "try this" "Or this" "or this"
                                       "Gold" "Silver" "Bronze"
                                       "platinum" "Tin" "lead" "zinc"
                                       "iron")
             :prompt-text "Give the man a medal!"
             :select-text "DO IT"
             :cancel-text "Forget it." :columns 3)
|#
