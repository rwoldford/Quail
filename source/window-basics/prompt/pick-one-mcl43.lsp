;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               pick-one-mcl.lisp
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

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(pick-one)))

(defun pick-one (items &key
                       (prompt-text "Pick one of the following.")
                       (item-print-function NIL)
                       (action-function #'(lambda (i) i))
                       (select-text "Select")
                       (cancel-text "Cancel"))
  "Prompts the user to pick one of the items in the sequence items, ~
   using the text supplied by the keyword prompt-text. ~
   If supplied, the function action-function is called on the selected item.  ~
   Default action just returns the item.  ~
   If supplied, item-print-function will be used on each item to display it ~
   in the table.  ~
   (:see-also prompt-for-items prompt-t-or-f)"
  (let*
    ((p1 (make-instance 'pick-one-prompt 
                        :view-position (h-draw:make-point 10 10)
                        :window-show NIL
                        :window-type :shadow-edge-box
                        :items items
                        :item-print-function item-print-function
                        :item-action-function action-function
                        :prompt-text prompt-text
                        :select-text select-text
                        :cancel-text cancel-text))
    (ans (modal-dialog p1)))
    (let ((w (car (ccl::windows))))
      (if (typep w 'canvas)
        (mapcar  #'ccl:menu-enable (mapcar #'cdr (slot-value w 'title-menus)))))
    ans))


(defclass pick-one-prompt (ccl:dialog)
  ((items
    :initarg :items
    :initform NIL)
   (prompt-text
    :initform "Pick one of the following."
    :initarg :prompt-text)
   (selected-item)
   (item-action-function
    :initform #'(lambda (i) i)
    :initarg :item-action-function)
   )
  (:documentation "A dialog that allows the user to pick one of a sequence ~
                   of items.")
  )

(defmethod initialize-instance ((self pick-one-prompt)
                                &rest initargs)
  (apply #'call-next-method self :window-show NIL 
    :window-type :double-edge-box
    :view-position '(:top 100)
    initargs))

(defmethod initialize-instance :after ((self pick-one-prompt)
                                       &rest initargs
                                       &key (item-print-function NIL)
                                       (select-text "Select")
                                       (cancel-text "Cancel"))
  (declare (ignore initargs))
  (flet
    ((text-width (string font)
       (string-width string font)))
    (let*
      ((view-font (view-font self))
       (error-text "Please choose an item first!")
       (error-text-width (string-width error-text view-font))
       (prompt-text (slot-value self 'prompt-text))
       (prompt-text-width (string-width prompt-text view-font))
       (text-height (multiple-value-bind (ascent descent widmax leading)
                                         (font-info view-font)
                      (declare (ignore widmax))
                      (+ ascent descent leading)))
       (text-box-width  (+ 5 (max prompt-text-width error-text-width)))
       (text-box-height (round (* 2.5 text-height)))
       (button-height (+ 2 text-height))
       (button-width
        (+ 5 (max (string-width select-text view-font)
                  (string-width cancel-text view-font))))
       (seq-height 150)
       (seq-width 200)
       (text-dialog (make-dialog-item
                     'static-text-dialog-item
                     #@(10 20)
                     (h-draw:make-point text-box-width text-box-height)
                     prompt-text))
       (seq-dialog
        (if (functionp item-print-function)
          (make-dialog-item
           'sequence-dialog-item
           (h-draw:make-point 10 (+ 12 text-box-height))
           (h-draw:make-point seq-width seq-height)
           "Pick one"
           #'(lambda (d)
               (setf (slot-value (view-container d) 'selected-item)
                     (cell-contents d (first (selected-cells d)))))
           :dialog-item-enabled-p T
           :table-print-function item-print-function
           :table-vscrollp T
           :table-sequence (slot-value self 'items)
           )
          (make-dialog-item
           'sequence-dialog-item
           (h-draw:make-point 10 (+ 12 text-box-height))
           (h-draw:make-point seq-width seq-height)
           "Pick one"
           #'(lambda (d)
               (setf (slot-value (view-container d) 'selected-item)
                     (cell-contents d (first (selected-cells d)))))
           :dialog-item-enabled-p T
           :table-vscrollp T
           :table-sequence (slot-value self 'items)
           )))
       
       (select-button
        (make-dialog-item
         'default-button-dialog-item
         (h-draw:make-point (+ 20 seq-width) (+ 12 text-box-height))
         (h-draw:make-point button-width button-height)
         select-text
         (eval `(function
                 (lambda (d)
                   (let
                     ((p (view-container d)))
                     (if (slot-boundp p 'selected-item)
                       (return-from-modal-dialog
                        (funcall
                         (slot-value p 'item-action-function)
                         (slot-value p 'selected-item)))
                       (set-dialog-item-text ,text-dialog ,error-text))))))))
       
       (cancel-button
        (make-dialog-item
         'button-dialog-item
         (h-draw:make-point (+ 20 seq-width) (+ 10 (* 2 (+ 2 text-box-height))))
         (h-draw:make-point button-width button-height)
         cancel-text
         #'(lambda (d)
             (declare (ignore d))
             (return-from-modal-dialog (values)))))
       
       (self-view-size (view-size self))
       )
      (set-view-size self (h-draw:make-point (max (h-draw:point-x self-view-size)
                                                  (+ 20 seq-width button-width 2)
                                                  (+ 20 text-box-width 2))
                                             (max (h-draw:point-y self-view-size)
                                                  (+ 16 text-box-height seq-height)
                                                  (+ 14 (* 2 (+ 2 text-box-height))
                                                     button-height))))
      (add-subviews self text-dialog seq-dialog select-button cancel-button)
      )))
