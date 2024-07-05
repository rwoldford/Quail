;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               numerical-label.lisp
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
;;;     R.W. Oldford 1997
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :views)

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(numerical-label set-numerical-format)))

(defclass numerical-label (label)
  ((number  
    :initarg :number
    :reader numerical-value-of 
    :initform nil
    :documentation "Number being displayed as text.")
   (format  
    :initarg :format
    :reader format-of 
    :initform "~s"
    :documentation "Format string to be used to create text from number.")
   (style-keys :initform '(:font :color) :allocation :class)
   (middle-menu :allocation :class :initform nil))
  (:documentation "A linkable label to display numerical values.")
  (:default-initargs :font *default-label-font* :linkable? t
    :justification :right :orientation :horizontal))



(defmethod initialize-instance :after ((self numerical-label)
                                       &rest initargs &key (format NIL format?))
  (declare (ignore initargs))
  (cond
   ((eq format :prompt)
    (set-numerical-format self NIL :draw? NIL))
   ((and format? (null format))
    (setf (format-of self) "~s"))
   )
  (unless (viewed-object-of self)
    (setf (viewed-object-of self) (numerical-value-of self)))
  (setf (text-of self)
        (format NIL (format-of self) (numerical-value-of self)))
  )

(defmethod (setf numerical-value-of) (new-value (self numerical-label))
  
  (when (eq new-value :prompt)
    (setq new-value (wb:prompt-user :result-type 'number
                                    :prompt-string "Enter numerical value")))
  (let ((drawn? (viewports-of self)))
    (when (numberp new-value)
      (if drawn? (erase-view self))
      (if (and (numberp (viewed-object-of self))
               (= (viewed-object-of self) (numerical-value-of self)))
        (setf (viewed-object-of self) new-value))
      (setf (slot-value self 'number) new-value)
      (setf (text-of self) (format NIL (format-of self) new-value))
      (if drawn? (draw-view self))
      )
    ))

(defmethod (setf format-of) (new-value (self numerical-label))
  
  (let ((drawn? (viewports-of self)))
    (cond
     ((eq new-value :prompt) 
      (set-numerical-format self NIL :draw? drawn?))
     ((stringp new-value)
      (if drawn? (erase-view self))
      (setf (text-of self) (format NIL new-value (numerical-value-of self)))
      (setf (slot-value self 'format) new-value)
      (if drawn? (draw-view self))
      )
     ((wb::prompt-t-or-f
       (format NIL "New value (~s) for format is not a string. ~& 
                    Would you like to construct one now?" new-value)) ;;; spce added after ~&  12MAR2022  gwb
      (set-numerical-format self NIL :draw? drawn?)
      ))))


(defmethod get-menu-items ((self numerical-label) (slot-name (eql 'middle-menu)))
  
  `(("Numerical format"  (set-numerical-format nil :viewport))
    ))

(defmethod draw-view :before ((self numerical-label) 
                              &key number text viewport)
  (declare (ignore number text viewport))
  
  )
(defmethod set-numerical-format ((self numerical-label)  format
                                 &key viewport (draw? t))
  (unless format
    (let* ((pos (prompt-position self viewport))
           (format-types '("Lisp format directive"
                           "Fixed-format floating point"
                           "Exponential floating point"
                           "Dollars floating point"
                           "General floating point"
                           "Decimal"
                           "Binary"
                           "Octal"
                           "Hexadecimal"
                           "Radix"
                           ))
           (format-type (wb:pick-one format-types))
           )
      (cond
       ((string-equal format-type "Lisp format directive")
        (setf format
              (wb:prompt-user :result-type 'string :read-type :eval
                              :prompt-string 
                              (format nil "Format directive string:")
                              :left (2d-position-x pos)
                              :top  (2d-position-y pos))))
       ((string-equal format-type "Fixed-format floating point")
        (setf format "~")
        (setf format-type
              (wb:collect-input
               '(("Width in characters" )
                 ("Digits after decimal place" )
                 ("Scale factor" )
                 ("Overflow character" )
                 ("Pad character" )
                 )
               :prompt-text (format NIL
                                    "Fixed-format floating point. ~%
                                     Fill in values as appropriate:")
               ))
        (loop for pair in format-type
              do
              (setf format
                    (if (cdr pair)
                      (cond
                       ((string-equal (car pair) "Overflow character")
                        (concatenate 'string
                                     format
                                     (format NIL "'~a" (cdr pair))
                                     ",")
                        )
                       ((string-equal (car pair) "Pad character")
                        (concatenate 'string
                                     format
                                     (format NIL "'~a" (cdr pair))
                                     ))
                       (T
                        (concatenate 'string
                                     format
                                     (format NIL "~a" (cdr pair))
                                     ",")))
                      (concatenate 'string
                                   format
                                   ","))))
        (setf format (concatenate 'string format "F"))
        )
       ((string-equal format-type "Exponential floating point")
        (setf format "~")
        (setf format-type
              (wb:collect-input
               '(("Width in characters" )
                 ("Digits after decimal place" )
                 ("Digits for exponent" )
                 ("Scale factor" )
                 ("Overflow character" )
                 ("Pad character" )
                 ("Exponent character" )
                 )
               :prompt-text (format NIL
                                    "Exponential floating point. ~%
                                     Fill in values as appropriate:")
               ))
        (loop for pair in format-type
              do
              (setf format
                    (if (cdr pair)
                      (cond
                       ((or (string-equal (car pair) "Overflow character")
                            (string-equal (car pair) "Pad character")
                            )
                        (concatenate 'string
                                     format
                                     (format NIL "'~a" (cdr pair))
                                     ",")
                        )
                       ((string-equal (car pair) "Exponent character")
                        (concatenate 'string
                                     format
                                     (format NIL "'~a" (cdr pair))
                                     ))
                       (T
                        (concatenate 'string
                                     format
                                     (format NIL "~a" (cdr pair))
                                     ",")))
                      (concatenate 'string
                                   format
                                   ","))))
        (setf format (concatenate 'string format "E"))
        )
       ((string-equal format-type "Decimal")
        (setf format "~")
        (setf format-type
              (wb:collect-input
               '(("Width in columns" )
                 ("Pad character" )
                 ("Comma character" )
                 ("Interval between commas" )
                 )
               :prompt-text (format NIL
                                    "Decimal. ~%
                                     Fill in values as appropriate:")
               ))
        (loop for pair in format-type
              do
              (setf format
                    (if (cdr pair)
                      (cond
                       ((or (string-equal (car pair) "Pad character")
                            (string-equal (car pair) "Comma character"))
                        (concatenate 'string
                                     format
                                     (format NIL "'~a" (cdr pair))
                                     ","))
                       ((string-equal (car pair) "Interval between commas")
                        (concatenate 'string
                                     format
                                     (format NIL "~a" (cdr pair))
                                     ))
                       (T (concatenate 'string
                                       format
                                       (format NIL "~a" (cdr pair))
                                       ","))
                       )
                      (concatenate 'string
                                   format
                                   ","))))
        (setf format (concatenate 'string format "D"))
        )
       ((string-equal format-type "Binary")
        (setf format "~")
        (setf format-type
              (wb:collect-input
               '(("Width in columns" )
                 ("Pad character" )
                 ("Comma character" )
                 ("Interval between commas" )
                 )
               :prompt-text (format NIL
                                    "Binary. ~%
                                     Fill in values as appropriate:")
               ))
        (loop for pair in format-type
              do
              (setf format
                    (if (cdr pair)
                      (cond
                       ((or (string-equal (car pair) "Pad character")
                            (string-equal (car pair) "Comma character"))
                        (concatenate 'string
                                     format
                                     (format NIL "'~a" (cdr pair))
                                     ","))
                       ((string-equal (car pair) "Interval between commas")
                        (concatenate 'string
                                     format
                                     (format NIL "~a" (cdr pair))
                                     ))
                       (T (concatenate 'string
                                       format
                                       (format NIL "~a" (cdr pair))
                                       ","))
                       )
                      (concatenate 'string
                                   format
                                   ","))))
        (setf format (concatenate 'string format "B"))
        )
       ((string-equal format-type "Octal")
        (setf format "~")
        (setf format-type
              (wb:collect-input
               '(("Width in columns" )
                 ("Pad character" )
                 ("Comma character" )
                 ("Interval between commas" )
                 )
               :prompt-text (format NIL
                                    "Octal. ~%
                                     Fill in values as appropriate:")
               ))
        (loop for pair in format-type
              do
              (setf format
                    (if (cdr pair)
                      (cond
                       ((or (string-equal (car pair) "Pad character")
                            (string-equal (car pair) "Comma character"))
                        (concatenate 'string
                                     format
                                     (format NIL "'~a" (cdr pair))
                                     ","))
                       ((string-equal (car pair) "Interval between commas")
                        (concatenate 'string
                                     format
                                     (format NIL "~a" (cdr pair))
                                     ))
                       (T (concatenate 'string
                                       format
                                       (format NIL "~a" (cdr pair))
                                       ","))
                       )
                      (concatenate 'string
                                   format
                                   ","))))
        (setf format (concatenate 'string format "O"))
        )
       ((string-equal format-type "Hexadecimal")
        (setf format "~")
        (setf format-type
              (wb:collect-input
               '(("Width in columns" )
                 ("Pad character" )
                 ("Comma character" )
                 ("Interval between commas" )
                 )
               :prompt-text (format NIL
                                    "Hexadecimal. ~%
                                     Fill in values as appropriate:")
               ))
        (loop for pair in format-type
              do
              (setf format
                    (if (cdr pair)
                      (cond
                       ((or (string-equal (car pair) "Pad character")
                            (string-equal (car pair) "Comma character"))
                        (concatenate 'string
                                     format
                                     (format NIL "'~a" (cdr pair))
                                     ","))
                       ((string-equal (car pair) "Interval between commas")
                        (concatenate 'string
                                     format
                                     (format NIL "~a" (cdr pair))
                                     ))
                       (T (concatenate 'string
                                       format
                                       (format NIL "~a" (cdr pair))
                                       ","))
                       )
                      (concatenate 'string
                                   format
                                   ","))))
        (setf format (concatenate 'string format "X"))
        )
       
       ((string-equal format-type "Radix")
        (setf format "~")
        (setf format-type
              (wb:collect-input
               '(("Radix" )
                 ("Width in columns" )
                 ("Pad character" )
                 ("Comma character" )
                 ("Interval between commas" )
                 )
               :prompt-text (format NIL
                                    "Radix. ~%
                                     Fill in values as appropriate:")
               ))
        (loop for pair in format-type
              do
              (setf format
                    (if (cdr pair)
                      (cond
                       ((or (string-equal (car pair) "Pad character")
                            (string-equal (car pair) "Comma character"))
                        (concatenate 'string
                                     format
                                     (format NIL "'~a" (cdr pair))
                                     ","))
                       ((string-equal (car pair) "Interval between commas")
                        (concatenate 'string
                                     format
                                     (format NIL "~a" (cdr pair))
                                     ))
                       (T (concatenate 'string
                                       format
                                       (format NIL "~a" (cdr pair))
                                       ","))
                       )
                      (concatenate 'string
                                   format
                                   ","))))
        (setf format (concatenate 'string format "R"))
        )
       
       ((string-equal format-type "Dollars floating point")
        (setf format "~")
        (setf format-type
              (wb:collect-input
               '(("Digits after decimal place" )
                 ("Minimum number of digits before decimal")
                 ("Minimum total width" )
                 ("Pad character" )
                 )
               :prompt-text (format NIL
                                    "Dollars floating point. ~%
                                     Fill in values as appropriate:")
               ))
        (loop for pair in format-type
              do
              (setf format
                    (if (cdr pair)
                      (if (string-equal (car pair) "Pad character")
                        (concatenate 'string
                                     format
                                     (format NIL "'~a" (cdr pair))
                                     )
                        (concatenate 'string
                                     format
                                     (format NIL "~a" (cdr pair))
                                     ","))
                      (concatenate 'string
                                   format
                                   ","))))
        (setf format (concatenate 'string format "$"))
        )
       ((string-equal format-type "General floating point")
        (setf format "~")
        (setf format-type
              (wb:collect-input
               '(("Width in characters" )
                 ("Digits after decimal place" )
                 ("Digits for exponent" )
                 ("Scale factor" )
                 ("Overflow character" )
                 ("Pad character" )
                 ("Exponent character" )
                 )
               :prompt-text (format NIL
                                    "General floating point. ~%
                                     Fill in values as appropriate:")
               ))
        (loop for pair in format-type
              do
              (setf format
                    (if (cdr pair)
                      (cond
                       ((or (string-equal (car pair) "Overflow character")
                            (string-equal (car pair) "Pad character")
                            )
                        (concatenate 'string
                                     format
                                     (format NIL "'~a" (cdr pair))
                                     ",")
                        )
                       ((string-equal (car pair) "Exponent character")
                        (concatenate 'string
                                     format
                                     (format NIL "'~a" (cdr pair))
                                     ))
                       (T
                        (concatenate 'string
                                     format
                                     (format NIL "~a" (cdr pair))
                                     ",")))
                      (concatenate 'string
                                   format
                                   ","))))
        (setf format (concatenate 'string format "G"))
        )
       (T
        (setf format
              (wb:prompt-user :result-type 'string :read-type :eval
                              :prompt-string 
                              (format nil "Format directive string:")
                              :left (2d-position-x pos)
                              :top  (2d-position-y pos))))
       )
      )
    )
  (setf (format-of self) format)
  (if draw?
    (draw-view self :erase? t)))
