;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               axis.lisp
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

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(axis ntics-of set-tic-format axis basic-axis
          tic-list-of 
          ;;(setf tic-list-of) 
          default-axis-settings set-tics set-ntics set-extent)))
;;;----------------------------------------------------------------------------------

(defclass basic-axis (tic-mixin justification-mixin  orientation-mixin simple-view )
  
  ((style-keys :initform '(:tics? :tic-labels? :font :color :tic-ruler?) :allocation :class)
   (tic-length :initform nil :initarg :tic-length :accessor tic-length-of)
   (middle-menu :allocation :class :initform nil))
  (:default-initargs 
    :tic-ruler? t :color *default-axis-color*
                     :tics? t :tic-labels? t
                     :justification :default :initform-fn nil
                     :orientation :horizontal 
                      :font wb:*very-small-graphics-font*))

(defclass axis ( basic-axis 1d-view)
  
  ((middle-menu :allocation :class :initform nil))
  (:default-initargs 
    :initform-fn nil))
#|
(defclass axis (tic-mixin justification-mixin 1d-view simple-view )
  
  ((style-keys :initform '(:tics? :tic-labels? :font :color :tic-ruler?) :allocation :class)
   (tic-length :initform nil :initarg :tic-length :accessor tic-length-of)
   (middle-menu :allocation :class :initform nil))
  (:default-initargs 
    :tic-ruler? t 
                     :tics? t :tic-labels? t
                     :justification :default :initform-fn nil
                     :orientation :horizontal 
                      :font wb:*very-small-graphics-font*))
|#

(defgeneric default-axis-settings (axis)
  (:documentation "Gives axis default number of tics") )

(defgeneric set-extent (axis min max  &key viewport &allow-other-keys)
  (:documentation "Sets the min and max of axis.~
                  If min or max is nil the user is prompted."))

(defgeneric set-tics (axis tics &key  viewport &allow-other-keys)
  (:documentation "Changes to tics.~
                  If tics is nil the user is prompted for a list."))

(defgeneric set-ntics (axis ntics &key  viewport &allow-other-keys)
  (:documentation "Changes to number of tics ntics.~
                  If ntics is nil the user is prompted."))

(defgeneric set-tic-format (axis  format &key viewport &allow-other-keys)
  (:documentation "Changes tic format.~
                   If format is nil the user is prompted. ~
                   Format must be a string that is a legal Common Lisp format ~
                   for numbers."))

;;;----------------------------------------------------------------------------------

(defmethod initialize-instance :after  ((self basic-axis) &key orientation justification) 
  (if (eq justification :default)
    (if (eq orientation :horizontal)
      (setf (justification-of self) :bottom)
      (setf (justification-of self) :right))))
      

(defmethod draw-view ((self basic-axis) &key viewport)
  (let* ((orient (orientation-of self))
         (just (justification-of self))
         (draw-just (if (eq orient :vertical)
                      (if (eq just :right) :right :left)
                      (if (eq just :top) :top :bottom))))
    (with-exposed-viewports self viewport vp
      (draw-axis self draw-just vp))))


(defmethod erase-view ((self basic-axis) 
                       &key viewport)
  
  ;; erases the axis in VIEWPORT, if supplied, else erases  in all exposed
  ;; viewports
  ;; axes have labels outside of bounds-- erase a bigger region
  
  
  (with-exposed-viewports self viewport vp
    (let* ((w (window-of vp))
           rl rb rw rh)
      (when vp
        (if (eq  (orientation-of self) :horizontal)
          (setq rl (- (left-of vp) 4)
                rb (1- (bottom-of vp))
                rw (+ 9 (width-of vp))
                rh (+ 3 (height-of vp)))
          (setq rl (- (left-of vp) 1)
                rb (- (bottom-of vp) 4)
                rw (+ 3 (width-of vp))
                rh (+ 9 (height-of vp))))
        (wb:canvas-clear  w
                          :canvas-left rl :canvas-bottom rb
                          :width rw :height rh)))))

(defun draw-horizontal-axis (tic-list tic-line tic-start tic-end 
                                      label-posn font  tic-labels? window)
 (if tic-line  (wb:canvas-draw-line window (caar tic-list) tic-line 
                      (caar (last tic-list)) tic-line))
  (loop  for t-l in tic-list
         for tic-posn-x = (car t-l)
         for tic-label = (cdr t-l) do
         (wb:canvas-draw-line window tic-posn-x  tic-start 
                              tic-posn-x tic-end)
         (wb:canvas-move-to
          window
          (- tic-posn-x 
             (truncate (wb:canvas-string-width window tic-label :font font) 2)
             )
          label-posn)
         (if tic-labels?
           (wb:with-canvas-font window font
             (wb:canvas-draw-string window tic-label)))))



(defun draw-vertical-axis (tic-list tic-line tic-start tic-end  
                                    label-posn font tic-labels? window)
  (if tic-line (wb:canvas-draw-line window tic-line (caar tic-list ) 
                     tic-line (caar (last tic-list))))
  (loop for t-l in tic-list
        for tic-posn-y = (car t-l)
        for tic-label = (cdr t-l) do
        (wb:canvas-draw-line window tic-start  tic-posn-y 
                             tic-end tic-posn-y)
        (wb:canvas-move-to
         window label-posn
         (- tic-posn-y 
            (truncate (wb:canvas-font-ascent window :font font) 2))
         )
        (if tic-labels?
          (wb:with-canvas-font window font
            (wb:canvas-draw-string window tic-label)))))

(defmethod tic-list-for-viewport ((self basic-axis)  vp )
  (let* ((orientation (orientation-of self))
         (tic-list
          (normalize-tic-list (tic-list-of self)
                              (tic-format-of self)
                              orientation
                              (justification-of self)
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

(defmethod draw-axis ((self basic-axis) (position (eql :top)) vp)
  (let ((bw (window-of vp)))
    (wb:with-pen-values bw (draw-style self :color) 1 nil
      (multiple-value-bind (xmin xmax ymin ymax) (bounds-of vp)
        (declare (ignore ymin))
        (if (draw-style self :tics?)
          (let* ((font (draw-style self :font))
                 (tic-list (tic-list-for-viewport self vp)) 
                 (font-ascent (wb:canvas-font-ascent bw :font font))
                 (font-height (wb:canvas-font-height bw :font font))
                 tic-len
                 tic-elen
                 
                 tic-end  label-posn)
            (if (eq (tic-length-of self) :viewport)
              (setf tic-len (height-of vp)
                    tic-elen 0)
              (setf tic-len (or (first (tic-length-of self))
                              (ceiling (* 0.7 font-ascent)))
                    tic-elen (or (second (tic-length-of self)) 0)))
            
            (setf tic-end  (- ymax tic-len )
                    label-posn (- tic-end font-height))
            
            (draw-horizontal-axis tic-list (and (draw-style self :tic-ruler?) ymax) (+ ymax tic-elen) tic-end label-posn font
                                  (draw-style self :tic-labels?) bw))
         (if (draw-style self :tic-ruler?)
          
           (wb:canvas-draw-line bw xmin ymax xmax ymax)))))))


(defmethod draw-axis ((self basic-axis) position vp)
  (declare (ignore position))
  (let ((bw (window-of vp)))
    (wb:with-pen-values bw (draw-style self :color) 1 nil
      (multiple-value-bind (xmin xmax ymin ymax) (bounds-of vp)
        (declare (ignore ymax))
        (if (draw-style self :tics?)
          (let* ((font (draw-style self :font))
                 (tic-list (tic-list-for-viewport self vp)) 
                 (font-ascent (wb:canvas-font-ascent bw :font font))
                 (font-descent (wb:canvas-font-descent bw :font font))
                 tic-len
                 tic-elen
                 
                 tic-end  label-posn)
            (if (eq (tic-length-of self) :viewport)
              (setf tic-len (height-of vp)
                    tic-elen 0)
              (setf tic-len (or (first (tic-length-of self))
                              (ceiling (* 0.7 font-ascent)))
                    tic-elen (or (second (tic-length-of self)) 0)))
            (setf tic-end  (+ ymin tic-len )
                    label-posn (+ tic-end font-descent))
            (draw-horizontal-axis tic-list (and (draw-style self :tic-ruler?) ymin) (- ymin tic-elen) tic-end label-posn 
                                  font  (draw-style self :tic-labels?) bw))
          (if (draw-style self :tic-ruler?) (wb:canvas-draw-line bw xmin ymin xmax ymin)))))))

(defmethod draw-axis ((self basic-axis) (position (eql :right)) vp)
  (let ((bw (window-of vp)))
    (wb:with-pen-values bw (draw-style self :color) 1 nil
      (multiple-value-bind (xmin xmax ymin ymax) (bounds-of vp)
        (declare (ignore xmin))
        (if (draw-style self :tics?)
          (let* ((font (draw-style self :font))
                 (tic-list (tic-list-for-viewport self vp)) 
                 (font-ascent (wb:canvas-font-ascent bw :font font))
                 (font-descent (wb:canvas-font-descent bw :font font))
                 (tic-len )
                 (tic-elen )
                 tic-end label-posn tic-label-wid)

            (if (eq (tic-length-of self) :viewport)
              (setf tic-len (width-of vp)
                    tic-elen 0)
              (setf tic-len (or (first (tic-length-of self))
                              (ceiling (* 0.7 font-ascent)))
                    tic-elen (or (second (tic-length-of self)) 0)))
            (setq tic-label-wid 
                  (loop for t-l in tic-list 
                        maximize (wb:canvas-string-width bw (cdr t-l) :font font)))
            
            (setf tic-end  (- xmax tic-len )
                    label-posn (- tic-end font-descent tic-label-wid))
            (draw-vertical-axis tic-list (and (draw-style self :tic-ruler?) xmax) tic-end (+ xmax tic-elen) label-posn 
                                font (draw-style self :tic-labels?) bw))
           (if (draw-style self :tic-ruler?) (wb:canvas-draw-line bw xmax ymin xmax ymax)))))))


(defmethod draw-axis ((self basic-axis) (position (eql :left)) vp)
  (let ((bw (window-of vp)))
    (wb:with-pen-values bw (draw-style self :color) 1 nil
      (multiple-value-bind (xmin xmax ymin ymax) (bounds-of vp)
        (declare (ignore xmax))
        (if (draw-style self :tics?)
          (let* ((font (draw-style self :font))
                 (tic-list (tic-list-for-viewport self vp)) 
                 (font-ascent (wb:canvas-font-ascent bw :font font))
                 (font-descent (wb:canvas-font-descent bw :font font))
                 (tic-len )
                 (tic-elen )
                 tic-end label-posn )
            
            (if (eq (tic-length-of self) :viewport)
              (setf tic-len (width-of vp)
                    tic-elen 0)
              (setf tic-len (or (first (tic-length-of self))
                              (ceiling (* 0.7 font-ascent)))
                    tic-elen (or (second (tic-length-of self)) 0)))
            
            (setf tic-end  (+ xmin tic-len)
                    label-posn (+ tic-end  font-descent ))
            (draw-vertical-axis tic-list (and (draw-style self :tic-ruler?) xmin) tic-end (- xmin tic-elen) label-posn 
                                font  (draw-style self :tic-labels?) bw))
           (if (draw-style self :tic-ruler?) (wb:canvas-draw-line bw xmin ymin xmin ymax)))))))
                            
                            



(defmethod default-axis-settings ((self basic-axis))
  (setf (ntics-of self) (default-ntics))
  (setf (slot-value self 'tic-info) nil)
  (setf (tic-list-of self) nil))

(defmethod set-extent ((self basic-axis)   min max &key viewport (draw? t) (msg "Change extent from ~S to:"))
  
  (default-axis-settings self)
  (let ((pos (prompt-position self viewport))
        (dir (axis-of-orientation self)))
    
    (cond ((and (null min) (null max))
           (let ((new (wb:prompt-user 
                       :result-type 'list
                       :read-type :read
                       :prompt-string (format nil msg
                                              (coerce (tic-interval-of self) 'list))
                       :left (2d-position-x pos)
                       :top (2d-position-y pos))))
             (setq min (first new))
             (setq max (second new))))
          ((null min)
           (setq min (wb:prompt-user 
                      :result-type 'number 
                      :read-type :eval
                      :prompt-string (format nil "Change min from ~S to:"
                                             (min-of (tic-interval-of self)))
                      :left (2d-position-x pos)
                      :top (2d-position-y pos))))
          ((null max)
           (setq max (wb:prompt-user 
                      :result-type 'number 
                      :read-type :eval
                      :prompt-string (format nil "Change max from ~S to:"
                                             (max-of (tic-interval-of self)))
                      :left (2d-position-x pos)
                      :top (2d-position-y pos))))
          (t nil))
    
    
    (change-bounding-region self (region-of-extent self min max)
                            :ignore-x? (eq dir :y) :ignore-y? (eq dir :x) :draw? draw?)))
  



(defmethod set-tics ((self basic-axis) tics &key  viewport (draw? t) (msg "Enter a list of tics"))
  (let ((pos (prompt-position self viewport))
        (dir (axis-of-orientation self)))
    (setf tics
          (or tics (wb:prompt-user :result-type 'list :read-type :read
                                   :prompt-string msg
                                   :left (2d-position-x pos)
                                   :top (2d-position-y pos))))
    (setf (tic-list-of self) tics)
    (let* ((f (car tics))
           (min (if (listp f)  (car f)  f))
           (l (first (last tics)))
           (max (if (listp l) (car l)  l)))
    (with-constrained-extent self dir draw?
      (set-bounding-region self 
                           :region (region-of-extent self min max)
                           )))))




(defmethod set-ntics ((self basic-axis) ntics  &key  viewport (draw? t) (msg "Enter number of tics"))
  (default-axis-settings self)
  (let ((pos (prompt-position self viewport))
        (dir (axis-of-orientation self)))
    (setf ntics (or ntics
                    (wb:prompt-user :result-type 'integer :read-type :eval
                                    :prompt-string msg
                                    :left (2d-position-x pos)
                                    :top (2d-position-y pos))))
    (setf (ntics-of self) ntics)
    (with-constrained-extent self dir draw?
      (set-bounding-region self 
                           :region (copy-region (bounding-region-of self))))))

(defmethod set-tic-format ((self basic-axis)  format &key viewport (draw? t))
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
  (setf (tic-format-of self) format)
  (if draw?
    (draw-view self :erase? t)))
   


(defmethod style-menu-items ((self basic-axis))
  
  `(("Font" nil "" :sub-items ,(font-menu-items))
    ("Tics On?"  (set-drawing-style :tics? :toggle))
    ("Tics Labels?"  (set-drawing-style :tic-labels? :toggle))
    ))




(defmethod get-menu-items ((self basic-axis) (slot-name (eql 'middle-menu)))
  
  `(( "#Tics"  (set-ntics nil :viewport))
    ( "Tic Limits"  (set-extent nil nil :viewport))
    ( "Tic List"  (set-tics nil :viewport))
    ( "Tic format"  (set-tic-format nil :viewport))
    ))


(defmethod update-menu-items :before ((self basic-axis) 
                                      (slot-name (eql 'middle-menu)))
  (let* ((m (slot-value self slot-name)))
    (wb:check-menu-item m "Tics On?" (draw-style self :tics?))
    (wb:check-menu-item m "Tics Labels?" (draw-style self :tics?))
    ))

(defmethod new-variable ((self axis) &key )
  (declare (ignorable self)) ;(ignore self)) ; 04SEP2023
  (call-next-method)
 )

(defmethod new-variable :before  ((self axis) &key )
                                    
  (setf (tic-list-of self) nil))


(defmethod hide-axis-p ((self basic-axis))
  (case (orientation-of self)
    (:vertical
     (not (some #'(lambda(v) (and (not (typep v 'axis))
                                  (show-y-axis-p v)))
                (link-bounds-y-of self))))
    (t
     (not (some #'(lambda(v) (and (not (typep v 'axis))
                                  (show-x-axis-p v))) 
                (link-bounds-x-of self))))))



#|
Currently, plotting on an alternate (eg log) scale is not supported,
without actually transforming the data via x-function y-function, etc.

To allow alternate scales, add a -plot-scale variable to each 1d and 2d view,
whose default value is identity, but could be log... This additonal transformation
would be applied to the cases (after -function -transform), see the function case-coords
in abstract-views.lisp. 
Axis should be modified to provide tic-labels on the original scale.
|#
