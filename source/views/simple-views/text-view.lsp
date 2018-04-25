;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               text-view.lisp
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
;;;     C.B. Hurley 1992 George Washington University
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :views)

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '( text-view number-of-text-lines)))

(defclass text-view (label) 
  ((middle-menu :allocation :class :initform nil))
  (:default-initargs :justification-menu? nil :orientation-menu? nil))



(defmethod draw-view ((self text-view) &key viewport)
  (let ((col (draw-style self :color))
        (font (draw-style self :font))
        (string (get-text self))
        (start 0)
        )
    (flet
      ((read-word (string)
         (If (> start (length string))
           nil
           (let* ((pos-space (position #\Space string :test #'char= :start start))
                  (pos-line (position #\Newline string :test #'char= :start start))
                  result
                  to-newline?)
             (cond
              ((and pos-line pos-space)
               (cond
                ((< pos-line pos-space)
                      (setq result (subseq string start pos-line))
                      (setq start (+  1 pos-line))
                      (setq to-newline? T))
                (T (setq result (subseq string start (+ 1 pos-space)))
                   (setq start (+  1 pos-space)))))
              (pos-space
               (setq result (subseq string start (+ 1 pos-space)))
               (setq start (+  1 pos-space)))
              (pos-line
               (setq result (subseq string start pos-line))
               (setq start (+  1 pos-line))
                      (setq to-newline? T))
              (T  (setq result (subseq string start))
                  (setq start (+ 1 (length string)))))
             (cons result to-newline?)))))
      
      (with-exposed-viewports self viewport vp
        (let* ((w (window-of vp))
               (leading (wb:canvas-font-leading w :font font))
               (descent (wb:canvas-font-descent w :font font))
               (ascent (wb:canvas-font-ascent w :font font))
               (height (+ leading ascent descent)))
          (wb:with-pen-color w col
            (wb:with-canvas-font w font
              (multiple-value-bind (l r b tp ) (bounds-of vp)
                (wb:canvas-move-to w l (- tp ascent))
                (loop for (word . newline) = (read-word string)
                      until (or (null word) (< (- (wb:canvas-y w) descent)  b))
                      do 
                      (when (> (wb:canvas-string-width w word) (- r (wb:canvas-x w)))
                        (wb:canvas-move-to w l (- (wb:canvas-y w) height)))
                      (wb:canvas-draw-string w word)
                      (if newline
                        (wb:canvas-move-to w l (- (wb:canvas-y w) height)))
                      )))))))))

 
#|
(defun number-of-text-lines (text-view width)
  "Returns the number of lines required to fit the text-view into the given width."
  (let* ((font (draw-style text-view :font))
         (string (get-text text-view))
         (n 1)
         (line-pos 0)
         (start 0)
         )
    (flet
      ((read-word (string)
         (If (> start (length string))
           nil
           (let* ((pos-space (position #\Space string :test #'char= :start start))
                  (pos-line (position #\Newline string :test #'char= :start start))
                  (result
                   (cond (pos-line (subseq string start pos-line))
                         (pos-space (subseq string start (+ 1 pos-space)))
                         (t (subseq string start)))))
             
             (setq start (+  1 (or pos-line pos-space (length string))))
             
             (cons result pos-line)))))
      
      (loop for (word . newline) = (read-word string)
            until (null word)
            do
            (when (> (wb:canvas-string-width NIL word :font font)
                     (- width line-pos))
              (incf n 1)
              (setf line-pos 0))
            (incf line-pos (wb:canvas-string-width NIL word :font font))
            (when newline
              (incf n 1)
              (setf line-pos 0))
            )
      n)))
|#
 
(defun number-of-text-lines (text-view width)
  "Returns the number of lines required to fit the text-view into the given width."
  (let* ((font (draw-style text-view :font))
         (string (get-text text-view))
         (n 1)
         (line-pos 0)
         (start 0)
         )
    (flet
      ((read-word (string)
         (If (> start (length string))
           nil
           (let* ((pos-space (position #\Space string :test #'char= :start start))
                  (pos-line (position #\Newline string :test #'char= :start start))
                  (which (cond
                          ((and (numberp pos-space) (numberp pos-line))
                           (if (< pos-space pos-line)
                             :space
                             :line))
                          (pos-space :space)
                          (pos-line :line)
                          (T
                           :end)))
                  (result
                   (case which
                     (:line
                      (prog1
                        (subseq string start pos-line)
                        (setq start (1+ pos-line))))
                     (:space
                      (prog1
                        (subseq string start (+ 1 pos-space))
                        (setq start (1+ pos-space))))
                     (:end (prog1
                             (subseq string start)
                             (setq start (1+ (length string))))))))

             (cons result (eq which :line))))))
      
      (loop with word-width
            for (word . newline) = (read-word string)
            until (null word)
            do
            (setq word-width (wb:canvas-string-width NIL word :font font))
            (when (> word-width
                     (- width line-pos))
              (incf n 1)
              (setf line-pos 0))
            (incf line-pos word-width)
            (when newline
              (incf n 1)
              (setf line-pos 0)))
      n)))
