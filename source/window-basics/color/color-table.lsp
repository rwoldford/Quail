;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                              color-table.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;
;;;  This file is an addition to Window-basics for moving pointclouds
;;;  
;;;
;;;  Authors:
;;;      R.W. Oldford 1992
;;;      P. Poirier 1992
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package :window-basics)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(make-color-table)))

(defun make-color-table (&key number colors
                              (red? NIL)
                              (green? NIL)
                              (blue? NIL)
                              (from-color NIL)
                              (to-color NIL)
                              (fast-colors? NIL))
  "Creates and returns a vector of colours as determined by the values of ~
   the supplied keywords.  ~
   If number is given it defines the number of colors to be used (default 10).~
   If colors is supplied, then it should be a sequence containing the color ~
   records to be used in the table.  ~
   If both colors and number are supplied then the first number from colors ~
   are selected to be elements of the table (with wrapping if the length of ~
   colors is less than number).  ~
   If number is given and not colors, then the keywords red? green? and blue? ~
   come into play.  ~
   These are booleans which if non-nil mean ~
   every color in the table will be saturated with the corresponding color.  ~
   For example, the ~
   configuration  red? = NIL green? = NIL and blue? = T  produces ~
   a table of colors of size number which vary from pure blue through ~
   light blue to bright white.  ~
   The default ~
   configuration is red? = NIL green? = NIL and blue? = NIL and produces ~
   a table of colors of size number which vary from black to bright white.  ~
   If either from-color (default *black-color*) ~
   or to-color (default *bright-white-color*) is given then a linear interpolation ~
   on each rgb color component is made between these two colors.  ~
   If fast-colors? is T (default is NIL) then the color records used in the ~
   table are of optimal record-type for the given machine."
  #-:sbcl(declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0)))
   (declare (special *black-color*
                    *bright-white-color*))
  (unless (or colors number) (setf number 32))
  (flet
    ((get-color-piece (index num-col color?)
       ;;(declare (special *max-color-saturation*))
       (if color?
         1.0
         (/ index num-col)
         ;;(truncate (* index *max-color-saturation*) num-col)
         ))
     
     (smooth-color (s-r s-g s-b e-r e-g e-b index num-col)
       (make-color
        (+ s-r (/ (* index (- e-r s-r)) num-col))
        (+ s-g (/ (* index (- e-g s-g)) num-col))
        (+ s-b (/ (* index (- e-b s-b)) num-col))
        ;;(truncate (+ s-r (/ (* index (- e-r s-r)) num-col)))
        ;;(truncate (+ s-g (/ (* index (- e-g s-g)) num-col)))
        ;;(truncate (+ s-b (/ (* index (- e-b s-b)) num-col)))
        ))
     )
    
    (cond
     ((and number colors)
      (let ((ncolors (length colors)))
        (make-array
         (list number)
         :initial-contents
         (if fast-colors?
           (loop for i from 0 to (- number 1)
                 with j =  0
                 collect 
                 (if (colorp (elt colors j))
                   (optimize-color-record (elt colors j))
                   (elt colors j))
                 do
                 (if (= j ncolors)
                   (setf j 0)
                   (incf j))
                 )
           (loop for i from 0 to (- number 1)
                 with j =  0
                 collect
                 (if (colorp (elt colors j))
                   (elt colors j)
                   (deoptimize-color-record (elt colors j)))
                 do
                 (if (= j ncolors)
                   (setf j 0)
                   (incf j))
                 )))))
     (number
      (let ((nc (- number 1)))
        (make-array
         (list number)
         :initial-contents
         (if (or from-color to-color)
           (let
             (start-red
              start-green
              start-blue
              end-red
              end-green
              end-blue)
             (unless from-color
               (setf from-color *black-color*))
             (unless to-color
               (setf to-color *bright-white-color*))
             (setf
              start-red      (red-of from-color)
              start-green    (green-of from-color)
              start-blue     (blue-of from-color)
              end-red        (red-of to-color)
              end-green      (green-of to-color)
              end-blue       (blue-of to-color))
             (if fast-colors?
               (loop for i from 0 to nc
                     collect
                     (optimize-color-record 
                      (smooth-color start-red
                                    start-green
                                    start-blue
                                    end-red
                                    end-green
                                    end-blue
                                    i nc)))
               (loop for i from 0 to nc
                     collect
                     (smooth-color start-red
                                   start-green
                                   start-blue
                                   end-red
                                   end-green
                                   end-blue
                                   i nc))
               ))
           (if fast-colors?
             (loop for i from 0 to nc
                   collect
                   (optimize-color-record 
                    (make-color (get-color-piece i nc red?)
                                (get-color-piece i nc green?)
                                (get-color-piece i nc blue?))))
             (loop for i from 0 to nc
                   collect
                   (make-color (get-color-piece i nc red?)
                               (get-color-piece i nc green?)
                               (get-color-piece i nc blue?)))
             ))))
      )
     (colors
      (make-array
       (length colors)
       :initial-contents
       (if fast-colors?
         (loop for j from 0 to (1- (length colors))
               collect
               (if (colorp (elt colors j))
                 (optimize-color-record (elt colors j))
                 (elt colors j)))
         (loop for j from 0 to (1- (length colors))
               collect
               (if (colorp (elt colors j))
                 (elt colors j)
                 (deoptimize-color-record (elt colors j))))))))
    ))
