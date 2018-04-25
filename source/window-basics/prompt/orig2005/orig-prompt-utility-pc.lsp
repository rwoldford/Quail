;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; prompt-utility-pc.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;  This file is part of the Window-Basics package intended to provide a uniform
;;;  window environment for Common Lisp programming.
;;;  
;;;  For copyright information and history, see window-basics-copyright.lisp
;;;  Authors:
;;;     
;;;     G.W. Bennett 1996-1997
;;;     
;;;----------------------------------------------------------------------------------
;;; Two functions to deal with the layout of multi-line
;;; prompt-strings.

(in-package :wb)

(defun how-many (char string)
   "Counts the number of occurrences of char in string"
   (1+ (- (length string)
      (length (remove-if #'(lambda (x)
                             (eq x char)) string)))))

(defun list-of-lengths (string)
   ;; Assume string does *not* start with ~%
   ;; Gets a list of the stream-string-width of the
   ;; text bits of string
   (let ((result '())
         (fmat (list #\~ #\% #\Newline))
         (alphabet (list #\a #\b #\c #\d #\e #\f #\g #\h
                 #\i #\j #\k #\l #\m #\n #\o #\p #\q
                 #\r #\s #\t #\u #\v #\w #\x #\y #\z
                 #\A #\B #\C #\D #\E #\F #\G #\H
                 #\I #\J #\K #\L #\M #\N #\O #\P #\Q
                 #\R #\S #\T #\U #\V #\W #\X #\Y #\Z
                 #\Space  #\. #\, #\; #\: #\( #\)))
         )
      (push (cg::stream-string-width cg::*screen* string) result)
      (do ((j 0 (+ j 1))
           (n (how-many #\Newline string)))
           ((= j n))
           (setf string (string-left-trim fmat 
                        (string-left-trim alphabet string)))
           (push (cg::stream-string-width cg::*screen* string) result)
      )
    result))

(defun max-text-segment (lengths) 
   " Gets the length of the maximum text segment~
   from a collection of cumulative lengths of a piece~
   of text assumed to have come from list-of-lengths."
   (let ((diffs '()))
      (do ((j 0 (+ j 1))
           (n (length lengths)))
          ((= j (- n 1)))
         (push (- (elt lengths j) (elt lengths (+ j 1))) diffs))
      (cond ((eq 1 (length lengths))
             (first lengths))
            (T (abs (first (sort diffs #'<))))
            )
      ))