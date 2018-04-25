;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                          Iteration Introduction                             
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (C) 1994 Statistical Computing Laboratory, University Of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1994.
;;;
;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Iteration can be done in many different ways in Common Lisp.
;;;
;;;   There are a variety of iteration special forms and macros
;;;   in common lisp.
;;;   These can be very roughly grouped into
;;;
;;;   General iteration macros like loop, do, do* 
;;;   Simple iteration macros like dolist, and dotimes
;;;   Mapping functions like map, mapcar, reduce, ...
;;;   specialized for ``mapping over'' sequences or more often just lists
;;;
;;;   And finally, some program ``features'' like go and tagbody that while
;;;   they allow you a great deal of freedom are really too fortranesque
;;;   to be recommended for the general user.
;;;
;;;   In Quail, we have introduced some functions that allow one to use
;;;
;;;   General iteration macros with arrays
;;;       ... column-major-eref
;;;           column-major-list-elements
;;;           column-major-set-elements
;;;           column-major-ref-slice
;;;           row-major-eref
;;;           row-major-list-elements
;;;           row-major-set-elements
;;;           row-major-ref-slice
;;;
;;;   Simple iteration macros
;;;       ... doslices
;;;           collect-slices
;;;
;;;   Mapping functions:
;;;       ... map-element
;;;           map-slices
;;;           reduce-slices
;;;           collapse
;;;           sweep
;;;
;;;   Searching functions:
;;;       ... find-slices, find-slices-if
;;;           count-slices, count-slices-if
;;;           slice-positions, slice-positions-if
;;;
;;;   Modification functions:
;;;       ... remove-slices, remove-slices-if
;;;           substitute-slices, substitute-slices-if
;;;           replace-slices
;;;           
;;;           
;;;
;;;   We will give examples of the uses of these functions over the next few files.
;;;   In this file, we will just illustrate some of the Common Lisp iteration
;;;   constructs with Quail arrays.
;;;
;;;   If your application program involves arrays of only fixed dimension,
;;;   say 2 (i.e. matrices) the code will often be more efficient (at less
;;;   general) by sticking to the iteration control described in this file.


(in-package :quail-user)

(setf a (array '((1.2 3 4.5) (6.7 8.9 0.1))
                :dimensions '(2 3)))

(setf b (seq 10 -10 -1))
(setf small-b (ref b '(:c 0 1 2)))        ;; all but the first three elements of b

(setf c (array (random-uniform :n 4) :dimensions '(2 2)))

(setf d (sel a))  ;; a copy of a

(setf e (array (seq 1 24) :dimensions '(2 3 4)))  ;; 2 rows, 3 cols, 4 faces

;------------------------------------------------------------------------------

;;;
;;; Here's a function that will print the (i,j)'th element of matrix
;;;
;;;

(defun print-elt (matrix i j)
  "Prints element i j of matrix."
  (format *quail-terminal-io*
          "~&Element (~s,~s) = ~s"
          i j (eref matrix i j)))
          

;;;
;;;  The most natural looping construct is the loop macro.
;;;  It has lots of nice keywords like: for from to by do collect sum ....
;;;  that make it relatively natural to write and easy to read.
;;;
;;;  Here's a function that prints out all elements of a matrix

(defun print-matrix (a)
  "Print the contents of the matrix a in row-major-order"
  (let ((m (first (dimensions-of a)))
        (n (second (dimensions-of a))))
    (loop for i from 0 to (- m 1)
          do
          (loop for j from 0 to (- n 1)
                do
                (print-elt a i j))
          )
    )
  )

(print-matrix a)


;;; Note that it will not work for b  as b has no second dimension
;;;
;; (print-matrix b)    ;; error

;;; Nor will it work for e ; too many dimensions
;;;

;; (print-matrix e)  ;; error

;;;
;;; Writing functions for arrays of arbitrary dimensions can be a pain.
;;; That's why we have included several functions in Quail that allow
;;; one to access elements of an array of any shape or size.

(defun print-elements (a)
  "This prints out all the elements of the array a in row major order"
  (let ((n (number-of-elements a)))
    (format *quail-terminal-io* "~&In row-major order,")
    (loop for i from 0 to (- n 1)
          do (format *quail-terminal-io*
                     "~&Element ~s = ~s"
                     i
                     (row-major-eref a i)))))

;;;
;;; All of the following will work
;;;

(print-elements a)
(print-elements b)
(print-elements e)


;;;
;;;  But more on that later (in files after this one).
;;;  First we illustrate a few more common lisp iteration constructs.
;;; 
;;;  Here's an example of do

(do  ((i 0 (+ i 1)))
     ((= i (first (dimensions-of b)))
      (list i))
  (format *quail-terminal-io* "~&------")
  (print (eref b i)))

;;;
;;;  Not quite as obvious what's going on is it?  
;;;  Still, the control structure is straightforward and general
;;;  do is often preferred for this reason.  Here it is dissected.


(do  (                                    ;(do  (
      (i 0 (+ i 1))                       ;      (iteration-var initial-value step-form)
                                          ;       .... as many as you want
      )                                   ;      )
     (                                    ;     (
      (= i (first (dimensions-of b)))     ;      end-test-form ... loop ends when this
                                          ;                        evaluates to non-NIL
      (list i)                            ;      result-form
      )                                   ;      )
  (format *quail-terminal-io* "~&------") ;  form1
  (print (eref b i))                      ;  form2
                                          ;  ... as many as you want
  )                                       ;  )  and the loop ends returning
                                          ;     the value of result-form

;;;
;;; The do* macro is exactly the same except that the binding of the iteration
;;; variables occurs in parallel with do and in top-down sequence with do*
;;; Same as the distinction between let and let*
;;;

;;;
;;;  dolist is pretty straightforward
;;;

(<- bdbd
      (dolist (i (list "How" "now" "brown" "cow") "That's all folks!")
        (print i)
        (print "?")))
bdbd

;;;
;;; as is dotimes
;;;

(<- fu
      (dotimes (i 5 'bar)
        (print i)
        (print (- i))))
FU

;;;
;;;  Then there is mapping.
;;;  Lots of mapping functions for lists, some for more general things 
;;;  called sequences (including lists and strings)
;;;  We'll only illustrate a few here.
;;;

;;;
;;;  For, a list there is the age-old mapcar
;;;  which maps a specified function over the elements 
;;;  of a list and returns a list of those values
;;;

(mapcar (function abs) '(-3 -2 -1 0 1 2 3))
;;;
;;; or in the preferred short hand for functions
;;;

(mapcar #'abs '(-3 -2 -1 0 1 2 3))
(mapcar #'+ '(-3 -2 -1 0 1 2 3) '(3 3 3 3 3 3 3))

;;;
;;; the function being mapped over the elements of the list must
;;; be able to handle as many arguments are there are lists.
;;;
;;;
;;; The general sequence version of this is map.
;;; Because there are so many different types of sequence,
;;; You need to specify the type of the return sequence
;;; when you call map
;;;

(map 'list #'abs '(-3 -2 -1 0 1 2 3))
(map 'list #'+ '(-3 -2 -1 0 1 2 3) '(3 3 3 3 3 3 3))
(map 'string #'(lambda (x y)
                 (if (char= x y) x (eref "-" 0)))    ; An anonymous function
     "Now is the time for all good women to aid!"
     "Never have so few  sacrificed so little  !")

;;;
;;; And also for sequences
;;;

(reduce #'+ '(1 2 3 4))
(reduce #'* '(1 2 3 4))
(reduce #'+ '(1 2 3 4) :initial-value 100)

;;;
;;;  And many others!
;;;  Including subseq copy-seq elt concatenate position position-if
;;;            find find-if sort remove remove-if 
;;;            substitute substitute-if count count-if delete ....
;;;
;;;
;;;  Unfortunately none of these work on arrays with the Quail exception of
;;;  sort.
;;;
;;;  Hence the functions in the remaining iteration files.
