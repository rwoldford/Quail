;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                        Modifying refable objects
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (C) 1994 Statistical Computing Laboratory, University Of Waterloo
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                   eg-array-manip-n.lsp
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1994.
;;;
;;;
;;;   
;;;   In this file, we illustrate the use of the following map functions
;;;   macros for refable objects:
;;;
;;;       ... remove-slices            ... Removes all slices from ref-object which
;;;                                        match item when the function test is
;;;                                        applied to item and the slice of ref-object.
;;;                                        It is non-destructive to ref-object and
;;;                                        returns an appropriately tailored ref of
;;;                                        ref-object.
;;;       ... remove-slices-if         ... Removes every slice from ref-object which
;;;                                        returns non-NIL when the function pred is
;;;                                        applied to it.
;;;                                        It is non-destructive to ref-object and
;;;                                        returns an appropriately tailored ref of
;;;                                        ref-object.
;;;       ... substitute-slices        ... Returns a copy of ref-object with slices
;;;                                        that matched old-slice according to the
;;;                                        test function replaced by new-slice.
;;;                                        It is a non-destructive operation.
;;;                                        If no slices matched it returns a copy
;;;                                        of ref-object.
;;;       ... substitute-slices-if     ... Returns a copy of ref-object with slices
;;;                                        that returned non-NIL when tested by the
;;;                                        predicate function pred replaced by
;;;                                        the new-slice.
;;;                                        It is a non-destructive operation.
;;;                                        If no slices matched it returns a copy of
;;;                                        ref-object.
;;;       ... replace-slices           ... Replaces every slice of ref-object by the
;;;                                        result of calling function on that slice.
;;;                                        This means that function must return an
;;;                                        object of identical dimensions to the
;;;                                        original slice.
;;;                                        By default, replace-slices is not
;;;                                        destructive to the original ref-object.
;;;                                        If copy? is NIL then replace-slices will
;;;                                        be destructive and operate directly on the
;;;                                        original ref-object.
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :quail-user)

;;;
;;; Some arrays to work with.
;;;


(setf a (array  (iseq 6) :dimensions '(2 3)))

(setf b (array (iseq 1 24) :dimensions '(2 3 4)))  ;; 2 rows, 3 cols, 4 layers

(setf c (ref a 0))

(setf d (sel a))
(setf (eref d 0 0) NaN)

(setf e (sel b))
(setf (eref e  0 0 0) NaN)

;;;
;;; And the following print function may be helpful to see what the
;;; pieces.
;;;

(defun print-slices (obj &key 
                         (slices :elements)
                         (order :row))
  (let ((i 0))
    (format *quail-terminal-io* "~&~% Order is ~s~%" order)
    (doslices (slice obj slices 'done order)
      (format *quail-terminal-io* "~& Slice ~s is ~s" i slice)
      (incf i))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  remove-slices
;;;
;;; (remove-slices item ref-object
;;;                :slice 0 :test #'ref-eq)
;;;
;;;  Removes all slices from ref-object which
;;;  match item when the function test is
;;;  applied to item and the slice of ref-object.
;;;  It is non-destructive to ref-object and
;;;  returns an appropriately tailored ref of
;;;  ref-object.
;;;
;;;  Slice must be a SINGLE dimension number.
;;;  Nothing else makes sense.
;;;
;;; 

(remove-slices c a)
(remove-slices (ref a 0) a)
(remove-slices (ref a 1) a)
(remove-slices (sel a 1) a)

(remove-slices (ref a T 0) a :slice 1)
(remove-slices (ref a T 1) a :slice 1)
(remove-slices (ref a T 2) a :slice 1)

;;;
;;; Note that if the test would remove all slices
;;; then remove-slices returns NIL
;;;

(remove-slices   (ref a T 2)
                 a
                 :test
                 #'(lambda (x y)
                     (find-slices 3 x
                                  :test
                                  #'(lambda (a b) (or (numberp a)
                                                      (numberp b))))))
;;;
;;;  Also works on sequences
;;;

(remove-slices 3 '(0 1 2 3 4 5 4 3 2 1 6 6) :test #'=)

(remove-slices #\a "acaoaoaalaa!aaa" :test #'char= )

;;;
;;;  The reason for this is that if the
;;;  (number-of-dimensions ref-object) is <= 1 the slice
;;;  argument is ignored and slices are taken to be :elements.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  remove-slices-if
;;;
;;; (remove-slices-if pred ref-object :slices 0)
;;;
;;;  Removes every slice from ref-object which
;;;  returns non-NIL when the function pred is
;;;  applied to it.
;;;  It is non-destructive to ref-object and
;;;  returns an appropriately tailored ref of
;;;  ref-object.

;;;
;;;  Remove all row slices which contain an element less than 3
;;;

(remove-slices-if  #'(lambda (x) (find-slices 3 x :test #'>))
                   a
                   :slice 0)

;;;
;;;  The same test removes all the columns of a!
;;;  And so remove-slices-if returns NIL
;;;

(remove-slices-if   #'(lambda (x) (find-slices 3 x :test #'>))
                    a
                    :slice 1)

;;;
;;;  A less stringent test would be
;;;

(remove-slices-if   #'(lambda (x) (find-slices 1 x :test #'>))
                    a
                    :slice 1)


;;;
;;;  And again works on sequences
;;;

(remove-slices-if #'(lambda (x) (= x 3))
                  '(0 1 2 3 4 5 4 3 2 1 6 6))

(remove-slices-if #'(lambda (x)
                      (char= x #\a))
                  "awaoaaawaa!aaa")

(defun find-nan (thing)
  (find-slices NaN thing :test #'eq))

(defun remove-NaNs (thing &key (slice 0))
  (remove-slices-if #'find-nan thing :slice slice))

(remove-nans d)
(remove-nans d :slice 1)
(remove-nans e)
(remove-nans e :slice 0)
(remove-nans e :slice 1)
(remove-nans e :slice 2)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                        substitute-slices
;;;
;;;
;;;
;;;(substitute-slices old-slice new-slice ref-object
;;;                   :slices :elements
;;;                   :test #'ref-eq)
;;;
;;;  Returns a copy of ref-object with slices
;;;  that matched old-slice according to the
;;;  test function replaced by new-slice.
;;;  It is a non-destructive operation.
;;;  If no slices matched it returns a copy
;;;  of ref-object.
;;;
;;;
;;;  Replace NaN by 9999

(substitute-slices NaN 9999 d)

(substitute-slices #\o #\a "ooooh" :test #'char=)

;;;
;;;  Replace row 0 of d  with c if row 0 contains NaN
;;;

(substitute-slices (ref d 0) c d
                   :slices 0
                   :test
                   #'(lambda (x y) (find-nan y)))

;;;
;;;  Note that our old-slice was completely ignored by the test
;;;  function so it could have been anything.
;;;

(substitute-slices "fubar" c d
                   :slices 0
                   :test
                   #'(lambda (x y) (find-nan y)))

;;;
;;;  What would happen if we forgot the slices argument?
;;;

(substitute-slices (ref d 0) c d
                   :test
                   #'(lambda (x y) (find-nan y)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;;                       substitute-slices-if
;;;
;;;
;;; (substitute-slices-if
;;;            new-slice pred ref-object
;;;            :slices :elements)
;;;
;;;  Returns a copy of ref-object with slices
;;;  that returned non-NIL when tested by the
;;;  predicate function pred replaced by
;;;  the new-slice.
;;;  It is a non-destructive operation.
;;;  If no slices matched it returns a copy of ref-object.
;;;
;;;
;;;
;;;
;;;  Replace NaN by 9999

(substitute-slices-if 9999 #'find-Nan d)

(substitute-slices-if #\a                           ; new-slice
                      #'(lambda (x) (char= x #\o))  ; test predicate
                      "ooooh")                      ; refable object

;;;
;;;  Replace with c every row of d that contains NaN.
;;;

(substitute-slices-if  c #'find-nan d :slices 0)

;;;
;;;  Let's get a bigger array with more NaNs.
;;;

(dimensions-of e)

;;; e already has one NaN let's put two more in somewhere

(<- (ref e 0 0 '(2 3)) (array NaN :dimensions 2))

;;; and shuffle the elements around

(setf f (permute e))

;;;
;;;  Now replace every row of f that has a NaN with
;;;

(setf g (array '(1111 2222 3333)))

(substitute-slices-if  g #'find-nan f :slices '(0 2))

;;;
;;;  Every column of f that has a NaN with
;;;

(setf g (array '(4444 5555)))

(substitute-slices-if  g #'find-nan f :slices '(1 2))

;;;
;;;  Every layer of f that has a NaN with
;;;

(setf g (array '(1111 2222 3333 4444 5555 6666)
               :dimensions '(2 3)))

(substitute-slices-if  g #'find-nan f :slices 2)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;;                       replace-slices
;;;
;;;
;;;
;;; (replace-slices ref-object
;;;                 :slices :elements
;;;                 :function #'identity
;;;                 :copy? T)
;;;
;;;
;;;  Replaces every slice of ref-object by the
;;;  result of calling function on that slice.
;;;  This means that function must return an
;;;  object of identical dimensions to the
;;;  original slice.
;;;  By default, replace-slices is not
;;;  destructive to the original ref-object.
;;;  If copy? is NIL then replace-slices will
;;;  be destructive and operate directly on the
;;;  original ref-object.
;;;
;;;;;;;;;;

;;;
;;;  Replace-NaNs again
;;;

(replace-slices d :function #'(lambda (x) (if (find-nan x)
                                            9999
                                            x)))
;;;
;;; Now destructively
;;;

(replace-slices d :function #'(lambda (x) (if (find-nan x)
                                            9999
                                            x))
                :copy? NIL)
d

;;;
;;; Replace all numbers greater than 12 by 1 and
;;; the others by 0.  If they are NaNs then leave them as such.
;;;
e
(replace-slices e
                :function
                #'(lambda (x)
                    (cond
                     ((eq x NaN) NaN)
                     ((> x 12) 1)
                     (T 0))))
;;;
;;;  Nice for making numerical data from 
;;;  coded categorical
;;;

(setf categories
      (permute
       (array (list "Red" "Green" "Blue") :dimensions '(4 3))))

(replace-slices categories
                :function
                #'(lambda (x)
                    (cond
                     ((string= x "Red")     1)
                     ((string= x "Green")   2)
                     ((string= x "Blue")    3)
                     ))
                )


;;;
;;;  Note that when slices are :elements (default)
;;;  That messing up on the dimensions will still work
;;;  but may surprise you.

(replace-slices d
                :function
                #'(lambda (x)
                    (cond
                     ((> x 3) 1)
                     (T (array '(100 200))))))

;;;
;;;  Here we replace rows of d by random numbers
;;;

(replace-slices d
                :function
                #'(lambda (x)
                    (array
                     (random-discrete-uniform
                      :from 0 :to 99
                      :n (number-of-elements x))
                     :dimensions (dimensions-of x)))
                :slices 0)

;;;
;;;  Here we generate an error by getting the dimensions wrong of the
;;;  replacement slice.
;;;
#|
(replace-slices d
                :function
                #'(lambda (x)
                    (iseq 10))
                :slices 0)

|#
;;;
;;;  And again.
;;;
#|
(replace-slices d
                :function
                #'(lambda (x)
                    100000)
                :slices 0)
|#
;;;
;;; Note that if things break in the middle of a destructive
;;; replace, the result can be pretty messed up.  Be careful.
;;;

(setf h (array (list NaN 1 2 3 4 5) :dimensions  '(2 3)))

;;;
;;;  The following fails AFTER some replacement has been done!
;;;
#|
(replace-slices h
                :copy? NIL
                :function
                #'(lambda (x)
                    (if (find-nan x)
                      (iseq 3)
                      (iseq 4)))
                :slices 0)
|#
;;;
;;; Here's the changed h.
;;;

h


