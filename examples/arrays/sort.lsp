;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                   Sorting array contents
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1994.

(in-package :quail-user)

(setf a (array '((1.2 3 4.5) (6.7 8.9 0.1))
                :dimensions '(2 3)))

(setf b (seq 10 -10 -1))
(setf small-b (ref b '(:c 0 1 2)))        ;; all but the first three elements of b

(setf c (array (random-uniform :n 4) :dimensions '(2 2)))

(setf d (sel a))  ;; a copy of a

(setf e (array (iseq 1 24) :dimensions '(2 3 4)))  ;; 2 rows, 3 cols, 4 layers

;;;----------------------------------------------------------------------------


;;;;;;;;;;;;;;;;;;
;;;
;;;    SORT has two required arguments:  The object to be sorted and
;;;    the predicate to be used to compare two elements.
;;;    The predicate should take two arguments and return non-NIL if and
;;;    only if the first argument is to be regarded as preceding the
;;;    the second.
;;;    As with the Common Lisp sort function, sort is by default destructive.
;;;    Unlike the Common Lisp sort it has a keyword copy? which when T
;;;    will first copy the object and then sort it.
;;;    Sorts the sub-arrays of slices of ref-object into an order  
;;;    determined by predicate.  The arguments predicate and key are to be interpreted 
;;;    exactly as the same named arguments in the Common Lisp sort function.  
;;;    The argument stable? is NIL (the default) or non-NIL depending on whether 
;;;    it is desirable that stability be guaranteed in the sense of the Common Lisp
;;;    function  stable-sort.
;;;    
;;;    These types of sort are possible:   
;;; 
;;;        1. If the arguments permit, then sort follows the Common Lisp standard. 

              (sort '(5 4 3 5 1 2) #'<)
              (sort "zyxwv" #'char< )

;;;        2. For most uses, the sort in Quail will look just like the sort in
;;;           Common Lisp, just extended to cover arrays as well as sequences.

              (sort b #'<)

;;;           As was the case in the previous example, this was destructive.
;;;           This has important consequences when we use ref instead of sel
;;;           to reference blocks of an array.  Not only is b affected by the
;;;           above sort, but so is any ref of it.  Look at 

              small-b

;;;           It is now all but the first three elements of the SORTED b.
;;;
;;;           For non-destructive sorts, the keyword argument copy? must
;;;           be non-NIL

              (sort a #'< :copy? T)

;;;           has now sorted the elements of a into a copy of a. The array a
;;;           is unchanged.
;;;           Note that the cost is cost of copying a before the sort.

;;;        3. If object is a ref-object and type is :interior then,
;;;           for example, 

              (sort d #'> :slices 1 :type :interior)

;;;           rearranges the contents of each column  (slices 1) of the matrix d 
;;;           so that the greatest element is in the first row, and so forth. 
   
;;;        4. If object is a ref-object and type is :exterior (the default), then

              (sort a #'> :key #'sum :slices 1 :type :exterior)

;;;           rearranges the  columns (slices 1) of the matrix a 
;;;           so that the column with the greatest total is the 0-th column, and so forth. 
;;;   
;;;        5. In all of the above, destructive sorts are performed if copy? is nil  
;;;           (the default). 
;;;           If copy? is t, then a non-destructive sort is performed.
;;;
;;;
;;;
;;;    Now let's start fooling around with the 3-way array e
;;;    This should give us some idea of what slices are all about as well
;;;    as the distinction between interior and exterior.
;;;    We will always work with copies so as to not mess up the original array, e.
;;;
;;;    First some familiarity with e is in order.
;;;
      (dimensions-of e)

;;;   Now suppose that we wanted a sort where the slice was defined to be 1

      (setf slice 1)

;;;   that is if e is 2 by 3 by 4, then we would want to look at all 2 by 4
;;;   sub-blocks of e and these are indexed entirely by the 1 index.
;;;   they are

(loop for i from 0 to (- (number-of-slices e slice) 1)    ; 1 must be subtracted
                                                          ; because of zero based
      do                                                  ; indexing
      (print i)
      (print (ref e T i T))                               ; Slice indexed by second
                                                          ; second index i.e. index 1
                                                          ; Entries taken are with all
                                                          ; other indices used (ie T)
      )

;;;   A little nicer print out illustrates the use of the CL format function
;;;

(defun print-slice (object i)
  (format *quail-terminal-io*
              "~&Slice ~s is ~%~10T ~s"
              i
              (ref object T i T)))

(loop for i from 0 to (- (number-of-slices e slice) 1)
      do
      (print-slice e i)
      )
              
;;;
;;;  Note that each slice is of the proper dimension.
;;;

;;;
;;;  Suppose that we want to sort the contents of each slice in descending order
;;;

(<- f (sort e #'> :slices slice :copy? T :type :interior))

(loop for i from 0 to (- (number-of-slices f slice) 1) 
      do
      (print-slice f i)
      )



;;;
;;;  And finally, suppose that we want to shuffle the slices so that
;;;  the one with the larger mean elements are first.
;;;

(loop for i from 0 to (- (number-of-slices e slice) 1) 
      do
      (print-slice e i)
      (format *quail-terminal-io*
              "~&~10T Mean = ~s"
              (mean (ref e t i t)))
      
      )


(defun bigger (a b)
  (> (mean a) (mean b)))

(<- g (sort e #'bigger :slices slice :type :exterior :copy? T))

(loop for i from 0 to (- (number-of-slices g slice) 1)
      do
      (print-slice g i)
      (format *quail-terminal-io*
              "~&~10T Mean = ~s"
              (mean (ref g t i t)))
      )

;;;
;;;  Interior sorts when no slices are given will apply the sort to
;;;  individual elements.
;;;

(sort (list 1 2 3) #'> :type :interior)

;;;  will try to sort each of the interiors 1, 2 and 3 respectively and
;;;  so will return the original list unaltered.
;;;
;;;  Similarly

(sort "zyxwvutsrqponmlkjihgfedcba" #'char< :type :interior)

;;;  doen't really do anything and so is quite different from 

(sort "zyxwvutsrqponmlkjihgfedcba" #'char< :type :exterior)

;;;
;;;  The copy? flag applies only to the original object
;;;  NOT TO ITS ELEMENTS!

(setf foo (list a b ))

(setf bar (sort (list a b) #'> :type :interior :copy? T))


;;; Note that this sort is destructive to the interior elements!

 a
 b

;;; The copy? flag applied only to the original list foo.
;;; foo and bar have the same contents

(and (eq (eref foo 0) (eref bar 0))
     (eq (eref foo 1) (eref bar 1)))

;;; But bar is indeed a copy of foo and hence a different list.

(eq foo bar)


;;;
;;;
;;;  EXTENDING SORT.  
;;;  If you want to extend sort to new object classes,
;;;  You need only define the appropriate methods for the generic-function
;;;  sort-object using defmethod or defmethod-multi
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;;   SORT-POSITION    and      RANKS
;;;
;;;   Sometimes one would only like the relative positions of the
;;;   elements or slices of an object.
;;;   On other occasions one would like to know the relative rankings of the
;;;   elements or slices of an object.
;;;
;;;   These two different functionalities are given by the functions
;;;   sort-position and ranks respectively.
;;;   It is easy to confuse these two different functionalities. BE CAREFUL!
;;;
;;;   Some examples will help.

(setf h '(1 4 2 3))

;;;
;;;  In this sequence the first number is the smallest and so has rank 0,
;;;  the second is largest and so has rank 3, and so on

(ranks  h #'<)

;;;  returns the desired rankings.
;;;
;;;  When we are concerned about sorting the numbers however, we may like to 
;;;  know what the position the sorted elements have in the original object.
;;;  Sorted, this list would be

(setf i (sort h #'< :copy? T))

;;;  The positions of these elements in the original object h are

(setf j (sort-position h #'<))

;;;
;;;  The sort-positions are the indices in the original object of the
;;;  elements in sorted position.
;;;  You could use these positions to look up ordered values in the 
;;;  original object, as in 

(loop for pos in (sort-position h #'<)
      collect (eref h pos))

;;;
;;;  to collect the ordered values of the list h.
;;;
;;;  Alternatively there exists a function called ORDER that will
;;;  do just that for you.
;;;  

(order h (sort-position h #'<) :copy? T )

;;;
;;;  Some higher dimensional examples:
;;;
;;;  Let's look at e and a random permutation of its contents.
;;;
e
(dimensions-of e)
(setf k (permute e :copy? T))
(dimensions-of k)

;;;
;;; First ranks
;;;

;;; The ranks of the contents (in row-major-order) are

(ranks e #'<)
(ranks k #'<)

;;; The ranks within every element of the contents (in row-major-order) are

(ranks e #'< :type :interior)

;;;  Now within various slices
;;;
;;;
;;;
;;; The ranks within each layer are easiest to see
;;;
e
(ranks e #'< :slices '(0 1) :type :interior)
k
(ranks k #'< :slices '(0 1) :type :interior)
;;;
;;; The ranks within each layer of the contents (in row-major-order) are
;;;

(ranks e #'< :slices 2 :type :interior)
(ranks k #'< :slices 2 :type :interior)


;;;
;;; The ranks within each row
;;;

(ranks e #'< :slices '(0 2) :type :interior)
(ranks k #'< :slices '(0 2) :type :interior)

;;;
;;; The ranks within each column
;;;

(ranks e #'< :slices '(1 2) :type :interior)
(ranks k #'< :slices '(1 2) :type :interior)

;;;
;;;   And some :exterior rankings.  Again layers are easiest to see.
;;;

e
(ranks e #'< :key #'sum :slices '(0 1) :type :exterior)

;;;
;;; or equivalently
;;;

(ranks e #'(lambda (x y) (< (sum x) (sum y))) :slices '(0 1) :type :exterior)

;;;
;;; and for k
;;;

k
(ranks k #'< :key #'sum :slices '(0 1) :type :exterior)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                        SORT-POSITION
;;;
;;;
;;;


;;; The positions of the contents (in row-major-order) are

(sort-position e #'<)
(sort-position k #'<)

;;; The positions within every element of the contents (in row-major-order) are

(sort-position e #'< :type :interior)


;;;  Now within various slices
;;;
;;;
;;;
;;; The positions within each layer are easiest to see
;;;
e
(sort-position e #'< :slices '(0 1) :type :interior)
k
(sort-position k #'< :slices '(0 1) :type :interior)

;;;
;;; The positions within each layer of the contents (in row-major-order) are
;;;

(sort-position e #'< :slices 2 :type :interior)
(sort-position k #'< :slices 2 :type :interior)


;;;
;;;   And some :exterior rankings.  Again layers are easiest to see.
;;;

e
(sort-position e #'< :key #'sum :slices '(0 1) :type :exterior)

;;;
;;; or equivalently
;;;

(sort-position e #'(lambda (x y) (< (sum x) (sum y))) :slices '(0 1) :type :exterior)

;;;
;;; and for k
;;;

k
(sort-position k #'< :key #'sum :slices '(0 1) :type :exterior)
;;;
;;; The positions of the layers within each column and row
;;;

(setf k (sort-position e #'> :slices '(0 1) :type :interior))

;;;
;;; And again the sort is done by
;;;

(setf m (order e k :slices '(0 1) :type :interior :copy? T ))

;;; Or

(setf n (sort e #'> :slices '(0 1) :type :interior :copy? T))

;;;;;;;;;;;;;;;;;;
;;;
;;;   A little more on order.  It's definition begins
;;;  (defun order (object positions 
;;;                     &key 
;;;                     (slices   :elements)
;;;                     (copy? NIL)
;;;                     (type :exterior)
;;;                     (order :row))
;;;   ..... )
;;;  Repositions the slices of the fixed-dimensions of object into an order
;;;  determined by the second argument, a list of slice positions,
;;;  in conjunction with the information
;;;  given by the keyword arguments slices, types, and order.
;;;
;;;  Positions is a list of indices of the elements or slices in the desired order.
;;;  For example, the first element in positions is the index of the slice of 
;;;  object which will be in the first position of the ordered object.
;;;  The list of positions can be interpreted in either row or column major order.
;;;  If type is :exterior, the slices are repositioned; if it is :interior, then
;;;  the elements of each slice are repositioned.
;;;
;;;
;;;
;;;
;;; And suppose we are looking at all slices of e that correspond
;;; to fixing dimensions 0 and 1 of e. It will be convenient to
;;; assign this to a variable.

(setf slices '(0 1))

;;; That is each slice will have dimensions

(ref (dimensions-of e)
     (sort (set-difference
            (iseq (number-of-dimensions e))
            slices)
           #'<))
;;;
;;; Here's an even more helpful print function that uses the Quail
;;; iteration macro doslices  ... more on that in later files.
;;;

(defun print-slices (obj slices order)
  (let ((i 0))
    (format *quail-terminal-io* "~&~% Order is ~s~%" order)
    (doslices (slice obj slices 'done order)
      (format *quail-terminal-io* "~& Slice ~s is ~s" i slice)
      (incf i))))
;;;
;;; Now we can print the slices in either row or column major order.
;;;

(print-slices e slices :row)

(print-slices e slices :column)

;;; And suppose we want to shuffle these around according to some
;;; random permutation as in

(setf pos (permute (iseq (number-of-slices e slices))))


;;; Then using the default order (row-major) we have

(print-slices e slices ::row)
(setf l (order e pos :slices slices :copy? T))
(print-slices l slices ::row)

;;; which is always the same as
 
(setf foo (order e pos :slices slices :copy? T :order :row))
(print-slices foo slices :row)

;;; But rarely the same as using a column major ordering

(setf bar (order e pos :slices slices :copy? T :order :column))
(print-slices bar slices :row)

;;; Same slices, same permutation, just depends on whether
;;; we number the original slices 0 to (- (number-of-slices e '(0 1)) 1)
;;; in row or column major order.
;;; Occasionally these will yield the same results.
;;; For an example, 

(setf pos2 '(5 4 3 2 1 0))
(setf far (order e pos2 :slices slices :copy? T :order :row))
(setf boo (order e pos2 :slices slices :copy? T :order :column))

(print-slices far slices :row)
(print-slices boo slices :row)

;;; Pen and paper exercise: Show why :row and :column must yield
;;; the same results in this case.


;;;;;;;;;;;;;;;;;;
;;;
;;;    Of course we needn't use order in conjuction with order
;;;    to permute slices.  permute has been written more generally
;;;    to permute slices randomly.
;;;
;;;    PERMUTE randomly permutes the elements or slices of object.
;;;    It has one required argument: the refable object to be permuted.
;;;    And it accepts the keywords :slices :copy? and :type
;;;    which are interpreted just as in sort.
;;;    slices --- index or list of indices identifying the fixed
;;;               dimensions of each slice.
;;;               Default :elements.
;;;    copy?  --- Destructive permutation if NIL, permutes a copy otherwise.
;;;               Default is NIL.
;;;    type   --- :exterior if slices are to be permuted randomly
;;;               :interior if the contents of slices are to be
;;;               randomly permuted.
;;;               Default is :exterior
;;;
;;;
;;;    Extending permute:  The generic function permute-object which
;;;          destructively permutes would need to be specialized for
;;;          new data structures.
;;;          It has three required args: object slices type.
;;;
;;;

a
(permute a)
a

;;;
;;; permute the columns of a copy of a
;;; 
a
(permute a :slices 1 :type :exterior :copy? T)
;;;
;;; permute the rows of a copy of a
;;; 
a
(permute a :slices 0 :type :exterior :copy? T)

;;;
;;; permute the elements of the rows of a copy of a
;;; 
a
(permute a :slices 0 :type :interior :copy? T)

;;;
;;; The following makes no pemutation because of the :interior
;;; permutation type when the elements have no structure.
;;; 

(permute a :slices :elements :type :interior :copy? T)

;;;
;;; Whereas an :exterior does permute
;;;

(permute a :slices :elements :type :exterior :copy? T)

;;;
;;;  Compare the preceding two examples
;;;  to the following case.

(setf foo (list a b))

(permute foo :slices :elements :type :exterior :copy? T)

(permute foo :slices :elements :type :interior :copy? T)

;;; Note that the interior permutation was destructive.

a

;;;
;;; permute the row-slices of a copy of e
;;; 

e
(permute e :slices '(0) :type :exterior :copy? T)

;;;
;;; permute the rows of a copy of e
;;; 

e
