;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                        Mapping over slices
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
;;;
;;;   
;;;   In this file, we illustrate the use of the following map functions
;;;   macros for refable objects:
;;;
;;;       ... map-element     ... map a function over the elements of
;;;                               one or more ref-objects.
;;;                               Function takes as many arguments as
;;;                               there are ref-objects.
;;;       ... map-slices      ... map a function over the slices of
;;;                               one or more ref-objects
;;;                               Function takes as many arguments as
;;;                               there are ref-objects.
;;;       ... reduce-slices   ... Like the CL reduce function but operates
;;;                               across or within slices.
;;;       ... collapse        ... Collapses each slice into a single element.
;;;                               All elements are returned in an appropriately
;;;                               dimensioned object.
;;;                               Collapse is performed by applying a given
;;;                               function individually to each slice.
;;;       ... sweep           ... Sweeps from each slice the result of applying
;;;                               a function to that slice.  Sweep is done using
;;;                               a ``broom'' function which by default is #'-
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :quail-user)

;;;
;;; Some arrays to work with.
;;;


(setf a (array  (iseq 6) :dimensions '(2 3)))

(setf b (array (seq 1 24) :dimensions '(2 3 4)))  ;; 2 rows, 3 cols, 4 layers

(setf c (* a 1000))

(setf d (* b 1000))

(setf e (tp b))

(<- f (array (iseq (factorial 6)) :dimensions '(2 3 4 5 6)))

;;;
;;; And the following print function will be helpful to see what the
;;; pieces.
;;;

(defun print-slices (obj slices order)
  (let ((i 0))
    (format *quail-terminal-io* "~&~% Order is ~s~%" order)
    (doslices (slice obj slices 'done order)
      (format *quail-terminal-io* "~& Slice ~s is ~s" i slice)
      (incf i))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  map-element
;;;
;;; (map-element function order thing1 thing2 ... thingn)
;;;
;;;  Applies function elementwise to the arguments in args.
;;;  map-element returns a result whose dimensions are those of the argument
;;;  with the most elements.
;;;  Ordinarily, it returns a ref-array.
;;;  If the number of elements differs, then some elements are reused.
;;;
;;;  The principal difficulty in reusing elements is deciding on the order of
;;;  of the mapping.  For example, if we consider adding the arrays a and b
;;;  the dimensions do not match.

(dimensions-of a)
(dimensions-of b)

;;; It will be necessary to reuse some elements of a to complete the calculation.
;;; But how?
;;; Because a matches b on the first two dimensions of b, it might make sense
;;; to add a to every slice of b indexed by b's last dimension.
;;; That is the calculation should vary b's first two indices fastest and its' last
;;; index most slowly.  Similarly a's indices will follow suit. 
;;; This will be column major order and the resulting calculation is
;;; completed as follows

(map-element #'+ :column a b)

;;; And we note that this has the desired effect.  The first (0'th) 2 by 3 slice
;;; of the result is

(ref (map-element #'+ :column a b) T T 0)

;;; which is numerically equal to adding a directly to the 0'th slice of b

(+ a (ref b T T 0))

;;;
;;; We might have wished to force the opposite ordering, row-major ordering
;;; where the last indices of all arguments vary most rapidly.  Then the call
;;; would be 

(map-element #'+ :row a b)

;;; which uses the elements of a in a strange order with b.
;;; Where this makes more sense is when the obvious ordering is ambiguous
;;; as would be the case if a was to be added to a 4d array having dimensions
;;; (2 3 2 3).  Then either row or column major ordering would make sense.
;;;
;;; Because the obvious ordering can often be determined (particularly for
;;; 2 and fewer dimensional arrays), map-element will accept the value NIL
;;; for the order argument.  In this case the order is computed from the
;;; dimensions of the arguments.  if an ``obvious'' order is not determinable,
;;; then an error results. The order is computed via the function ``compute-order''
;;; which takes the arguments thing1, thing2, ... in the order they were
;;; presented to map-element. So we have

(compute-order a b)

;;; and

(compute-order b a)

;;; producing the desired column ordering.  
;;; And so if given a NIL argument in this case, map-element will do the
;;; right thing.

(map-element #'+ NIL a b)

;;; But there is no obviously usable ordering for b and the transpose of a,
;;; so

(compute-order (tp a) b)

;;; returns NIL and map-element with NIL order would produce an error in this
;;; case.
;;;
;;; In the ambiguous case where either order is a possibility, we somewhat
;;; arbitrarily choose column major order as the default.

(<- 4d (array (iseq 36) :dimensions '(2 3 2 3)))

(compute-order a 4d)

;;;
;;; The last thing to note is that the dimensions of the returned result is
;;; determined by those of the argument having the most elements, not by
;;; the object with the most dimensions.  So

(map-element #'+ :row a (iseq 10))

;;; returns a 1 dimensional object having 10 elements.

;;;
;;;  Here are a few more examples of using map-element.
;;;

(map-element #'+ :row '(0 1 2 3 4) '(4 3 2 1 0))

(map-element #'+ :row a 100)

(map-element #'+ :row a 100 1000)

(map-element #'+ :row a '(100 1000))

(map-element #'+ :row a '(100 1000 10000))

(map-element #'+ :column a '(100 1000))

(map-element #'+ :column a '(100 1000 10000))

(map-element #'+ :column a '(100 1000) 10000)

(map-element #'(lambda (x y)
                 (mean (list x y)))
             :column a '(100 1000))

(map-element #'+ :row c b)

(map-element #'+ :column c b)

;;;
;;;  Map-element is particularly handy if you want to extend a function
;;;  that operates only on scalars to one that operates on arrays.
;;;

(defun scalar-cube-root (x)
  "Takes the cube-root of the argument x."
  (if (numberp x)
    ;; then use the base common-lisp function expt
    (CL:expt x 1/3)
    (quail-error "~&Scalar-cube-root: ~s is not a number!" x)))

;;;
;;; So this works
;;;

(scalar-cube-root 8)

;;;
;;; But this fails
;;;

(scalar-cube-root a)

;;;
;;;  To build a function that takes cube-roots of elements
;;;  of arrays use map-element.
;;;

(defun cube-root (x)
  "Takes the cube-root of the argument x."
  (map-element #'scalar-cube-root :row x))

;;;
;;; And so we have
;;;

(cube-root a)

;;; and

(cube-root (list 1 2 3 4 5 6))

;;;
;;;  But failure on
;;;

(cube-root "abcdefg")

;;;
;;;  For arbitrary number of arguments
;;;

(defun scalar+ (x &rest args)
  "Adds scalars together."
  (unless (numberp x)
    (quail-error "~&Scalar+: ~s is not a number!" x))
  (if args
    ;; then calculate the result
    (let ((result x))
      (loop
        for y in args
        do
        (unless (numberp y)
          (quail-error "~&Scalar+: ~s is not a number!" y))
        (setf result (CL:+ result y))
        )
      result)
    ;; else just return x
    x))

;;;
;;; These work

(scalar+ 1)
(scalar+ 1 2 3 4)

;;; And this fails

(scalar+ a 1000)

;;;
;;; Define a version for refable objects
;;;

(defun my+ (x &rest args)
  "Adds the elements of its arguments in row-major order. ~
   This is important when dimensions don't match"
  (apply #'map-element #'scalar+ :row x args))

(my+ a 100)
(my+ a 100 1000)

(my+ a '(1000 2000))


  
;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;;  map-slices
;;;
;;;  This provides much of the functionality available with
;;;  map-element but for slices.
;;;
;;;  NOTE that it DOES NOT try to be clever about reusing slices.
;;;  If the arguments do not all have the same number of slices to map over,
;;;  then the minimum number of slices are operated on.
;;;  However, the slices argument must be sensible for ALL arguments.
;;;
;;;  NOTE also that there is no attempt to construct an appropriate
;;;  dimensioned ref'able thing as a result.
;;;  Instead a list of the results is returned.
;;;
;;;  Syntax:
;;;  (map-slices fun slices order ref-object ref-obj2 ref-obj3 ...)
;;;  
;;;  At least one ref-object must be passed to map-slices.
;;;

;;; With slices = :elements this is kind of like map-element but not nearly
;;; as clever.

(map-slices #'- :elements :row a)
(map-slices #'- :elements :column a)

(map-slices #'+ :elements :row a 1000)
(map-slices #'+ :elements :row a '(1000 10000))
(map-slices #'+ :elements :column a '(1000 10000))

(map-slices #'+ :elements :row a c)
(map-slices #'+ :elements :column a c)

(setf slices '(1))
(map-slices #'+ slices :row a c)
(map-slices #'+ slices :column a c)

(setf slices '(0))
(map-slices #'+ slices :row a c)
(map-slices #'+ slices :column a c)

(setf slices '(0 1))
(map-slices #'+ slices :row a c)
(map-slices #'+ slices :column a c)

(setf slices '(1))
(number-of-slices b slices)
(number-of-slices d slices)
(map-slices #'+ slices :row b d)
(map-slices #'+ slices :column b d)

(setf slices '(0))
(number-of-slices b slices)
(number-of-slices d slices)
(map-slices #'+ slices :row b d)
(map-slices #'+ slices :column b d)

(setf slices '(0 1))
(number-of-slices b slices)
(number-of-slices d slices)
(map-slices #'+ slices :row b d)
(map-slices #'+ slices :column b d)

(setf slices '(0 1))
(dimensions-of b)
(number-of-slices b slices)
(dimensions-of e)
(number-of-slices e slices)
(map-slices #'(lambda (x y)
                (> (mean x) (mean y)))
            slices :row b e)
(map-slices #'(lambda (x y)
                (> (mean x) (mean y)))
            slices :column b e)


;;;;;;;;;;;
;;;
;;;  reduce-slices
;;;
;;;  Like the reduce function this applies a function of two arguments
;;;  over the elements.
;;;  It takes two required arguments:
;;;    ... the function to be used to combine slices
;;;    ... and the refable object over which slicing is to be performed.
;;;
;;; Keywords:            Default
;;;
;;;    :order            :row            ... The order of slicing.
;;;    :initial-value     not-supplied   ... First value given to function
;;;                                          If it's supplied.
;;;    :slices           :elements            ... The keyword :elements or
;;;                                          a list of dimension numbers
;;;                                          that determine the slices
;;;                                          to iterate over.
;;;    :type             :exterior       ... Determines whether the function
;;;                                          is to be applied to pairs of
;;;                                          slices (:exterior) or to pairs
;;;                                          of elements within slices
;;;                                          (:interior).
;;;    :list?            NIL             ... Logical flag to determine
;;;                                          the kind of structure to be
;;;                                          returned by an :interior type
;;;                                          reduction.
;;;                                          Because there are as many results
;;;                                          as there are slices in this case,
;;;                                          we can return them in a structure
;;;                                          having the same dimensions as a
;;;                                          single slice.
;;;                                          This is what happens when list?
;;;                                          is non-NIL.
;;;                                          Unfortunately, this might not 
;;;                                          always be desired, as perhaps 
;;;                                          when the function returns some
;;;                                          very complicated structure.  
;;;                                          Then we might rather not
;;;                                          have these complicated structures 
;;;                                          stored away as the elements of 
;;;                                          a matrix, say.  
;;;                                          Reduce-slices therefore gives 
;;;                                          the option of returning them in
;;;                                          a list when list? is non-NIL.
;;;                                          
;;;
;;;
;;;
;;;
;;;     Some Examples
;;;
;;;  First we sum all elements in b
;;;  The slices are

(print-slices b :elements :row)

;;;
;;; and they sum to

(reduce-slices #'+ b )

;;;
;;; Next we sum over those slices that are defined by fixing the
;;; first dimension in b, dimension 0
;;; The slices are

(print-slices b '(0) :row)
;;; and they sum to

(reduce-slices  #'+ b :slices '(0))
;;;
;;; Next we sum over the layers of b, that is over those
;;; slices that are defined by fixing the last dimension, 2, of b.
;;; The slices are

(print-slices b '(2) :row)
;;;
;;; and they sum to

(reduce-slices  #'+ b :slices '(2))
;;;
;;; And finally we sum over all columns of b, that is over those
;;; slices that are defined by fixing the last two dimensions so that
;;; only the rows are changing within a slice
;;; The slices are

(print-slices b '(1 2) :row)
;;;
;;; and they sum to

(reduce-slices  #'+ b :slices '(1 2))

;;;
;;;
;;;  Note that it didn't matter whether we went row major or column major
;;;  in these examples because the + function is associative (+ a b c)
;;;  = (+ (+ a b) c) = (+ a (+ b c))
;;;
;;;  But for some functions this is not the case.

(print-slices b '(1 2) :row)
;;;
;;; and they reduce to

(reduce-slices  #'(lambda (x y) (mean (list (mean x) (mean y))))
                b :slices '(1 2))

;;;
;;; Compare this to the following
;;;

(print-slices b '(1 2) :column)
;;;
;;; and they reduce to

(reduce-slices  #'(lambda (x y) (mean (list (mean x) (mean y))))
                b :slices '(1 2) :order :column)

;;;
;;;  We might also like to give reduce-slices a starting value
;;;  as in


(reduce-slices  #'+ b :slices '(1 2) :initial-value 10000)

;;; 
;;;  These addition functions worked because in Quail + will work with
;;;  array arguments as well as numbers.
;;;  If it did not, the kind of collapsing over dimensions of b
;;;  which we illustrated above could be accomplished by using an interior
;;;  reduction.
;;;
;;;  For example, suppose we would like the 2 by 3 table that results
;;;  from summing over the last dimension of b.
;;;  This could be had as follows:

(reduce-slices #'CL:+ b :slices '(0 1) :type :interior)

;;;
;;;  which because + works on arrays in Quail, is the same as
;;;

(reduce-slices #'+ b :slices '(2) :type :exterior)

;;;
;;;  Instead, collecting together the results in a list we have
;;;

(reduce-slices #'CL:+ b :slices '(0 1) :type :interior :list? T)

;;;
;;;  Note also that some :exterior reductions are the same as :interior
;;;  reductions on other slices.
;;;  For example, consider the multi-way array f

(dimensions-of f)

;;;  And suppose we would like to sum over the contents of every slice
;;;  of f formed from having one index along dimension 1
;;;  and the other along dimension 3.
;;;  This would be an :interior reduction and should return an
     (eref (dimensions-of f) 1)
;;;  by
     (eref (dimensions-of f) 3)
;;;  array whose i, j element is the sum of the contents of the corresponding
;;;  i, j sub-array of f.   So for i=j= 0 this would be the
;;;  sum of the contents of

 (ref f T 0 T 0 T)

;;; i.e.

(sum (ref f T 0 T 0 T))

;;;  which had better be the (0 0) element of

(reduce-slices #'+ f :slices '(1 3) :type :interior)


;;;
;;;  Another way to think of this is as a summation over the slices of
;;;  f that are defined by indices fixed along the 0, 2, and 4 dimension.
;;;  That is just sum all the (eref (dimensions-of f) 1) by
;;;  (eref (dimensions-of f) 3) slices of f.
;;;  This is an :exterior reduction as in 

(reduce-slices #'+ f :slices '(0 2 4) :type :exterior)

;;;  For operations that ultimately work element-wise on the slices,
;;;  like +, - , etc. it will be faster to do the reduction as an :interior
;;;  reduction than as an :exterior one.
;;;  This is especially true where the operator for two exterior slices may
;;;  incur more overhead than an equivalent operator for elements within a slice
;;;   E.g. + versus CL:+
;;;  In any case, a timing should be done before committing to one over the
;;;  other. 

(time (reduce-slices #'+ f :slices '(0 2 4)))

;;;  versus

(time (reduce-slices #'+ f :slices '(1 3) :type :interior))

;;;  or

(time (reduce-slices #'CL::+ f :slices '(1 3) :type :interior))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  
;;;  Collapse.
;;;  Syntax: (collapse fun ref-object &key (slices NIL) (order  :row))
;;;
;;;  Calls the function fun on each slice of ref-object having indices 
;;;  in the dimensions identified by the list slices.  
;;;  Collapse tries to return an object like ref-object but having fewer dimensions. 
;;;  The dimensions of the returned object will match those of ref-object as identified 
;;;  by slices.  For example if ref-object is a 2 by 3 by 4 by 5  array and slices is (0 1), 
;;;  then the returned object will be a 2 by 3 array.  
;;;  Each element of the returned object will be the result of calling fun on 
;;;  the corresponding slice.  In our example, the 0 0 element of the returned object 
;;;  will be the result of calling fun on (ref ref-object 0 0 T T).  
;;;  The elements of the returned object are filled in the same order that the slices 
;;;  of ref-object are traversed: either :row or :column major order.
;;;
;;;
;;;  A statistical example:
;;;  Multi-way arrays are often used as contingency
;;;  tables to store counts in each cell.
;;;  A common  thing to do is to collapse a table by summing the elements
;;;  over one of its dimensions (or margins).

(setf g (array (iseq 1 24) :dimensions '(2 3 4)))

;;;  In statistics this might represent the cross-classification of
;;;  three variables: the first having two possible outcomes
;;;  the second three and the third four.
;;;  Each cell of the table represents the number of individuals observed
;;;  to have that combination of values for the three variables.
;;;
;;;  The total number of individuals then is

(sum g)

;;; or equivalently

(collapse #'sum g :slices NIL)

;;;  If we want the 2 by 3 table of totals over the 4 categories of variable 3,
;;;  then we use

(collapse #'sum g :slices '(0 1))

;;;  If we want the totals of all columns (that is summing over all rows
;;;

(collapse #'sum g :slices '(1 2))

;;;  If we want to look at the observed marginal distribution for the first
;;;  variable

(collapse #'sum g :slices 0)

;;;
;;;  For many applications we will want other summary statistics.
;;;
;;;  Mean number of individuals in each category of variable 3
;;;  cross-classified according to the first two variables

(collapse #'mean g :slices '(0 1))

;;;  Median for same

(collapse #'median g :slices '(0 1))

;;;  Standard deviation for same

(collapse #'sd g :slices '(0 1))

;;;
;;;
;;;  Collapse can also be applied directly to the elements of an array
;;;  As in

(collapse #'sum g :slices :elements)

;;;  Which essentially returns a copy of g
;;;
;;;  or

(collapse #'(lambda (x) (if (> x 12) 'big 'small))
          g :slices :elements)

;;;
;;;  This is most useful when each element is a more complicated structure
;;;  as in 

(<- h (list a g))

;;;  then the list of table totals is produced by

(collapse #'sum h :slices :elements)

;;;
;;;  Or more interestingly, we might have four samples of different sizes
;;;

(<- x1 (random-gaussian :n 5 :location 10 :scale 1))
(<- x2 (random-gaussian :n 4 :location 20 :scale 2))
(<- x3 (random-gaussian :n 10 :location 30 :scale 3))
(<- x4 (random-gaussian :n 6 :location 40 :scale 4))

;;;
;;; cross-classified by two variables having two categories each
;;; as in

(<- y (array (list x1 x2 x3 x4) :dimensions '(2 2)))

;;;
;;;  The table of sample means is
;;;  

(collapse #'mean y :slices :elements)

;;;
;;;  The table of standard deviations is
;;;  

(collapse #'sd y :slices :elements)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                          sweep
;;;
;;;   Syntax: (sweep fun ref-object
;;;                  :slices NIL
;;;                  :order :row
;;;                  :copy? NIL
;;;                  :broom #'- )
;;;
;;;  Sweeps the result of applying fun from each slice of ref-object using
;;;  the function broom. 
;;;  This is done by calling the function fun on each slice to get a result
;;;  for that slice.  Then that slice is replaced by the result of
;;;  calling broom on slice and result. an on subtracting the
;;;  the result from that slice.
;;;  Returns the swept ref-object, or if copy? is non-NIL the swept copy of
;;;  ref-object and a list of the values swept from each slice.
;;;
;;;
;;;  For example sweep will remove the mean from a matrix as follows.
;;;  First recall the matrix a:

a
(dimensions-of a)
(mean a)
(sweep #'mean a :copy? T)
a

;;;
;;;  Now let's remove the row means from a
;;;

(sweep #'mean a :slices 0 :copy? T)

;;;
;;;  Or the column means from a
;;;

(sweep #'mean a :slices 1 :copy? T)

;;;
;;;  And this will work as expected on more complicated arrays
;;;  

g
(dimensions-of g)
(mean g)
(sweep #'mean g :copy? T)

;;;
;;;  Now let's remove the row means from g
;;;  (where a row is defined within layers)
;;;
(print-slices g  '(0 2) :row)
(doslices  (slice g '(0 2) NIL :row)
  (print (- slice (mean slice))))

;;;
;;; compared to
(setf ans 
      (sweep #'mean g :slices '(0 2) :copy? T))
(print-slices ans  '(0 2) :row)

;;;
;;;  To get both values we need only use something like
;;;

(multiple-value-setq (swept row-means)
           (sweep #'mean g :slices '(0 2) :copy? T))
swept
row-means

;;;
;;;  or to return them in a list use
;;;  

(setf answer
      (multiple-value-list   (sweep #'mean g :slices '(0 2) :copy? T)))

;;;
;;;  Local binding can use multiple-value-bind 
;;;
;;;
;;;  The broom can be an arbitrary function of two arguments.
;;;

(sweep #'mean g :slices '(0 2) :copy? T :broom #'mod)

;;;
;;;  The following illustrates the multiple-value-bind and
;;;  shows the results of the sweep more clearly.
;;;

(multiple-value-bind
  ;; local program like let with variables as named in the 
  ;; next list
  (ans swept-values)
  ;; this form returns the values for the local variables
  ;; in multiple-value form
  (sweep #'mean g :slices '(0 2) :order :row :copy? T :broom #'mod)
  ;; first form of 7 to be processed as the body of the multiple-value-bind
  (format *quail-terminal-io* "~&Before sweep")
  ;; Original slices
  (print-slices g '(0 2) :row)
  ;; Now the swept slices
  (format *quail-terminal-io* "~&After sweep")
  (print-slices ans '(0 2) :row)
  ;; Now the moduli that were used by the broom #'mod in the sweeping
  ;; from each slice.
  (format *quail-terminal-io* "~&~%~%Moduli used were")
  (print-slices swept-values :elements :row)
  ;; A last silly thing to return, the value of (quote done)
  'done)

;;;  
