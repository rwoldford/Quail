;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                          Summary statistics                           
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (C) 1995 Statistical Computing Laboratory, University Of Waterloo
;;;
;;;   
;;;  Authors:
;;;     R.W. Oldford 1995.
;;;
;;;
;;;  Contents:
;;;      1. Measures of location
;;;         1.1 Mean and sum
;;;         1.2 Median
;;;      2. Measures of spread
;;;         2.1 Range
;;;         2.2 Inter-quartile range
;;;         2.3 Standard deviation
;;;      3. Percentiles
;;;         3.1 Five number summary
;;;         3.2 Min and Max
;;;         3.3 Quartiles
;;;         3.4 Quantiles
;;;      4. Variances
;;;      5. Covariances
;;;      6. Map-slices and collapse
;;;      7. Otheriteration schemes
;;;
;;;-------------------------------------------------------------------------------

(in-package :q-user)

;;;
;;;  First some data
;;;

(<- y (random-gaussian :n 100))
(<- x (array (random-gaussian :n 90) :dimensions '(30 3)))
(<- z (array (random-gaussian :n 60) :dimensions '(3 4 5)))


;;;  \section Measures of location
;;;
;;;

;;;
;;;  \subsection  Mean and sum

(sum y)
(/ (sum y) (number-of-elements y))
(mean y)

;;;  always over the whole array

(sum x)
(mean x)



;;;
;;;  \subsection  Median

(median x)
(median y)
(median z)

;;;
;;;  \section Measures of spread
;;;
;;;    \subsection Range

(range x)
(range y)
(range z)

;;;    \subsection Inter-quartile range

(iqr x)
(iqr y)
(iqr z)

;;;    \subsection Standard deviation

(sd x)
(sd y)
(sd z)

;;; \section Percentiles
;;;
;;;   \subsection Five number summary
;;;
;;;   The five number summary for a data set is the
;;;   (min, 1st quartile, median, 3rd quartile, max)

(five-num-sum y)

;;;
;;;   \subsection Min and max

(min y)
(max y)

;;;   \subsection Quartiles
;;;
;;;  First quartile
(quartile y)
(quartile y 1)

;;;  Second quartile
(quartile y 2)
(median y)

;;;  Third quartile
(quartile y 3)

;;;  The inter-quartile range is the difference between the 3rd and 1st
;;;  quartiles.

(= (iqr y)
   (- (quartile y 3) (quartile y 1)))

;;;   \subsection Quantiles
;;;
;;;  Arbitrary quantiles can be had because a dataset can be regarded as a
;;;  an empirical distribution.

(quantile-at y .3)

;;;  Here's the five number summary

(quantile-at y '(0.0 0.25 0.5 0.75 1.0))


;;; \section Variances

(var x)
(var y)
(var z)

(- (sqrt (var z)) (sd z))

;;; \section Covariances
;;; 
;;; These return the whole variance covariance matrix and so
;;; works only on matrices.  Calling it on y or z will result in error.

(cov x)

;;; \subsection Mapping over slices of an array
;;;

;;; column means of x

(map-slices #'mean '(1) :column x)

;;; Note that since this is a two dimensional array the iteration order
;;; could be :row.  The answer will be the same.

(map-slices #'mean '(1) :row x)

;;; row means of x

(map-slices #'mean '(0) :row x)

;;; or

(map-slices #'mean '(0) :column x)

;;;
;;; Order makes a difference when we have a three way array

(map-slices #'mean '(0 1) :row z)
(map-slices #'mean '(0 1) :column z)

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

;;;  If we want the totals of all columns (that is summing over all rows)
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

(<- h (list (array  (iseq 6) :dimensions '(2 3))
            g))

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

;;;  \section Other iteration schemes
;;;

(edit-file "eg:Arrays;overview.lsp")
