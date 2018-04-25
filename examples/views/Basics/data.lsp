;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               data                             
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (C) 1998 Statistical Computing Laboratory, University Of Waterloo
;;;
;;; 
;;;  Authors:
;;;     C.B. Hurley 1998.

#|
 This file describes aspects of data for plotting.
The views system is intended to be flexible, and to plot any kind of
 data, as long as it obeys certain protocols.

 We have pre-defined protocols which work for two "natural"
 data structures: 
1. A list of lists, each sublist is a "case"
2. Arrays  See also (edit-file "q:Examples;Arrays;array.lsp")


;;================================================================================
;;---- Simple datasets 1: lists------------------------
;;================================================================================

For the first of these, we construct a list of lists.
|#

(<- cig-list 
'((14.1   0.86   .9853  13.6)
               (16.0  1.06  1.0938  16.6)
               (29.8  2.03  1.1650  23.5)
               (8.0    .67   .928   10.2)
               (4.1    .4    .9462   5.4)
               (15.0  1.04   .8885  15)
               (8.8    .76   1.0267   9)
               (12.4   .95   .9225  12.3)
               (16.6  1.12   .9372  16.3)
               (14.9  1.02   .8858  15.4)
               (13.7  1.01   .9643  13)
               (15.1   .9    .9316  14.4)
               (7.8    .57   .9705  10)
               (11.4   .78  1.124   10.2)
               (9.0    .74   .8517   9.5)
               (1.0    .13   .7851   1.5)
               (17    1.26   .9186  18.5)
               (12.8  1.08  1.0395  12.6)
               (15.8   .96   .9573  17.5)
               (4.5    .42   .9106   4.9)
               (14.5  1.01  1.007   15.9)
               (7.3    .61   .9806   8.5)
               (8.6    .69   .9693  10.6)
               (15.2  1.02   .9496  13.9)
               (12.0   .82  1.1184  14.9)))

(<- cig-data cig-list)



;; and we plot the first two variables (the first two values from each sublist as)

(scatterplot :data cig-data :x 0 :y 1)

;; If the x and y coordinates came in two separate lists, this is plotted via

(scatterplot  :x '(0 1 2 3 4) :y '(3 4 5 1 1))

;; In this case the data in the form of two lists does not obey the protocol,
;; but we allow this form of input data for convenience. The plot system
;; simply converts the two lists into a list of lists prior to plotting,
;; so the above is equivalent to

(scatterplot  :data '((0 3) (1 4) (2 5) (3 1) (4 1)) :x 0 :y 1)

;;================================================================================
;;---- Simple datasets 2: arrays ---------------------------------------------
;;================================================================================

;; In Quail, quail arrays are the basic structure. An array version of
;; cig-data is constructed via

(<- cig-data (array cig-list))

;; which is again plotted as
(scatterplot :data cig-data :x 0 :y 1)

;; If the x and y coordinates came in two separate 1d-arrays, this is plotted via


(scatterplot  :x (array '(0 1 2 3 4) ) :y (array '(3 4 5 1 1)))

;; In this case the data in the form of two arrays does not obey the protocol,
;; but we allow this form of input data for convenience. The plot system
;; simply converts the two 1d arrays into a a 2d-array  prior to plotting,
;; so the above is equivalent to

(scatterplot  :data (array '((0 3) (1 4) (2 5) (3 1) (4 1))) :x 0 :y 1)



#|
;;================================================================================
----------The plot-data interface. ----------------------------------------
;;================================================================================


The plot extracts information from data.
Typically, this information consists of coordinates extracted
from cases corresponding to one or more variates. Other information
consists of the variates and case identifiers in the dataset.

The most important elements of the plot-data interface are:

--------------------------------------------
list-cases: extract a list of cases from a dataset.

   (list-cases cig-data)
--------------------------------------------
value-of
   (setq case (car (list-cases cig-data)))
   (value-of case 0)
--------------------------------------------

dataset-name:
       gives the name of a dataset as a string, if available.
(dataset-name cig-data)

This information is printed on the title in the scatterplot.

(dataset-name case)
     gives the identifier of a case as a string, if available.

This information is printed with a left mouse click on a point symbol,
if available.

--------------------------------------------

list-variates:
      lists the variates in the dataset.

(list-variates cig-data)

The arguments given to :x and :y are usually elements of this list. 
This is used to construct the menu of choices for changing
variables (middle button on the axis or point cloud)

--------------------------------------------

We regard a case as a kind of dataset, so the functions list-cases,
dataset-name, list-variates are also defined for cases.

|#

;;================================================================================
;;---- Richer datasets : the dataset function ---------------------------------------
;;================================================================================


;; In the cigaratte data, the variables and case identifiers are
;; given 

(<- cig-vars (list "tar" "nicotine" "weight" "carbon-monoxide"))

(<- cig-names (list "Alpine" "Benson & Hedges" "Bull Durham"
                    "Camel Lights" "Carlton" "Chesterfield"
                    "Golden Lights" "Kent" "Kool" "L&M"
                    "Lark Lights" "Marlboro" "Merit"
                    "Multifilter" "Newport Lights" "Now"
                    "Old Gold" "Pall Mall Light" "Raleigh"
                    "Salem Ultra" "Tareyton" "True"
                    "Viceroy Rich Lights" "Virginia Slims" "Winston Lights"))

;; We can associate this information with the numerical cig-data via
;; the dataset function:

(dataset cig-data :identifiers cig-names :variates cig-vars
         :name "Cigarettes")

;; The dataset function does not change the structure of the cig-data,
;; it simply alters the behaviour of dataset-name, list-variates,
;; and value-of. The dataset function could be used with either the
;; array or list version of the cig-data.


(value-of (car (list-cases cig-data))  "tar")

;; Now I plot the cig-data via


(scatterplot :data cig-data :x "tar" :y "nicotine")

;;================================================================================
;;---- Richer datasets : the mway-dataset function ----------------------
;;================================================================================

;; Consider two-way anova data, where factor A has 4 levels and factor B 3 levels.

(setq response (random-gaussian :n 12))
;; We will make a two-way dataset from response.
;; This identifies the elements of response with the factor levels
;; ("A-1","B-1")   ("A-1","B-2") in order: ie the first factor varies last.

(mway-dataset  response :factors (list "A" "B") :variates (list "Y")
               :factor-levels (list 4 3)
               :name "Two-way")

;; mway-dataset differs from dataset in the way it constructs cases.
;; Here there is one case for each combination of factor levels.

(list-cases response)
(list-variates response)
(loop with v = (list-variates response)
      for c in (list-cases response)
      do (print (values-of c v)))



;; Alternatively, if response were a 2d-array:

(setq response (array (random-gaussian  :n 12) :dimensions '(4 3)))

(mway-dataset  response :factors (list "A" "B") :variates (list "Y") :name "Two-way")

(scatterplot :data response :y "Y" :x :iseq)  ;; 12 point symbols, one per case.
               
;; In some settings, one may want to regard each level of A as a dataset.
;; Such a data set can be constructed from response via the dataset-transpose function:


(setq a-cases (dataset-transpose  response "A"))
(dataset-p a-cases)
(loop with d = a-cases
      with v = (print (list-variates d))
      for c in (list-cases d)
      do (print (values-of c v)))





;;---- Richer datasets : the mlevel-dataset function ----------------------



;;---- Your own dataset ---------------------------------------------
;; This example demonstrates how to construct a plot-data interface for some other
;; data structure. 
;; Here, we use a CL hash table for the dataset, where the case identifiers are
;; the keys for the entries. Each entry represents a case, and this is also a hash
;; table.

(defun make-hash-case(vals vars id)
  (let ((h (make-hash-table  :test #'equal )))
    (setf (gethash 'identifier  h) id)
    (loop for val in vals
          for var in vars do
          (setf (gethash var h) val))
    h))

(defun make-hash-data(data vars ids did &key (save? t))
  (let ((h (make-hash-table :test #'equal)))
    (setf (gethash 'identifier  h) did)
    (setf (gethash 'not-a-case  h) t)
    (loop for d in data
          for id in ids
          for case = (make-hash-case d vars id)
          do (setf (gethash id h) case))
    (if save? (push h *datasets*))
    h))

(setq cigs-hash (make-hash-data cig-list cig-vars cig-names "Cig hash"))


;; Since hash tables are used for cases (which are also datasets)  and datasets here we
;; define the following utility function:


(defun case-hash-p(d)
  (not (gethash  'not-a-case d)))

;; We need to define plot-data interface methods for the cigs-hash data.


(defmethod dataset-p((d hash-table)) t)

(defmethod identifier-of ((d hash-table))
  (gethash 'identifier  d))
  



(defmethod list-cases ((d hash-table)) 
  (if (case-hash-p d)
    (list d)
   (loop for case being the hash-value of d
         when (typep case 'hash-table)
        collect case)))

(defmethod list-variates ((d hash-table)) 
  (if (case-hash-p d) 
    (loop for v being the hash-key of d
          unless (eq v 'identifier)
          collect v)
    (list-variates (car (list-cases d)))
    ))


(defmethod value-of ((d hash-table) v &key (default :error))
  (gethash v d default))


(defmethod make-data-subset ((d hash-table) case-list &key name save? &allow-other-keys)
  (if case-list
    (let ((h (make-hash-table :test #'equal)))
      (setf (gethash 'identifier  h) name)
      (setf (gethash 'not-a-case  h) t)
      (loop  for case in case-list
            for id = (identifier-of case)
            do (setf (gethash id h) case))
      (if save? (push h *datasets*))
      h)))

(dataset-p cigs-hash)
(list-cases cigs-hash) ;; looks ok
(list-variates cigs-hash)
(value-of (car (list-cases cigs-hash)) "nicotine") 






(scatterplot :data cigs-hash :x "tar" :y "nicotine")

