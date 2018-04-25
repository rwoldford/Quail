;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               trellis                              
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (C) 1994 Statistical Computing Laboratory, University Of Waterloo
;;;
;;; 
;;;  Authors:
;;;     C.B. Hurley 
;;;
;;;




(in-package :quail-user)

#|
Datafile Name: Protein
Datafile Subjects: Europe , Health , Nutrition
Story Names: Protein Consumption in Europe
Reference: Weber, A. (1973) Agrarpolitik im Spannungsfeld der
internationalen Ernaehrungspolitik, Institut fuer Agrarpolitik und
marktlehre, Kiel. Also found in: Gabriel, K.R. (1981) Biplot display of
multivariate matrices for inspection of data and diagnosis. In Interpreting
Multivariate Data (Ed. V. Barnett), New York: John Wiley & Sons, 147-173.
Hand, D.J., et al. (1994) A Handbook of Small Data Sets, London: Chapman &
Hall, 297-298.
Authorization: Free Use
Description: These data measure protein consumption in twenty-five European
countries for nine food groups. It is possible to use multivariate methods
to determine whether there are groupings of countries and whether meat
consumption is related to that of other foods.
Number of cases: 25
Variable Names:

  1. Country: Country name
  2. RdMeat: Red meat
  3. WhMeat: White meat
  4. Eggs: Eggs
  5. Milk: Milk
  6. Fish: Fish
  7. Cereal: Cereals
  8. Starch: Starchy foods
  9. Nuts: Pulses, nuts, and oil-seeds
 10. Fr&Veg: Fruits and vegetables
|#


(<- euro  (array '( 
Albania 10.1    1.4     0.5     8.9     0.2     42.3    0.6     5.5     1.7
Austria 8.9     14.0    4.3     19.9    2.1     28.0    3.6     1.3     4.3
Belgium 13.5    9.3     4.1     17.5    4.5     26.6    5.7     2.1     4.0
Bulgaria        7.8     6.0     1.6     8.3     1.2     56.7    1.1     3.7     4.2
Czechoslovakia  9.7     11.4    2.8     12.5    2.0     34.3    5.0     1.1     4.0
Denmark 10.6    10.8    3.7     25.0    9.9     21.9    4.8     0.7     2.4
EGermany       8.4     11.6    3.7     11.1    5.4     24.6    6.5     0.8     3.6
Finland 9.5     4.9     2.7     33.7    5.8     26.3    5.1     1.0     1.4
France  18.0    9.9     3.3     19.5    5.7     28.1    4.8     2.4     6.5
Greece  10.2    3.0     2.8     17.6    5.9     41.7    2.2     7.8     6.5
Hungary 5.3     12.4    2.9     9.7     0.3     40.1    4.0     5.4     4.2
Ireland 13.9    10.0    4.7     25.8    2.2     24.0    6.2     1.6     2.9
Italy   9.0     5.1     2.9     13.7    3.4     36.8    2.1     4.3     6.7
Netherlands     9.5     13.6    3.6     23.4    2.5     22.4    4.2     1.8     3.7
Norway  9.4     4.7     2.7     23.3    9.7     23.0    4.6     1.6     2.7
Poland  6.9     10.2    2.7     19.3    3.0     36.1    5.9     2.0     6.6
Portugal        6.2     3.7     1.1     4.9     14.2    27.0    5.9     4.7     7.9
Romania 6.2     6.3     1.5     11.1    1.0     49.6    3.1     5.3     2.8
Spain   7.1     3.4     3.1     8.6     7.0     29.2    5.7     5.9     7.2
Sweden  9.9     7.8     3.5     24.7    7.5     19.5    3.7     1.4     2.0
Switzerland     13.1    10.1    3.1     23.8    2.3     25.6    2.8     2.4     4.9
UK      17.4    5.7     4.7     20.6    4.3     24.3    4.7     3.4     3.3
USSR    9.3     4.6     2.1     16.6    3.0     43.6    6.4     3.4     2.9
WGermany       11.4    12.5    4.1     18.8    3.4     18.6    5.2     1.5     3.8
Yugoslavia      4.4     5.0     1.2     9.5     0.6     55.9    3.0     5.7     3.2
) :dimensions '(25 10)))



(<- food-names '(RedMeat WhiteMeat       Eggs    Milk    Fish    Cereals Starch  Nuts    Fr&Veg))
(<- mdata (ref euro t (iseq 1 9)))
(<- country-names (loop for i from 0 to 24 collect (eref euro i 0)))




#|  This reorders the countries and uses only a subset...
(<- food-names '(RedMeat WhiteMeat       Eggs    Milk    Fish    Cereals Starch  Nuts    Fr&Veg))
(<- all-country-names (loop for i from 0 to 24 collect (eref euro i 0)))
(<- country-names '(BELGIUM DENMARK FRANCE IRELAND ITALY NETHERLANDS NORWAY SPAIN Portugal SWEDEN UK WGERMANY))
(<- mdata (ref euro (loop for c in country-names
                          collect (position c all-country-names)) (iseq 1 9)))
|#


(<- mdata (mway-dataset mdata :factors (list "Country" "Food Group"  )
                             :variates (list "protein")
                             :factor-levels (list country-names food-names)
                    :name "Protein data"))







;; There are many ways to make trellis-type plots.
;; ----VERSION 1. --------------------------------------------------------
;; This first attempt looks nice but is missing country labels.
;; Each panel in the plot shows a scatterplot corresponding to a single Food Group.
;; The datasets (one per food group) are constructed by applying the
;; function (data-subsets-fn  "Food Group")   to mdata

(setq b
      (batch-plot :data mdata :by "Food Group" :x "protein" :x-function #'log :y :iseq :ncols 3
                  :subview-type '(:type scatterplot
                                  :no-labels? t :no-margin-views? t
                                  :lines-to :left)
                   :no-labels? t :no-margin-views? t
                  ))

;; ----VERSION 2: add labels --------------------------------------------------------
;; To add labels, make the left margin view a 1d-point-cloud. These point clouds
;; use a "data-label" instead of a point-symbol. (A data-label is just a linkable-label
;; for use in a plot). They automtically get data information
;; (cases and var) from the adjacent scatterplot.
;; This looks right....
;; But, if b is linked, the country labels are linked only to the point symbols
;; in the adjacent scatterplot.
(setq b
      (batch-plot :data mdata :by "Food Group" :x "protein" :x-function #'log :y :iseq :ncols 3
                  :subview-type '(:type scatterplot :margins :none :title label
                                  :lines-to :left)
                  :left-view `(:type 1d-point-cloud :labels "Country"
                                     :case-view (:type data-label :font ,wb:*very-small-graphics-font*
                                                       :justification :right
                                                        )) 
                  :no-labels? t :top-view nil
                  :right-view nil
                  ))

;; ----VERSION 3: link the country labels --------------------------------------------------------
 
;; Ideally, clicking on a country label should show all values for that country.
;; We will first construct a point cloud to place in the left margin..
;; Using :cases of (:by "Country") constructs one dataset for each country. These datasets
;; are the cases, that is, the viewed object of each case-view is a country dataset.
;; The case-views are group-labels, which automatically pick up the country name as its
;; text. A group-label differs from a data-label in that it will "open up" its viewed object
;; into constituent cases prior to linking. 

(setq p (1d-point-cloud :data mdata :var :iseq :orientation :vertical
         :cases '(:by "Country")
          :case-view `(:type group-label
                             :font ,wb:*very-small-graphics-font*
                             :justification :right)
         ))


(setq b
      (batch-plot :data mdata :by "Food Group" :x "protein" :x-function #'log :y :iseq :ncols 3
                  :subview-type '(:type scatterplot :margins :none
                                  :title label
                                  :lines-to :left-right)
                  :left-view (list p (copy-view p ) (copy-view p ))
                  :no-labels? t :top-view nil
                  :right-view nil
                  ))
;; We use copies of p for the three sets of left margin labels. That is, they
;; are three different point clouds, which use the same (eq) case-views.

;; Now, when view b is linked, each country label is linked to nine point symbols.
;; This occurs because, for group labels, linking is done on the basis of the
;; constituent cases of the viewed objects, rather than considering the viewed object
;; as a single entity.
;; By contrast, the food group labels are not linked, because they are of type label,
;; which by default are not linkable.



;; ----VERSION 4: link both country labels and food group labels -----------------

;; If we want the food group labels and the country labels to have the same behaviour
;; then they should have the same type:eg change food-group labels to be of type group-label



      

(setq b
      (batch-plot :data mdata :by "Food Group" :x "protein" :x-function #'log :y :iseq :ncols 3
                  :subview-type '(:type scatterplot :margins :none
                                  :title group-label
                                  :lines-to :left-right)
                  :left-view (list p (copy-view p ) (copy-view p ))
                  :no-labels? t :top-view nil
                  :right-view nil
                  ))

;; Notice now each country label is linked to each food group label, because each pair 
;; has a case in common.

;; ----VERSION 5: no interlabel linking -----------------

;; Alternatively, all labels could be ordinary data labels, in which
;; case they will have no links (because data-labels, while linkable,
;; do not open up the viewed object before calculating links.)
;; Howevever, one can alter this linking behaviour by using a different link
;; test, notably contains-data-p:

(setq p (1d-point-cloud :data mdata :var :iseq :orientation :vertical
         :cases '(:by "Country")
          :case-view `(:type data-label
                             :font ,wb:*very-small-graphics-font*
                             :justification :right)
         ))


(setq b
      (batch-plot :data mdata :by "Food Group" :x "protein" :x-function #'log :y :iseq :ncols 3
                  :subview-type '(:type scatterplot :margins :none
                                  :title data-label
                                  :lines-to :left-right)
                  :left-view (list p (copy-view p ) (copy-view p ))
                  :no-labels? t :top-view nil
                  :right-view nil
                  ))

(setq contains-lt (make-link-table :test #'contains-data-p :name "Contains"))


(link-view  b :link-table contains-lt)

;; ----VERSION 6: reorder categories -----------------

;; the computation -------------
;; Finally, to order countries and food groups by their average value:

(setq fg-order (loop for x in (funcall (data-subsets-fn "Food Group") mdata )
                     for i upfrom 0
                collect
               (list 
                (apply #'+ (mapcar #'log (values-of  (list-cases x) "protein")))
                  ;; (median  (values-of  (list-cases x) "protein"))
               (identifier-of x )
               i)))
(setq fg-order (sort  fg-order #'< :key #'car))

(defun fg-order-fn(i)
  (position i fg-order :key #'third :test #'=))
  

(setq c-order (loop for x in (funcall (data-subsets-fn "Country") mdata ) 
                  for i upfrom 0
                collect
               (list 
                   (apply #'+ (mapcar #'log (values-of  (list-cases x) "protein")))
                    ;;(median (values-of  (list-cases x) "protein"))
               (identifier-of x )
               i)))
(setq c-order (sort  c-order #'< :key #'car))

(defun c-order-fn(i)
  (position i c-order :key #'third :test #'=))
;; the plots -------------

(setq p (1d-point-cloud :data mdata :var :iseq :function #'c-order-fn :orientation :vertical
         :cases '(:by "Country")
          :case-view `(:type data-label
                             :font ,wb:*very-small-graphics-font*
                             :justification :right)
        ))


(setq b
      (batch-plot :data mdata :by "Food Group" :order-levels (reverse (mapcar #'second fg-order))
                  :x "protein" :x-function #'log 
                  :y :iseq :y-function #'c-order-fn :ncols 3
                  :subview-type '(:type scatterplot :margins :none
                                  :title data-label
                                  :lines-to :left-right)
                  :left-view (list p (copy-view p ) (copy-view p ))
                  :no-labels? t :top-view nil
                  :right-view nil
                  ))


;; ----VERSION 7:  Cases or datasets?-------------------------------
#|
 A dataset is an object from which one can extract cases, via "list-cases".
 A case is an object from which one can extract values for variates.
 Therefore, a case is a kind of dataset.
 A d-view is a view like a point-cloud or histogram-view which displays information about  a list
 of objects which are typically cases . A d-view obtains one or more variate  values 
 from each of these `cases'. However, in some situations
 it is convenient to use :iseq in place of a variate, in which case the values are the
 integers 0,1,2 .... , and the integer i is used instead of a variate value for 
 the ith case (starting at 0).

 In the construction of p above, the 1d-point-cloud has a case list which is actually a 
 list of datasets, rather than cases.
 This worked because :iseq was used in place of a variates, and so there was no need
to obtain data values from the datasets.
One could adopt a convention that the viewed object of a point symbol 
in a point cloud should be a case, and it would seem sensible that
this convention should be followed when labels are used in place of
point symbols  in the point cloud.

So, this version of p is constructed as below.
This does not affect linking behaviour when contains-data-p
is used as the link-test, because for example
(contains-data-p france-case france.cereal-case) is true.

|#
(setq p (1d-point-cloud :data (dataset-transpose mdata "Country")
         :var :iseq :function #'c-order-fn :orientation :vertical
          :case-view `(:type data-label
                             :font ,wb:*very-small-graphics-font*
                             :justification :right)
        ))

;; -----------------------------------------------------------

