;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               borel-special2.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1990 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1990, 1992.
;;;
;;;
;;;----------------------------------------------------------------------------------
;;;
;;;                   SPECIAL BOREL SET CONSTANTS, VARIABLES, AND FUNCTIONS
;;;
;;;  In this file special Borel-Set constants, varibles, and related operators are defined.
;;;
;;;  Global constants:
;;;
;;;         *the-empty-set*           ... Set of NO ELEMENTS.
;;;         *the-real-line*           ... ( -infinity, +infinity ).
;;;         *the-extended-real-line*  ... [ -infinity, +infinity ].
;;;         *the-integers*            
;;;         *the-extended-integers*
;;;         *the-naturals*           
;;;         *the-extended-naturals*
;;;         *the-rationals*           
;;;         *the-extended-rationals*
;;;
;;;  Global variables:
;;;
;;;         *default-set-less-than-predicate*
;;;         *default-union-sort*
;;;         *default-set-greater-than-predicate*
;;;
;;;  Functions:
;;;
;;;         real-linep
;;;         extended-real-linep
;;;         integersp
;;;         extended-integersp
;;;         naturalsp
;;;         extended-naturalsp



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  GLOBAL CONSTANTS
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defconstant *the-empty-set* (make-instance 'empty-set))


(defconstant *the-real-line*
  (make-instance 'interval
                 :infimum -infinity
                 :supremum +infinity
                 :closure-property :open))


(defconstant *the-extended-real-line*
  (make-instance 'interval
                 :infimum -infinity
                 :supremum +infinity
                 :closure-property :both))


(defconstant *the-integers*
  (make-instance 'countable-set
       :infimum -infinity
       :supremum +infinity
       :begin-index -infinity
       :end-index +infinity
       :closure-property :open
       :element-function
        #'(lambda (set index)
                (declare (ignore set))
                index)
       :defining-predicate
       #'integers-defining-predicate))



(defconstant *the-extended-integers*
  (make-instance 'countable-set
       :infimum -infinity
       :supremum +infinity
       :begin-index -infinity
       :end-index +infinity
       :closure-property :closed
       :element-function
        #'(lambda (set index)
                (declare (ignore set))
                index)
       :defining-predicate
       #'extended-integers-defining-predicate))



(defconstant *the-naturals*
  (make-instance 'countable-set
       :infimum 0
       :supremum +infinity
       :begin-index -infinity
       :end-index +infinity
       :closure-property :open
       :element-function
        #'(lambda (set index)
                (declare (ignore set))
                index)
       :defining-predicate
       #'naturals-defining-predicate))



(defconstant *the-extended-naturals*
  (make-instance 'countable-set
       :infimum 0
       :supremum +infinity
       :begin-index -infinity
       :end-index +infinity
       :closure-property :closed
       :element-function
        #'(lambda (set index)
                (declare (ignore set))
                index)
       :defining-predicate
       #'extended-naturals-defining-predicate))

(defconstant *the-extended-rationals*
  (make-instance 'predicate-set
       :infimum -infinity
       :supremum +infinity
       :closure-property :closed
       :defining-predicate
       #'extended-rationals-defining-predicate))

(defconstant *the-rationals*
  (make-instance 'predicate-set
       :infimum -infinity
       :supremum +infinity
       :closure-property :open
       :defining-predicate
       #'rationals-defining-predicate))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  GLOBAL VARIABLES
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar *default-set-less-than-predicate*
    #'(lambda (set1 set2)
        (<= (infimum set1) (infimum set2))))

(defvar *default-union-sort*
    #'(lambda (set1 set2)
        (<= (infimum set1) (infimum set2))))

(defvar *default-set-greater-than-predicate*
  #'(lambda (set1 set2)
      (>= (supremum set1) (supremum set2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Useful predicate tests.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun extended-real-linep (thing)
  (declare (special  *the-extended-real-line*))
  (or (same-set-p thing *the-extended-real-line*)
      (and (typep thing 'predicate-set)
           (eq (defining-predicate-of thing)
               #'extended-reals-defining-predicate)
           (= (infimum thing) -infinity)
           (= (supremum thing) +infinity)
           (eq (closure-property-of thing)
               (closure-property-of *the-extended-real-line*))
           )))


(defun real-linep (thing)
  (declare (special  *the-real-line*))
  (and
   (not (borel-set-p thing))
   (or (same-set-p thing *the-real-line*)
       (and (typep thing 'predicate-set)
            (eq (defining-predicate-of thing)
                #'reals-defining-predicate)
            (= (infimum thing) -infinity)
            (= (supremum thing) +infinity)
            (eq (closure-property-of thing)
                (closure-property-of *the-real-line*))))))


(defun extended-integersp (thing)
  (declare (special  *the-extended-integers*))
  (and
   (not (borel-set-p thing))
   (or (same-set-p thing *the-extended-integers*)
       (and (typep thing 'predicate-set)
            (eq (defining-predicate-of thing)
                #'extended-integers-defining-predicate)
            (= (infimum thing) -infinity)
            (= (supremum thing) +infinity)
            (eq (closure-property-of thing)
                (closure-property-of *the-extended-integers*))))))


(defun integersp (thing)
  (declare (special  *the-integers*))
  (and
   (not (borel-set-p thing))
   (or (same-set-p thing *the-integers*)
       (and (typep thing 'predicate-set)
            (eq (defining-predicate-of thing)
                #'integers-defining-predicate)
            (= (infimum thing) -infinity)
            (= (supremum thing) +infinity)
            (eq (closure-property-of thing)
                (closure-property-of *the-integers*))))))


(defun extended-naturalsp (thing)
  (declare (special  *the-extended-naturals*))
  (and
   (not (borel-set-p thing))
   (or (same-set-p thing *the-extended-naturals*)
       (and (typep thing 'predicate-set)
            (eq (defining-predicate-of thing)
                #'extended-integers-defining-predicate)
            (= (infimum thing) 0)
            (= (supremum thing) +infinity)
            (eq (closure-property-of thing)
                (closure-property-of *the-extended-naturals*))))))


(defun naturalsp (thing)
  (declare (special  *the-naturals*))
  (and
   (not (borel-set-p thing))
   (or (same-set-p thing *the-naturals*)
       (and (typep thing 'predicate-set)
            (eq (defining-predicate-of thing)
                #'integers-defining-predicate)
            (= (infimum thing) 0)
            (= (supremum thing) +infinity)
            (eq (closure-property-of thing)
                (closure-property-of *the-naturals*))))))

