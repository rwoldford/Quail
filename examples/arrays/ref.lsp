;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                   ref.lsp
;;;
;;;
;;;  Authors:
;;;     D.G. Anglin 1994.
;;;     R.W. Oldford 1994.

(in-package q-user)

(setf a (array '((100 101 102) (110 111 112) (120 121 122))))

(setf b (array '(28 7 32)))

(setf c (array '((10) (20) (30))))

(setf d (array '(9 8 7 6 5) :dimensions '(1 5)))

(setf e (array 7 :dimensions '(4 5 6)))

(setf days (array '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")
                  :class 'ref-array))

(setf f (array 5 :dimensions '()))   ;; ie.  :dimensions NIL

(setf g (array '(((1.1 2.2 3.3 4.4) (5.5 6.6 7.7 8.8) (9.9 11.0 12.1 13.2))
                 ((10 20 30 40) (50 60 70 80) (90 100 110 120)))
                :dimensions '(2 3 4)))

;----------------------------------------------------------------------------------

;;; ref:  referencing blocks within arrays  ... "ref by margin"

;;; the block of a where columns 0 and 2 intersect with rows 1 and 2

(setf ar1 (ref a '(1 2) '(0 2)))

ar1

(dimensions-of ar1)

;;; a VERY IMPORTANT property of objects created by ref is that they
;;; refer back to the original instance

;;; note that the (1 1) element of ar1 is the (2 2) element of a

(setf (eref ar1 1 1) 333)

ar1

a
;;; This is true even for individual elements.  Note the important
;;; distinction between ref of a single element and eref of the same.
;;;
;;; The 0 0 element of a
(eref a 0 0)
;;; A reference to the 0 0 element of a
(ref a 0 0)

;;; the block of a where all rows intersect column 2.
;;;
;;; Note three things:
;;;
;;;   1.  "all rows" can be specified by T or
;;;       by listing them all, the latter in this case by (iseq 3)
;;;
;;;   2.  if only one row, column, etc is of interest, this may be
;;;       specified by its number rather than a list containing its number
;;;       ... here we use 2 instead of '(2)
;;;
;;;   3.  axes which are of length one 1 masked from the resulting
;;;       object .. here the result has dimensions '(3) rather than '(3 1)
;;

(setf ac2 (ref a T 2))

;;; all rows in column 2
;;;
;;; Note that unprovided trailing arguments default to T
;;; We sometimes call ac2 a ``slice'' because it is a complete slice through
;;; a down its second column and along its first dimension.
;;; More generally, if the reference indices are *all* either a single number
;;; or the letter T as in ac2 or (ref g 1 1 T)
;;; the result is a sometimes called a slice.
;;;
;;; Note also that despite
;;; the fact that ar3 is conceptually a row ie 1x3, matrix-dimensions-of
;;; provides '(3 1), consistent with the convention that all 1d objects
;;; default to being columns.
;;; 

(setf ar3 (ref a 1))
;;
;;; All columns in row 1

(dimensions-of ar3)

(matrix-dimensions-of ar3)

;;; However, we can still access ar3 like it's a row

(setf (eref ar3 0 1) 17)

;;; ... a reminder ...

a

;;; nothing forces us to keep rows, columns, etc in the same order
;;; also, a list with :c can be used to take a complement ... here
;;; we want all elements on the third axis except those along 0.
;;; ie '(:c 0) == '(1 2 3) in this case.

(ref g '(1 0) '(1 2 0) '(:c 0))

;;; if we want a reference to retain its original matrix dimensions, we can
;;; use the :shape keyword

(setf ar4 (ref a 2 :shape t))

(dimensions-of ar4)

;;; can ref things which were themselves produced by ref, and result still 
;;; refers to original object

(setf ar5 (ref ac2 '(1 2)))

(setf (eref ar5 0) 38)

ar5
ac2
a

;;; although eref is flexible about treating 1d objects as rows or columns,
;;; ref is not as forgiving

(eref ac2 0 2)

(eref ac2 2 0)

(ref ac2 '(1 2) 0)    ;;; ie 1 & 2 row of a single column  --> OK

;; (ref ac2 0 '(1 2))    ;;; ie 1 & 2 column of a single row --> error !!

;;; Note the consequences of this with T

(ref ac2 t 0)         ;; all rows of a column

(ref ac2 0 t)         ;; the columns of a single row of a column .. a 0d object

;----------------------------------------------------------------------------------

;;; ref:  referencing groups of elements within arrays by index
;;;       ... "ref by indices"

;;; ref by indices is distinguished from ref by margin in that there is only
;;; one argument taken after the object to be referenced, and this argument
;;; is an array or ref-array structure (expressly NOT a list) containing
;;; as elements lists specifying an index into the object.

;;; the result has the same dimensions as the argument array, 
;;; with elements of the referenced object corresponding to the indices

;;; eg. an matrix with dimensions '(3) containing elements (0 0 1), (0 1 0),
;;;     and (1 0 0) of g

(ref g (vector '(0 0 1) '(0 1 0) '(1 0 0)))

;;; a matrix with dimensions '(2 3) containing corresponding elements of a
;;; we use the Quail function ind to build the indices array

;;; a 2x3 array with elements (0 0), etc.

(ind '(((0 0) (2 2) (1 1)) ((1 0) (2 1) (0 2))))

(setf ar6
      (ref a (ind '(((0 0) (2 2) (1 1)) ((1 0) (2 1) (0 2))))))

;;; can cheat when the object is 1d and use the index rather than a list
;;; note also that ref-arrays are legitimate indices-ref specifiers

(ref '(100 101 102 103 104) (array '((0 1) (2 3))))

;;; is the same as .. 

(ref '(100 101 102 103 104) 
     (array '(((0) (1)) ((2) (3))) :dimensions '(2 2)))

;;; we can still do ref-by-margin on an indices-ref

(ref ar6 1 '(2 0))

;----------------------------------------------------------------------------------

;;;; sel:  Copying groups of elements

;;; sel has identical syntax to ref, but the returned instance is a copy of
;;; the original elements

a

(setf as1 (sel a t '(2 0)))

(setf (eref as1 0 1) 567)

as1

a

;----------------------------------------------------------------------------------

;;;; (setf ref):  Setting groups of elements

;;;  you can set an group of elements you can reference with ref to a set
;;;  of new values by providing a new-value object with the same dimensions

(setf (ref a t 1) '(987 654 321))

;;; (setf sel) is identical to (setf ref)  ... NO difference in functionality









