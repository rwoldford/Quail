(defun foo (list predicate key)
  (let ((indexed-list
         (loop for i from 0 by 1
               as l in list
               collect (cons i l))))
    (mapcar #'car
            (cl:sort indexed-list predicate 
                            :key (if key #'(lambda (x) (funcall key (cdr x)))
                                     #'cdr)))))

fixed-dimensions ---> slice


(rank '((6 5) (4 3) (2 1)) #'< )  ===> ((5 4) (3 2) (1 0))

(rank '((6 5) (4 3) (2 1)) #'(lambda (x y) (< (sum x) (sum y)))
      :slice 0)
        ==========>>>>>>>>>  (2 1 0)

(rank '((6 5) (4 3) (2 1)) #'(lambda (x y) (< (sum x) (sum y)))
      :slice 1)
        ==========>>>>>>>>>  (1 0)

(order '((6 5) (4 3) (2 1)) :order '(2 1 0 5 4 3))
         =========>>>>>>>>>>>>  '((4 5) (6 1) (2 3))
(order '((6 5) (4 3) (2 1)) :order '(2 1 0) :slice '(0))
      =========>>>>>>>> '((2 1) (4 3) (6 5))

(defun rank (object predicate 
                    &key 
                    (key      nil)
                    (fixed-dimensions   nil)
                    (type     :interior)
                    (store-in NIL))
  
  "Ranks the sub-arrays of fixed-dimensions of ref-object into an order  ~
   determined by predicate.  The arguments predicate and key are to be interpreted ~
   exactly as the same named arguments in the Common Lisp sort function.  ~
   The argument stable? is NIL (the default) or non-NIL depending on whether ~
   it is desirable that stability be guaranteed in the sense of the Common Lisp function ~
   stable-sort.  ~
   
   These types of sort are possible: ~  

       1. If the arguments permit, then sort follows the Common Lisp standard. ~
   
       2. If object is a ref-object and type is :exterior, then, for example, ~
   (sort A #'< :key #'sum :fixed-dimensions 1 :type :exterior) rearranges the  ~
   columns (fixed-dimensions 1) ~
   of the matrix A ~
   so that the column with the greatest total is the 0-th column, and so forth. ~
   
       3. If object is a ref-object and type is :interior, then, for example, ~
   (sort A #'< :fixed-dimensions 1 :type :interior) rearranges each column  ~
   (fixed-dimensions 1) of the matrix A ~
   so that the greatest element is in the first row, and so forth. ~
   
       4. In all of the above, destructive sorts are preformed if copy? is nil  ~
   (the default). ~
   If copy? is t, then a non-destructive sort is performed." 
  
  
  (if (not (listp fixed-dimensions))
    (setf fixed-dimensions (list fixed-dimensions))
    (setf fixed-dimensions (cl:sort (remove-duplicates fixed-dimensions) #'<)))
  
  (rank-object object
               predicate
               key
               fixed-dimensions
               type
               store-in)
  )
  

(defmethod rank-object (object
                        predicate
                        key
                        fixed-dimensions
                        type
                        store-in)
  (missing-method 'sort 
                  object
                  predicate
                  key
                  fixed-dimensions
                  type
                  store-in))


(defmethod rank-object :around ((object T)
                                predicate
                                key
                                fixed-dimensions
                                (type (eql :interior))
                                (store-in null))
  (if object
    (rank-object object predicate key fixed-dimensions type (sel object))))


(defmethod rank-object ((object sequence)
                        (predicate function)
                        key
                        (fixed-dimensions null)
                        (type (eql :interior))
                        store-in)
  (declare (ignore fixed-dimensions type))


(let ((indexed-list
         (loop for i from 0 to (- (length self
               as l in list
               collect (cons i l))))
    (mapcar #'car
            (cl:sort indexed-list predicate 
                            :key (if key #'(lambda (x) (funcall key (cdr x)))
                                     #'cdr))))))

(defmethod-multi rank-object
  ((object (dimensioned-ref-object array))
   (predicate function)
   (key (function null))
   (fixed-dimensions null)
   (type (eql :interior)))
  
  (declare (ignore fixed-dimensions type))
  (basic-sort-by-eref object predicate :key key))


(defmethod-multi rank-object ((object (dimensioned-ref-object array))
                              (predicate function)
                              key
                              (fixed-dimensions list)
                              (type (eql :interior)))
  (loop
    for index
    from 0
    to (- (number-of-slices (dimensions-of object) fixed-dimensions) 1)
    collect
    (rank-object
     (apply #'row-major-ref-slice object index fixed-dimensions)
     predicate key NIL type))
  )
