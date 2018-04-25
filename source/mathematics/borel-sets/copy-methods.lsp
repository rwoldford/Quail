;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               copy-methods.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1990 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     N. Wiebe 1998
;;;
;;;
;;;----------------------------------------------------------------------------------
;;;
;;;
;;;      Method                 |    Argument type     |
;;;                             |______________________|
;;;      Name                   |      Argument        |
;;;  ___________________________|______________________|
;;;                             |                      |
;;;                             |                      |      
;;;  op-copy-args               |      borel-set       |
;;;                             |   complement-set     |
;;;                             |   countable-set      |
;;;                             |      empty-set       |
;;;                             | explicit-finite-inter|
;;;                             | explicit-finite-set  |
;;;                             | explicit-finite-union|
;;;                             |      interval        |
;;;                             |   predicate-set      |
;;;                             |  (extended-realp)    |
;;;                             |                      |
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; copy-set - uses op-copy-args to copy a borel-set or a set-collection
;;; op-copy-args  - copies arguments to a list
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun copy-set (set &rest new-args)
  (if (extended-realp set)
    set
    (if new-args
      (apply #'make-instance (type-of set) (concatenate 'list new-args (op-copy-args set)))
      (apply #'make-instance (type-of set) (op-copy-args set)))))

(defgeneric op-copy-args (set)
  (:documentation "Copies arguments."))

(defmethod op-copy-args :around ((set borel-set))
  (concatenate 'list `(:infimum ,(infimum set)
                                :supremum ,(supremum set)
                                :closure-property ,(closure-property-of set))
               (call-next-method set)))

(defmethod op-copy-args :around ((set set-collection))
  (concatenate 'list `(:contains-set-p-function ,(contains-set-p-function-of set)
                                                         :disjoint? ,(disjointness-of set))
                              (call-next-method set)))


(defmethod op-copy-args :around ((set predicate-set))
  (concatenate 'list `(:defining-predicate ,(defining-predicate-of set))
                              (call-next-method set)))

(defmethod op-copy-args :around ((set countable-collection))
  (concatenate 'list `(:contains-set-p-function ,(contains-set-p-function-of set)
                                                         :disjoint? ,(disjointness-of set)
                                                         :from ,(begin-index-of set)
                                                         :to ,(end-index-of set)
                                                         :element-function ,(element-function-of set)
                                                         :index-function ,(index-function-of set))
                              (call-next-method set)))

(defmethod op-copy-args :around ((set explicit-finite-collection))
  (concatenate 'list `(:contents ,(loop for item in (contents-of set) collect
                                        (copy-set item))
                                 :set-less-than-predicate ,(set-less-than-predicate-of set))
               (call-next-method set)))


(defmethod op-copy-args ((set complement-set))
  `(:set ,(copy-set (set-of set)) 
         :wrt ,(copy-set (complement-wrt set))))

;;; dummy primary methods

(defmethod op-copy-args ((set set-collection))
  (declare (ignore set)))

(defmethod op-copy-args ((set borel-set))
  (declare (ignore set)))

(defmethod op-copy-args (set)
  (if (extended-realp set)
    (list set)
    (quail-error "The set ~s is not a borel set." set)))
