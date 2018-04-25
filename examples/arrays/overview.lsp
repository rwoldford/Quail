;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;               Overview of array manipulation functions                            
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (C) 1994 Statistical Computing Laboratory, University Of Waterloo
;;;
;;;
;;;
;;;  Authors:
;;;      R.W. Oldford  1994.
;;; 
;;; There is a lot of material to be covered about array manipulation
;;; in Quail so that the examples are split up into a number of topic files.
;;; A not unreasonable tutorial would be to browse the files in order.
;;; 
;;; Here we outline the contents of those files and offer a form to
;;; evaluate that will bring each example file up as desired.
;;;

(in-package :quail-user)
         
;;;               Topic                               Major Functions
;;; 
;;;    Introduction to array creation  ...             array
;;;                    array attributes ...            dimensions-of
;;;                                                    number-of-dimensions
;;;                                                    number-of-elements                    array element access ...        eref
;;;                    setting array elements ...      (setf eref)

(edit-file "eg:Arrays;intro.lsp")

;;;    Some handy arrays ...                           ones-array, seq, iseq
;;;                                                    diagonal, diagonal-of
;;;                                                    identity-matrix,
;;;                                                    upper-triangle-of

(edit-file "eg:Arrays;handy-arrays.lsp")

;;; 
;;;    Referencing blocks within arrays ...            ref
;;;       Copying blocks within arrays ...             sel
;;;       Setting blocks within arrays ...             (setf ref), (setf sel)

(edit-file "eg:Arrays;ref.lsp")

;;; 
;;;    Advanced array creation ...                     array & its keywords

(edit-file "eg:Arrays;array.lsp")

;;; 
;;;    Basic mathematical operations ...               +  -  *  /  .*
;;;                                                    min, max, tp

(edit-file "eg:Arrays;arith-ops.lsp")

;;; 
;;;    Other Mathematical functions ...                expt log exp sin cos
;;;                                                    atanh ... etc.

(edit-file "eg:Arrays;math-funs.lsp")

;;; 
;;;    Testing by numerical predicates ...             =, <, <=, >, >=
;;;                                                    equals-object,
;;;                                                    less-than-object,
;;;                                                    less-than-equals-object,
;;;                                                    greater-than-object,
;;;                                                    greater-than-equals-object,
;;;                                                    test-elements

(edit-file "eg:Arrays;num-preds.lsp")


;;; 
;;;    Selecting elements by predicate ...             indices, ref-if

(edit-file "eg:Arrays;select-by-pred.lsp")

;;; 
;;;    Putting arrays together ...                     cglue, rglue, glue

(edit-file "eg:Arrays;glue.lsp")

;;; 
;;;    Sorting, ranking, etc. ...                      sort, sort-object
;;;                                                    sort-position
;;;                                                    ranks
;;;                                                    order
;;;                                                    permute, permute-object

(edit-file "eg:Arrays;sort.lsp")

;;; 
;;; 
;;;    Iteration introduction ...                      loop, dotimes, dolist  

(edit-file "eg:Arrays;iter-general.lsp")

;;; 
;;;    General Iteration over elements ...             column-major-eref
;;;                                                    row-major-eref
;;;                                                    number-of-slices
;;;                                                    column-major-ref-slice
;;;                                                    row-major-ref-slice
;;;                                                    column-major-list-elements
;;;                                                    row-major-list-elements
;;;                                                    column-major-set-elements
;;;                                                    row-major-set-elements

(edit-file "eg:Arrays;iter-elements.lsp")

;;; 
;;;    Simple iteration macros for slices ...          doslices, collect-slices 

(edit-file "eg:Arrays;iter-slices.lsp")

;;; 
;;; 
;;;    Mapping functions                               map-element
;;;                                                    map-slices
;;;                                                    reduce-slices
;;;                                                    collapse
;;;                                                    sweep

(edit-file "eg:Arrays;iter-map.lsp")

;;; 
;;;    Searching ref-objects ...                       find-slices, find-slices-if
;;;                                                    count-slices, count-slices-if
;;;                                                    slice-positions
;;;                                                    slice-positions-if

(edit-file "eg:Arrays;search.lsp")

;;; 
;;;    Modifying ref-objects ...                       remove-slices
;;;                                                    remove-slices-if
;;;                                                    substitute-slices
;;;                                                    substitute-slices-if
;;;                                                    replace-slices

(edit-file "eg:Arrays;iter-modify.lsp")

;;; 
;;;    Focus on matrices ...                           rank-of, trace-of
;;;                                                    determinant, inverse,
;;;                                                    condition-number-of,
;;;                                                    singular-values-of,
;;;                                                    solve,
;;;                                                    decompositions,


(edit-file "eg:Arrays;Matrices;overview.lsp")


  

