;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               z-glue.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1990 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     Greg Anglin 1989, 1990.
;;;
;;;
;;;--------------------------------------------------------------------------------

(in-package :quail-kernel)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(glue rglue cglue rglue-replicates cglue-replicates)))

;--------------------------------------------------------------------------------

(defun glue (&rest args)
  (multiple-value-bind (x-list at)
                       (interpret args '((:at 0)))
    (if x-list
      (multiple-value-bind (xglue-dim at-lengths)
                           (glue-prep x-list at)
        (glue-kernel x-list at xglue-dim at-lengths))
      nil)))

(defun rglue (&rest args)
  (multiple-value-bind (x-list 1d)
                       (interpret args '((:1d :row)))
    (if x-list
      (multiple-value-bind (xglue-dim at-lengths)
                           (glue-prep x-list 0 1d)
        (glue-kernel x-list 0 xglue-dim at-lengths))
      nil)))
    
(defun cglue (&rest args)
  (multiple-value-bind (x-list 1d)
                       (interpret args '((:1d :col)))
    (if x-list
      (multiple-value-bind (xglue-dim at-lengths)
                           (glue-prep x-list 1 1d)
        (glue-kernel x-list 1 xglue-dim at-lengths))
      nil)))
    
;--------------------------------------------------------------------------------

;;;
;  Utility functions required for glue 
;

;
; glue-prep:  
;    Finds whether glue will work along axis at, and signals an error if not.
;    Returns multiple values:
;
;    1. xglue-dim, the dimensions which the object returned by glue will have
;    2. at-lengths, the added length along the at axis for each x in x-list
;

(defun glue-prep (x-list at &optional one-dim)
  (let* ((x-num (length x-list))
         (dim-list (mapcar #'dimensions-of x-list))
         (len-list (mapcar #'length dim-list))
         (n (apply #'max len-list))
         (xglue-dim nil)
         (at-lengths nil))
    (if (and one-dim (<= n 2))
      (progn
        (loop for i
              from 0
              to (- x-num 1)
              do (case (elt len-list i)
                   (0 (setf (elt len-list i) 2)
                      (setf (elt dim-list i) '(1 1)))
                   (1 (setf (elt len-list i) 2)
                      (setf (elt dim-list i) 
                            (ecase one-dim
                              (:row (list* 1 (elt dim-list i)))
                              (:col (append (elt dim-list i) (list 1))))))))
        (setf n (apply #'max len-list))))
    (if (> at n)
      (quail-error "Improperly specified :at parameter ~S." at))
    (if (= at n)
      (if (all-equal dim-list)
        (progn
          (setf xglue-dim (append (first dim-list) (list x-num)))
          (setf at-lengths (make-sequence 'list x-num :initial-element 1)))
        (glue-prep-error at))
      (let* ((at-mask (loop for i
                            from 0
                            to (- n 1)
                            collect (/= i at)))
             (pos-n (position n len-list))
             (dim-proto (list-if-t (elt dim-list pos-n) at-mask)))
;       (format *quail-terminal-io*
;               "~&n ~S at-mask ~S dim-proto ~S"
;               n at-mask dim-proto)
        (setf at-lengths
              (loop for i
                    from 0
                    to (- x-num 1)
                    collect (let* ((dim (elt dim-list i))
                                   (len (elt len-list i))
                                   (dim-mask nil)
                                   (at-length nil))
                              (cond ((eq n len)
                                       (setf dim-mask (list-if-t dim at-mask))
                                       (setf at-length (elt dim at)))
                                    ((eq (- n 1) len)
                                       (setf dim-mask dim)
                                       (setf at-length 1))
                                    (t (glue-prep-error at)))
;                             (format *quail-terminal-io*
;                                     "~&i ~S dim ~S len ~S dim-mask ~S at-length ~S"
;                                     i dim len dim-mask at-length)
                              (if (equal dim-proto dim-mask)
                                at-length
                                (glue-prep-error at)))))
        (setf xglue-dim (append (subseq dim-proto 0 at)
                                (list (apply #'+ at-lengths))
                                (subseq dim-proto at)))))
    (values xglue-dim at-lengths)))

(defun glue-prep-error (at) 
  (quail-error "Cannot glue given objects along axis ~S." at))

; glue-kernel:
;    creates and returns xglue as glue of elements of x-list along axis at.

(defun glue-kernel (x-list at xglue-dim at-lengths)  
  (let* ((xglue (apply #'make-dimensioned-result xglue-dim x-list))             
         (x-num (length x-list))
         (ref-indices (make-sequence 'list (length xglue-dim) :initial-element t))
         (at-current 0))
    (loop for i
          from 0
          to (- x-num 1)
          do (let ((x-at-length (elt at-lengths i)))
               (glue-setf-kernel xglue 
                                 ref-indices
                                 at
                                 at-current
                                 (elt x-list i)
                                 x-at-length)
               (setf at-current (+ at-current x-at-length))))
    xglue))

; glue-setf-kernel
;    destructively setf's x into xglue along at axis from at-current for x-at-length
    
(defun glue-setf-kernel (xglue ref-indices at at-current x x-at-length)
  (setf (elt ref-indices at) (iseq at-current (+ at-current x-at-length -1)))
  (apply *setf-ref* x xglue ref-indices))

;--------------------------------------------------------------------------------

(defun rglue-replicates (num-row row &rest other-involved-objects)
  (let* ((row-dim (dimensions-of row))
         (num-row-dim (length row-dim))
         (num-col (first row-dim))
         (result (apply #'make-dimensioned-result (list num-row num-col)
                                                  row
                                                  other-involved-objects)))
    (ecase num-row-dim
      (1
         (loop for j from 0 to (- num-col 1)
               do (let ((element (eref row j)))
                    (loop for i from 0 to (- num-row 1)
                          do (setf (eref result i j) element)))))
      (2
         (loop for j from 0 to (- num-col 1)
               do (let ((element (eref row j 0)))
                    (loop for i from 0 to (- num-row 1)
                          do (setf (eref result i j) element))))))
    result))

(defun cglue-replicates (num-col col &rest other-involved-objects)
  (let* ((col-dim (dimensions-of col))
         (num-col-dim (length col-dim))
         (num-row (first col-dim))
         (result (apply #'make-dimensioned-result (list num-row num-col)
                                                  col
                                                  other-involved-objects)))
    (ecase num-col-dim
      (1
         (loop for i from 0 to (- num-row 1)
               do (let ((element (eref col i)))
                    (loop for j from 0 to (- num-col 1)
                          do (setf (eref result i j) element)))))
      (2
         (loop for i from 0 to (- num-row 1)
               do (let ((element (eref col i 0)))
                    (loop for j from 0 to (- num-col 1)
                          do (setf (eref result i j) element))))))
    result))


