;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               copy-dispatch.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1990 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     Greg Anglin 1989, 1990.
;;;     M.E. Lewis 1991.
;;;     Greg Anglin 1993.
;;;
;;;--------------------------------------------------------------------------------

(in-package :quail-kernel)

;;;
;  DEFGENERIC of copy-dispatch
;

(defgeneric copy-dispatch (ref-thing thing)) 

;;;
;  DEFMETHODs of copy-dispatch for basic classes
;

;; These methods default to getting the dimensions correct, at the expense
;;   of class.
;;
;; For list, string, vector users can use :shape t to ensure they get the
;;   right class at the expense of dimensions correctness.

(defmethod copy-dispatch ((proto list) ref-r)
  (let* ((dim (dimensions-of ref-r)))
    (if dim
      (let* ((r (make-sequence 'list (first dim))))
        (copy-kernel (ref-obj-of ref-r) r (specs-of ref-r) (specs-mask-of ref-r))
        r)
      (eref ref-r))))       ; handles 0-dim case

(defmethod copy-dispatch ((proto string) ref-r)
  (let* ((dim (dimensions-of ref-r)))
    (if dim
      (let* ((r (make-sequence 'string (first dim))))
        (copy-kernel (ref-obj-of ref-r) r (specs-of ref-r) (specs-mask-of ref-r))
        r)
      (eref ref-r))))       ; handles 0-dim case

(defmethod copy-dispatch ((proto array) ref-r)
  (let* ((dim (dimensions-of ref-r)))
    (let* ((r (make-array dim :element-type (array-element-type proto))))
        (copy-kernel (ref-obj-of ref-r) r (specs-of ref-r) (specs-mask-of ref-r))
        r)))       
    
;;  Note: I think we don't need a method for dimensioned-ref-object, cuz proto came from
;;  the ref-obj slot to begin with, see copy-ref.

(defmethod copy-dispatch ((proto ref-array) ref-r)
  (let* ((dim (dimensions-of ref-r)))
    (let* ((ref-obj (ref-obj-of ref-r))
             (specs (specs-of ref-r))
             (specs-mask (specs-mask-of ref-r)))
        (convert-dim-ref-object ref-r)
        (setf (ref-contents-of ref-r)
              (make-array dim
                          :element-type (array-element-type proto)))
        (copy-kernel ref-obj ref-r specs specs-mask)
        ref-r)))        

#|
;; Alternate versions ... troubles with 0-dim case ...
;; ...  quick hack, might be wrong ... dga 92 11 03
;; ...  I think this is the right separation now ... dga 93 03 20

(defmethod copy-dispatch ((proto list) ref-r)
  (let* ((dim (dimensions-of ref-r)))
    (let* ((r (make-list (or (first dim) 1))))
        (copy-kernel (ref-obj-of ref-r) r (specs-of ref-r) (specs-mask-of ref-r))
        r)))       

(defmethod copy-dispatch ((proto string) ref-r)
  (let* ((dim (dimensions-of ref-r)))
    (let* ((r (make-string (or (first dim) 1))))
        (copy-kernel (ref-obj-of ref-r) r (specs-of ref-r) (specs-mask-of ref-r))
        r)))

(defmethod copy-dispatch ((proto array) ref-r)
  (let* ((dim (dimensions-of ref-r)))
    (if dim
      (let* ((r (make-array dim :element-type (array-element-type proto))))
        (copy-kernel (ref-obj-of ref-r) r (specs-of ref-r) (specs-mask-of ref-r))
        r)
      (eref ref-r))))       ; handles 0-dim case
    

(defmethod copy-dispatch ((proto ref-array) ref-r)
  (let* ((dim (dimensions-of ref-r)))
    (if dim
      (let* ((ref-obj (ref-obj-of ref-r))
             (specs (specs-of ref-r))
             (specs-mask (specs-mask-of ref-r)))
        (convert-dim-ref-object ref-r)
        (setf (ref-contents-of ref-r)
              (make-array dim
                          :element-type (array-element-type proto)))
        (copy-kernel ref-obj ref-r specs specs-mask)
        ref-r)
      (eref ref-r))))        ; handles 0-dim case
|#

;--------------------------------------------------------------------------------
;  copy-kernel is called by copy-dispatch unless the copy requires special
;  attention, such as in the case of foreign arrays.
;

(defun copy-kernel (src targ specs specs-mask)
  (declare (special *setf-eref*))
  (let* ((marginp (listp specs))
         (dim (dimensions-of targ))
         (targ-num-dims (length dim))
         (transformer (if marginp
                        #'eref-transform-of-margin
                        #'eref-transform-of-indices))
         (current (make-sequence 'list targ-num-dims :initial-element 0))
         index)
    (loop for d
          from 0
          to (- (apply #'* dim) 1)
          do (progn
               (setf index (funcall transformer
                                    (expand-list current 
                                                 specs-mask
                                                 0)
                                    specs))
               (apply *setf-eref*
                      (apply #'eref src index)
                      targ
                      current)
               (setf current (row-major-next-subscript current dim)))))
  targ)

