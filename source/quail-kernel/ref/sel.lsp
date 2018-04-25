;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               sel.lisp                               
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
;;;
;;;  Includes:
;;;          sel
;;;          copy-ref
;;;

(in-package :quail-kernel)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(sel)))

;--------------------------------------------------------------------------------

;;;
;  Utility functions required by the sel defmethods
;  (some others in eref.lisp).
;

(defun copy-ref (ref-r)
  (let* ((proto (ref-obj-of ref-r)))             ;; only used to find proper class
    (copy-dispatch proto ref-r)))

;--------------------------------------------------------------------------------

;;;
;  DEFGENERIC of sel
;

(defgeneric sel (r &rest args)
  (:documentation "Sel is identical to ref except that it makes ~
                   and returns a copy of the referenced structure of r ~
                   as opposed to a reference to the r. ~
                   (:see-also ref eref) ~
                   ")
  )

;;;
;  DEFMETHODs of sel
;

(defmethod sel ((self dimensioned-ref-object) &rest args)
  (if (indirect-ref-p self)
    (let* ((ref-self (ref-instantiate self)))
      (ref-kernel ref-self self args)                      ;; changes ref-self !!
      (copy-ref ref-self))
    (missing-method 'sel self)))

(defmethod sel ((self character) &rest args)
  (declare (ignore args))
  self)

(defmethod sel ((self number) &rest args)
  (declare (ignore args))
  self)

(defmethod sel ((self symbol) &rest args)
  (declare (ignore args))
  self)

(defmethod sel ((self sequence) &rest args)
  (let* ((ref-self (ref-instantiate self)))
    (ref-kernel ref-self self args)                      ;; changes ref-self !!
    (copy-ref ref-self)))

;;; array also handles string case

(defmethod sel ((self array) &rest args)
  (let* ((ref-self (ref-instantiate self)))
    (ref-kernel ref-self self args)                      ;; changes ref-self !!
    (copy-ref ref-self)))

;-------------------------------------------------------------------------------

;;;
;  DEFGENERIC of (setf sel)
;

(defgeneric (setf sel) (new-value r &rest args))

;;;
;  DEFMETHOD of (setf sel)
;

(defmethod (setf sel) (new-value (self t) &rest args)
  (apply *setf-ref* new-value self args))
