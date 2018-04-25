;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               num-array.lisp                               
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

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(num-array)))

;------------------------------------------------------------------------------

;;;
;  DEFCLASS of num-array
;

(defclass num-array (ref-array)
  ())

(push-extension-class 'num-array)

;------------------------------------------------------------------------------

;;;
;  return-class structure (see z-return-class.lisp)
;

(put-return-class 'num-array
                  'num-array
                  '(array vector cons  number integer fixnum float
                    rational complex symbol))

;------------------------------------------------------------------------------

;;;
;  initialize-contents methods for num-array
;

;; are these methods unnecessary with the introduction of the new array function?

(defmethod initialize-contents ((self num-array) (init (eql :empty)) 
                                &rest initargs &key)
  (declare (ignore initargs))
  (with-slots (dimensions
               ref-contents) self
    (setf ref-contents (make-array dimensions
                                   :initial-element nan))))
                                                
(defmethod initialize-contents ((self num-array) (init null)
                                &rest initargs &key initial-element)
  (declare (ignore initargs))
  (setf initial-element (or initial-element nan))
  (with-slots (dimensions
               ref-contents) self
    (setf ref-contents (make-array dimensions
                                   :initial-element initial-element))))

;------------------------------------------------------------------------------

;;;
;  DEFMETHODs for num-array
;

(defmethod ref ((self num-array) &rest args)
  (let* ((ref-self (ref-kernel (ref-instantiate self) self args))
         (num-dim-self  (length (dimensions-of self)))
         (num-dim-ref-self (length (dimensions-of ref-self))))
    (if (and (> num-dim-self 2) (<= num-dim-ref-self 2))
      (change-class ref-self 'matrix))
    ref-self))
    
(defmethod sel ((self num-array) &rest args)
  (let* ((ref-self (ref-instantiate self)))
    (ref-kernel ref-self self args)                      ;; changes ref-self !!
    (copy-ref ref-self)
    (let* ((num-dim-self  (length (dimensions-of self)))
           (num-dim-ref-self (length (dimensions-of ref-self))))
      (if (and (> num-dim-self 2)
               (<= num-dim-ref-self 2))
        (change-class ref-self 'matrix))
      ref-self)))

(defmethod print-object ((na num-array) stream)
  (print-dim-ref-object na 'N stream)
  na)

#|
(defmethod describe ((na num-array))
  (format t "~&#<num-array ~S> is an instance of class ~S with slots: ~
                  ~{~&~A~3,8@T~S~}"
                  (system-get-pointer na)
                  (class-of na)
                  (list
                  "CONTENTS" (contents-of na)
                  "DIMENSIONS" (dimensions-of na)))
  (values))
|#


         
                                                   
         





