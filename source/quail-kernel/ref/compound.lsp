;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               z-compound.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1990 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1989, 1990.      [ Incomplete ]
;;;
;;;
;;;--------------------------------------------------------------------------------
;;;
;;;  Includes:
;;;          z-compound
;;;          make-compound
;;;          components-of
;;;

(in-package :quail-kernel)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(z-compound make-compound components-of)))

;----------------------------------------------------------------------------- 

(defclass z-compound (ref-object)
  ((ref-contents :reader contents-of :initform (make-hash-table))))

(push-extension-class 'z-compound)

(defun make-compound (&key names values name-value-list like)
  (let ((instance NIL))
    (cond ((and names values
                (listp names) (listp values)
                (= (length names) (length values)))
           (setf instance (make-instance 'z-compound))
           (loop for n in names for v in values
              do (setf (ref instance n) v)))
          ((and names values
                (listp names) (listp values))
           (quail-cerror "Ignore the keyword parameters names and values"
                   "The keyword parameters names and values are lists ~
                     of unequal length!~%~
                     Names = ~S ~%~
                     and values = ~S ~%."
                     names values))
          ((and names values)
           (setf instance (make-instance 'z-compound))
           (setf (ref instance names) values))
          ((and names (listp names))
           (setf instance (make-instance 'z-compound))
           (loop for n in names
              do (setf (ref instance n) NIL)))
          (names
           (setf instance (make-instance 'z-compound))
           (setf (ref instance names) NIL)))    
    (if name-value-list
      (loop for pair on name-value-list
            by #'cddr
            with obj = (or instance (make-instance 'z-compound))
            do (setf (ref obj (car pair)) (cadr pair))))    
    (if (and like (typep like 'z-compound))
      (loop for name in (components-of like)
            with obj = (or instance
                           (setf instance (make-instance 'z-compound)))
            do (setf (ref obj name) NIL)))    
    (or instance (make-instance 'z-compound))))

;;;
;  DEFMETHODs for ref
;

(defmethod ref ((self z-compound) &rest selectors)
  (let ((n (length selectors)))
    (cond ((= n 0) self)
          ((= n 1) (gethash (first selectors) (contents-of self)))
          (t (loop for s in selectors with c = (contents-of self)
                         collect (gethash s c))))))

;;  setf:  do this way to be consistent with defgeneric

(defmethod (setf ref) (new-value (self z-compound) &rest selectors)
  (setf (gethash (first selectors) (contents-of self)) new-value)) 

(defmethod components-of ((self z-compound))
  (let ((components '()))
    (labels ((key-collect (key val)
                 (declare (ignore val))
                 (setf components (cons key components))))
      (maphash #'key-collect (contents-of self))
    components)))

      
                
        
      
