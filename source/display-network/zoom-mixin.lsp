;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               zoom-mixin.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1992 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1992.
;;;
;;;
;;;----------------------------------------------------------------------------

(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(zoom-mixin zoom)))

;;;
;;; class definitions
;;;

(defclass zoom-mixin ()
  ((zoom-window-cache :initform nil
                      :accessor zoom-window-cache-of)))
  
(defmethod zoom ((self zoom-mixin))
  "Zooms in on the current object by displaying it and its sub-structures in ~
   a new view.  ~
   The browser is cached in self and self is deposited in the browser. This ~
   circular structure is removed by the specialized destroy methods for ~
   quail-objects and microscopic-view."

       (if (and (slot-value self 'zoom-window-cache)
                (understands (slot-value self 'zoom-window-cache)
                       'zoom))
           (zoom (slot-value self 'zoom-window-cache))
           (let ((zoomed-view (make-instance 'microscopic-view)))
                (setf (slot-value zoomed-view 'zoomed-object)
                      self)
                (setf (slot-value self 'zoom-window-cache)
                      zoomed-view)
                (zoom zoomed-view))))



