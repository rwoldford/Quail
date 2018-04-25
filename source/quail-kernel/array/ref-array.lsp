;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               ref-array.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1990, 1991, 1992 Statistical Computing Laboratory, University of Waterloo
;;;
;;; 
;;;  Authors:
;;;     Greg Anglin 1989, 1990, 1991.
;;;     M.E. LEWIS 1991.
;;;     R.W. Oldford 1992.
;;;     Greg Anglin 1993.
;;;
;;;--------------------------------------------------------------------------------

(in-package :quail-kernel)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(ref-array)))

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(dimensions-of
          contents-of
          eref
          ref)))

;------------------------------------------------------------------------------

;;;
;  DEFCLASS of ref-array
;

(defclass ref-array (dimensioned-ref-object)
  ((ref-contents :accessor ref-contents-of
                 :initarg :contents
                 :initform :none)))

(push-extension-class 'ref-array)

(defmethod initialize-instance :after ((self ref-array) &rest initargs)
  (declare (ignore initargs))
  (with-slots ((dim dimensions)
               ref-contents) self
    (cond ((and (not (eq dim :none))
                (eq ref-contents :empty))
            (initialize-contents self :empty)))))

;------------------------------------------------------------------------------

;;;
;  return-class structure (see z-return-class.lisp)
;

(put-return-class 'ref-array
                  'ref-array
                  '(ref-array array vector cons number integer fixnum
                    rational float complex symbol))

;------------------------------------------------------------------------------

(defmethod indices-array ((indices ref-array) 
                          &key (instance nil) (elements nil))
  (if (arrayp (ref-contents-of indices))
    (indices-array-kernel (ref-contents-of indices) instance elements)
    (indices-array-kernel indices t elements)))

;;;
;  initialize-contents methods for ref-array
;

;; the with-slots in these methods call the dimensions slot dim because the keys
;; will always contain the keyword :dimensions

(defmethod initialize-contents ((self ref-array) (init (eql :empty)) &rest initargs &key)
  (declare (ignore initargs))
  (with-slots ((dim dimensions)
               ref-contents) self
    (setf ref-contents (make-array dim
                                  :initial-element nil))))

(defmethod initialize-contents ((self ref-array) (init null)
                                         &rest initargs &key (initial-element nil))
;;
;; This method retained for compatibility with mk-array and make-ref-array.
;;
;; Not needed by, but compatible with and used by, array
;;                                       
  (declare (ignore initargs))
  (with-slots ((dim dimensions)
               ref-contents) self
    (setf ref-contents (make-array dim
                                  :initial-element initial-element))))

(defmethod-multi initialize-contents ((self ref-array) (init (number symbol))
                                      &rest initargs)
  ;; the method for (init string) is handled by (init array) to allow for
  ;; possibility of deconstruction
  (declare (ignore initargs))
  (with-slots ((dim dimensions)
               ref-contents) self
    (setf ref-contents (make-array dim
                                   :initial-element init))))

(defmethod initialize-contents ((self ref-array) (init list) 
                                &rest initargs &key (fill :default) (dimensions :default))
  (declare (ignore fill))  ;; used as initargs, though
  (with-slots ((dim dimensions)
               ref-contents) self
    ;; use make-array if easy or necessary
    (if (or (listp (first init))         ;; can't deal with this any other way, anyway
            (eql (length init)
                 dimensions)
            (and (eql (length init)
                      (first dimensions))
                 (null (rest dimensions)))) ;; conformable with make-array
      (setf ref-contents (make-array dim
                                     :initial-contents init))
      (progn
        (setf ref-contents (make-array dim))
        (apply #'fill-contents self init initargs))
      )))

(defmethod initialize-contents ((self ref-array)
                                (init dimensioned-ref-object)
                                &rest initargs &key (fill :row))
  (declare (ignore fill))  ;; used as initargs, though
  (with-slots ((dim dimensions)
               ref-contents) self
    ;; just do the thing which always works ... avoids indirect-ref problems ...
    (setf ref-contents (make-array dim))
    (apply #'fill-contents self init initargs)))

(defmethod initialize-contents ((self ref-array)
                                (init array)
                                &rest initargs &key (fill :row))
  (declare (ignore fill))  ;; used as initargs, though
  (with-slots ((dim dimensions)
               ref-contents) self
    ;; just do the thing which always works ... avoids indirect-ref problems ...
    (setf ref-contents (make-array dim))
    (apply #'fill-contents self init initargs)))
             
(defmethod initialize-contents ((self ref-array) (init scan-env) &rest initargs &key)
  (declare (ignore initargs))
  (with-slots ((dim dimensions)
               ref-contents) self
    (if (> (count t dim) 1)
      (quail-error "~&The dimensions specification ~S is ambiguous for scanning ... 
                    ~&there can be at most one ~S in the dimensions list."
                   dim
                   t))
    (let* ((size-guess (apply #'* (substitute 16 t dim)))
           (adjustable-vector (scan-into-adjustable-vector init size-guess))
           (marginal-size (apply #'* (substitute 1 t dim)))
           (warn nil))
      (if (zerop (rem (length adjustable-vector)
                      marginal-size))
        (setf dim (substitute (floor (length adjustable-vector)
                                            marginal-size)
                                     t
                                     dim))
        (progn
          (setf dim (substitute (ceiling (length adjustable-vector)
                                                marginal-size)
                                       t
                                       dim))
          (adjust-array adjustable-vector (list (apply #'* dim)))
          (setf warn t)))
      (setf ref-contents (make-array dim :displaced-to adjustable-vector))
      (if warn
        (warn "~&Scanned array ~S did not conform 
               exactly to specified dimensions."
              self)))))

(defmethod initialize-contents ((self t) (init t) &rest initargs &key)
  (declare (ignore initargs))
  (format *quail-terminal-io* "Error: Invalid initial-contents ~S or missing initialize-contents method.~%~%"
                        init)
  (missing-method 'initialize-contents self init))

;;;
;  DEFMETHODs for ref-array
;

(defmethod eref ((self ref-array) &rest subscripts)
  (setf subscripts (eref-true-index self subscripts))
  (if (direct-ref-p self)
    (apply #'aref (ref-contents-of self) subscripts)
    (apply #'eref (ref-obj-of self)
                  (eref-transform self subscripts))))

(defmethod (setf eref) (new-value (self ref-array) &rest subscripts)
  (declare (special *setf-eref*))
  (setf subscripts (eref-true-index self subscripts))
  (if (direct-ref-p self)
    (setf (apply #'aref (ref-contents-of self)
                        subscripts)
          new-value)
    (apply *setf-eref* 
           new-value 
           (ref-obj-of self)
           (eref-transform self subscripts))))

(defmethod ref ((self ref-array) &rest args)
;;
;; This could, in principle, be handled by the method for dimensioned-ref-object, but
;; instead I decided that that method would only handle the indirect-ref case, and would
;; signal a missing-method for direct-ref.  Then subclasses of
;; dimensioned-ref-object *have* to define a ref method.
;;
  (ref-kernel (ref-instantiate self) self args))

(defmethod print-object ((ra ref-array) stream)
  (print-dim-ref-object ra 'R stream)
  ra)

#|
(defmethod describe-object ((za ref-array) stream)
  (format stream "~&#<ref-array ~S> is an instance of class ~S with slots: 
                  ~{~&~A~3,8@T~S~}"
                 (system-get-pointer za)
                 (class-of za)
                 (list
                  "CONTENTS" (contents-of za)
                  "DIMENSIONS" (dimensions-of za)))
  (values))
|#

#|
;; This is broken and not really useful anyway
(defmethod contents-of ((self ref-array))
  (if (direct-ref-p self)
    (ref-contents-of self)
    (ref-contents-of (ref-obj-of self))))
|#

;------------------------------------------------------------------------------

;;;
;  Methods below here are for some sort of compatiblity with Common Lisp arrays.
;  Most are probably unused and untested.

(defgeneric array-element-type (array)
  (:documentation
   "Returns a type specifier which describes what data types an element of array may have."
  ))

;; useful for array-element-type ((self foreign-array))
(defmethod array-element-type ((self T))
  (type-of self))

(defmethod array-element-type ((self array))
  (cl:array-element-type
   self))

(defmethod array-element-type ((self ref-array))
  (cl:array-element-type 
   (if (indirect-ref-p self)
     (ref-obj-of self)
     (ref-contents-of self))))

;-----

(defgeneric array-rank (array)
  (:documentation
   "Returns the rank (number of dimensions) of array."
   ))

(defmethod array-rank ((self array))
  (cl:array-rank 
   self))


(defmethod array-rank ((self t))
  (length (dimensions-of self)))

;-----

(defgeneric array-dimension (array dimension)
  (:documentation
   "Returns the length of dimension of array.  Vector fill-pointers are ignored 
    (i.e. the total size, including inactive elements, is returned)."
   ))

(defmethod array-dimension ((self array) dimension)
  (cl:array-dimension
   self dimension))

(defmethod array-dimension ((self ref-array) dimension)
  (elt (dimensions-of self) dimension))

;-----

(defgeneric array-dimensions (array)
  (:documentation
   "Returns a list whose elements are the dimensions of array."
   ))

(defmethod array-dimensions ((self array))
  (cl:array-dimensions
   self))

(defmethod array-dimensions ((self ref-array))
  (dimensions-of self))

;-----

(defgeneric array-total-size (array)
  (:documentation
   "Returns the total size of array.  This is the product of the sizes of all the dimensions."
   ))

(defmethod array-total-size ((self array))
  (cl:array-total-size  self))

(defmethod array-total-size ((self dimensioned-ref-object))
  (apply #'* (dimensions-of self)))

;-----

(defgeneric array-in-bounds-p (array &rest SUBSCRIPTS)
  (:documentation
   "Returns true if subscripts are all legal subscripts for array."
   ))

(defmethod array-in-bounds-p ((self array) &rest subscripts)
  (apply #'cl:array-in-bounds-p self subscripts))

(defmethod array-in-bounds-p ((self ref-array) &rest subscripts)
  (every #'(lambda (x y) (and (>= x 0) (< x y)))
         subscripts
         (dimensions-of self)))

;-----

(defgeneric adjustable-array-p (array)
  (:documentation
   "Returns true if array is adjustable, and nil if it is not."
   ))

(defmethod adjustable-array-p ((self array))
  (cl:adjustable-array-p
   self))


(defmethod adjustable-array-p ((self ref-array))
  nil)

;-----

(defgeneric array-row-major-index (array &rest SUBSCRIPTS)
  (:documentation
   "Given an array and a valid set of subscripts, returns a single number 
    indicating the position of the accessed element based on row-major ordering. 
    This function ignores fill-pointers."
   ))

(defmethod array-row-major-index ((self array) &rest subscripts)
  (apply #'cl:array-row-major-index
         self subscripts))

#|
(defmethod array-row-major-index ((self ref-array) &rest subscripts)
  (apply #'cl:array-row-major-index
         (contents-of self) subscripts))
|#

;-----

(defgeneric array-column-major-index (array &rest SUBSCRIPTS)
  (:documentation
   "Given an array and a valid set of subscripts, returns a single number 
    indicating the position of the accessed element based on column-major ordering. 
    This function ignores fill-pointers."
   ))

(defmethod array-column-major-index ((self array) &rest subscripts)
  (declare (ignore subscripts))
  (missing-method 'array-column-major-index self))

(defmethod array-column-major-index ((self ref-array) &rest subscripts)
  (declare (ignore subscripts))
  (missing-method 'array-column-major-index self))

;-----

