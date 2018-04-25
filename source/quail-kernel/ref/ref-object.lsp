;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               ref-object.lisp                               
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
;;;     R.W. Oldford 1991.
;;;     Greg Anglin 1994.
;;;
;;;
;;;--------------------------------------------------------------------------------
;;;  Includes:
;;;          ref-object
;;;          ref-obj-of
;;;          direct-ref-p
;;;          indirect-ref-p
;;;          empty-ro-p
;;;          dimensioned-ref-object
;;;          make-dimensioned-ref-object
;;;          convert-dim-ref-object
;;;          dimensions-of
;;;          number-of-dimensions
;;;          specs-of
;;;          specs-mask-of
;;;          print-dim-ref-object
;;;

(in-package :quail-kernel)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(ref-object dimensioned-ref-object dimensions-of 
          matrix-dimensions-of *dimensioned-classes*
          number-of-dimensions)))

;--------------------------------------------------------------------------------

;;;
;  DEFMETHOD of ref-obj-of for t, and hence all basic classes
;

(defgeneric ref-obj-of (self))

(defmethod ref-obj-of ((self t))
  :none)

;;; 
;  DEFMETHODs of dimensions-of for basic classes
;

(defgeneric dimensions-of (self)
  (:documentation "Returns the dimensions of its argument."))

(defmethod dimensions-of ((self t))
  (missing-method 'dimensions-of self))

(defmethod dimensions-of ((self number))
  nil)                                   

(defmethod dimensions-of ((self symbol))
  nil)                                  

(defmethod dimensions-of ((self sequence))
  (list (length self)))

;(defmethod dimensions-of ((self array))
;  (array-dimensions self))

(defmethod dimensions-of ((self array))
  (array-dimensions self))


(defun number-of-dimensions (object)
  "The number of dimensions in object. ~
    (:required ~
    (:arg object The object whose number of dimensions are to be determined.) ~
    )"
  (length (dimensions-of object)))

;;; 
;  DEFMETHODs of matrix-dimensions-of for basic classes
;

(defmethod matrix-dimensions-of ((self t))
  (missing-method 'matrix-dimensions-of self))

(defmethod-multi matrix-dimensions-of ((self (symbol number)))
  (list 1 1))

(defmethod matrix-dimensions-of ((self sequence))
  (list (length self) 1))

;(defmethod matrix-dimensions-of ((self array))
;  (case (array-rank self)
;    (0 (list 1 1))
;    (1 (append (array-dimensions self) (list 1)))
;    (2 (array-dimensions self))))

(defmethod matrix-dimensions-of ((self array))
  (case (array-rank self)
    (0 (list 1 1))
    (1 (append (array-dimensions self) (list 1)))
    (2 (array-dimensions self))))
        
;--------------------------------------------------------------------------------

;;;
;  DEFCLASS of ref-object.  All quail-objects accepted by ref are ref-objects.  Some 
;  additional basic classes (see Keene, p83) are accepted by ref as well.
;  Ref-object is an abstract class; we expect to instantiate only subclasses.  
;

(defclass ref-object (quail-object proto-mixin)
  ((ref-obj :accessor ref-obj-of
            :initform :none)))

(push-extension-class 'ref-object)

(defmacro direct-ref-p (r)
  `(symbolp (ref-obj-of ,r)))                ;; eq :none (ref-obj-of ,r) usually

(defmacro indirect-ref-p (r)
  `(not (direct-ref-p ,r)))

(defmacro empty-ro-p (ro)
  `(and (direct-ref-p ,ro) (eq (ref-contents-of ,ro) :none)))

;;;
;  DEFCLASS of dimensioned-ref-object.  Used for all classes with dimensions; in
;  particular some basic classes (Keene, p83) can be conveniently interpreted to
;  have dimension, and this class will be instantiated for indirect references to
;  those basic classes.
; 

(defclass dimensioned-ref-object (ref-object)
  ((dimensions :accessor dimensions-of
               :initarg :dimensions              ;; used by make-ref-array, etc.
               :initform :none)
   (specs :accessor specs-of
          :initform :none)
   (specs-mask :accessor specs-mask-of
          :initform :none)))

(push-extension-class 'dimensioned-ref-object)

;(defvar *dimensioned-classes*)
;(setf *dimensioned-classes*
;      '(symbol number cons sequence array string dimensioned-ref-object))

(defvar *dimensioned-classes*)
(setf *dimensioned-classes*
      '(symbol number cons sequence array string dimensioned-ref-object))

(defmethod quail-class-of ((self dimensioned-ref-object))
  (if (direct-ref-p self)
    (class-of self)
    (class-of (ref-obj-of self))))

(defun full-matrix-dimensions (dimensions)
  (case (length dimensions)
    (0 (list 1 1))
    (1 (append dimensions (list 1)))
    (t dimensions)))

(defmethod matrix-dimensions-of ((self dimensioned-ref-object))
  (with-slots (dimensions) self
    (full-matrix-dimensions dimensions)))

;;;  This is only called by the ref methods for basic classes.  It is always
;;;  called before any information is known about the new dimensioned ref-object, 
;;;  so it takes no parameters.

(defun make-dimensioned-ref-object ()
    (make-instance 'dimensioned-ref-object))

;;;  A function which takes an already instantiated dim-ref-obj dro
;;;  which is an indirect ref, and changes the slots to make it a direct
;;;  ref.  Only does changes to essential slots, and will destroy
;;;  the contents of some existing slots.  The calling routine is responsible
;;;  for saving anything it needs and filling contents later, with the
;;;  exception of:
;;;
;;;      1. dimensions
;;;

(defun convert-dim-ref-object (dro)
  (setf (ref-obj-of dro) :none)
  (setf (specs-of dro) :none)
  (setf (specs-mask-of dro) :none))

;--------------------------------------------------------------------------------

#|

(defmethod make-instance-string ((self dimensioned-ref-object)
                                 &optional (indent 0))
  (if (indirect-ref-p self)
    (make-instance-string (sel self) indent)             ; bummer
    (let* ((slot-inits 
            (delete-duplicates (delete nil 
                                       (apply #'append 
                                              (collect-slot-inits self)))
                               :key #'car
                               :test #'equal))
           (string-slot-inits 
            (mapcar #'(lambda (x) (string-slot-initarg x indent))
                    slot-inits)))
      (format nil "(make-instance '~S~{~A~})"
                  (class-name (class-of self))
                  string-slot-inits))))

(defmethod collect-slot-inits list ((self dimensioned-ref-object))
  (with-slots ((dimensions dimensions)) self
    (list (slot-initarg :dimensions dimensions :none))))

|#

;--------------------------------------------------------------------------------

;
;  Uses ref-print, which is loaded in z-ref.lisp, and some things from class
;  ref-array (cheating, but otherwise would be a real pain).
;
;  This function is primarily supports implementation of the print-object
;  generic function, and like print-object does NOT allow stream to be
;  NIL or T.

(defvar *array-print-case-fn*)
(setf *array-print-case-fn*
      (if (find #\a (format nil "~S" (make-array '())))
        #'string-downcase
        #'string-upcase))

(defun print-dim-ref-object (dro class-abbrev &optional (stream *quail-terminal-io*))
  (declare (special *array-print-case-fn*))
  (let ((dro-dims (if (slot-boundp dro 'dimensions)
                    (dimensions-of dro)
                    :unbound)))
    (cond ((eq dro-dims :unbound)
           (format stream "#<~S ~S>"
                   (class-name (class-of dro))
                   (system-get-pointer dro)))
          ((eq dro-dims :none)
           (format stream "#<empty ~S ~S>"
                   (class-name (class-of dro))
                   (system-get-pointer dro)))
          ((empty-ro-p dro) 
           (format stream "<empty ~S with dims ~S>"
                   (class-name (class-of dro))
                   dro-dims))
          (t (let* ((ro (ref-obj-of dro))
                    (ro (if (eq ro :none) dro ro))
                    (dro-num-dims (length dro-dims)))
               ;;  First the class-abbrev or special printing
               (typecase ro
                 (ref-array (case dro-num-dims
                              ;; (0 nil)   ;; dga 93 03
                              (t (format stream "#~S~A"
                                         dro-num-dims
                                         (funcall *array-print-case-fn*
                                                  class-abbrev)))))
                 (dimensioned-ref-object 
                  (format stream "#<dimensioned-ref-object of ~S>"
                          ro))
                 (list (case dro-num-dims
                         (0 (format stream "#0(~S)" (ref-list dro)))
                         (t (ref-print dro stream))))
                 (string
                  (case dro-num-dims
                    (0 (format stream "#0~S" (string (ref-list dro))))
                    (t (format stream "~S"
                               (coerce (apply #'vector (ref-list dro)) 'string)))))
                 ;; the next includes array and vector
                 (t (case dro-num-dims
                      ;; (0 nil)     ;; dga 93 03
                      (1 (format stream "#"))
                      (t (format stream "#~S~A"
                                 dro-num-dims
                                 (funcall *array-print-case-fn* 'a))))))
               ;;  Then the list of elements, when it hasn't been done already
               (typecase ro
                 (ref-array
                  (ref-print dro stream))
                 ((or string dimensioned-ref-object list) nil)
                 (t
                  (ref-print dro stream)))
               )))))

(defun print-unmistakeable-dim-ref-object (dro class-abbrev &optional (stream *quail-terminal-io*))
  (declare (special *array-print-case-fn*))
  (let ((dro-dims (if (slot-boundp dro 'dimensions)
                    (dimensions-of dro)
                    :unbound)))
    (cond ((eq dro-dims :unbound)
           (format stream "#<~S ~S>"
                   (class-name (class-of dro))
                   (system-get-pointer dro)))
          ((eq dro-dims :none)
           (format stream "#<empty ~S ~S>"
                   (class-name (class-of dro))
                   (system-get-pointer dro)))
          ((empty-ro-p dro) 
           (format stream "<empty ~S with dims ~S>"
                   (class-name (class-of dro))
                   dro-dims))
          (t (let* ((ro (ref-obj-of dro))
                    (dro-num-dims (length dro-dims)))
               (setf ro (if (eq ro :none) dro ro))
               ;;  First the class-abbrev or special printing
               (typecase ro
                 (ref-array (case dro-num-dims
                              ;; (0 nil)      ;; dga 93 03
                              (t (format stream "#~S~A"
                                         dro-num-dims
                                         (funcall *array-print-case-fn*
                                                  class-abbrev)))))
                 ((or dimensioned-ref-object string list)
                  (format stream "#<dimensioned-ref-object of ~S>"
                          ro))
                 ;; the next includes array and vector
                 (t (case dro-num-dims
                      ;; (0 nil)  ;; dga 93 03
                      (1 (format stream "#"))
                      (t (format stream "#~S~A"
                                 dro-num-dims
                                 (funcall *array-print-case-fn* 'a))))))
               ;;  Then the list of elements, when it hasn't been done already
               (typecase ro
                 (ref-array
                  (ref-print dro stream))
                 ((or string dimensioned-ref-object list) nil)
                 (t
                  (ref-print dro stream)))
               )))))

(defmethod print-object ((dro dimensioned-ref-object) stream)
  (cond 
    ((eq stream *terminal-io*)               (print-dim-ref-object dro 'DRO stream))
    ((eq stream *standard-output*)           (print-dim-ref-object dro 'DRO stream))
    ((eq stream *query-io*)                  (print-dim-ref-object dro 'DRO stream))
    ((eq stream *debug-io*)                  (print-unmistakeable-dim-ref-object dro 'DRO stream))
    ((eq stream *error-output*)              (print-unmistakeable-dim-ref-object dro 'DRO stream))
    ((eq stream *trace-output*)              (print-unmistakeable-dim-ref-object dro 'DRO stream))
    ((eq stream *quail-terminal-io*)         (print-dim-ref-object dro 'DRO stream))
    ((eq stream *quail-standard-output*)     (print-dim-ref-object dro 'DRO stream))
    ((eq stream *quail-query-io*)            (print-dim-ref-object dro 'DRO stream))
    ((eq stream *quail-debug-io*)            (print-unmistakeable-dim-ref-object dro 'DRO stream))
    ((eq stream *quail-error-output*)        (print-unmistakeable-dim-ref-object dro 'DRO stream))
    ((eq stream *quail-trace-output*)        (print-unmistakeable-dim-ref-object dro 'DRO stream))
    (T                                       (print-unmistakeable-dim-ref-object dro 'DRO stream)))
  dro)
  
