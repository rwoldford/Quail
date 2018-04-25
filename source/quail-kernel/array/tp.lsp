;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               tp.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1990 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     Greg Anglin 1989, 1990.
;;;     R.W. Oldford 1991.
;;;     M.E. Lewis 1991.
;;;
;;;
;;;--------------------------------------------------------------------------------
;;;
;;;  Includes:
;;;          tp
;;;          tp-dispatch
;;;

(in-package :quail-kernel)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(tp)))

;--------------------------------------------------------------------------------

;;;
;  DEFGENERIC for tp
;

(defgeneric tp (r &key perm)
  (:documentation "A generalized transpose operator.")
  )

;--------------------------------------------------------------------------------

;;;
;  Utility functions required for tp method
;

(defun tp-will-work (r perm)
  (let* ((dim (dimensions-of r))
         (len (length dim))
         (unperm (iseq len))
         (will-work (equal unperm (sort (ref-list perm) #'<))))
    (if will-work
      t
      (tp-error r perm))))

(defun tp-setf-kernel (rtp r perm)    ; destructively sets rtp to tp of r
  (declare (special *setf-eref*))
  (let* ((dim (dimensions-of r))
         (len (length dim))
         (current (make-sequence 'list (length dim) :initial-element 0)))
    (loop for d
          from 0
          to (- (apply #'* dim) 1)
          do (let* ((indices (perm-indices current perm len)))
               (apply *setf-eref* 
                      (apply #'eref r current)
                      rtp
                      indices)
               (setf current (row-major-next-subscript current dim))))))

(defun perm-indices (current perm len)
  (loop for i
        from 0
        to (- len 1)
        collect (elt current (elt perm i))))

(defun tp-dispatch (r perm)
  (let* ((dim (dimensions-of r))
         (len (length dim)))
    (if (not perm)
      (if (eq len 1)
        (setf perm '(1 0))
        (setf perm (reverse (iseq len)))))
    (cond ((and (eq len 1) (equal perm '(1 0)))
             (let ((cv (make-dimensioned-result (list 1 (first dim)) r)))
               (setf (ref cv) r)
               cv))
          ((and (eq len 1) (equal perm '(0)))
             r)
          ((and (eq len 0) (not perm))
             r)
          ((tp-will-work r perm)                  ; this case traps errors
             (let* ((dim (dimensions-of r))
                    (tp-r (make-dimensioned-result
                           (perm-indices dim perm len)
                           r)))
               (tp-setf-kernel tp-r r perm)
               tp-r)))))

(defun tp-error (r perm)
  (let ((len (length (dimensions-of r))))
    (quail-error "Second argument to tp must be a permutation of 0 through ~S, ~
            ~&since ~S has ~S dimensions.  ~
            ~&Provided argument ~S is not such a permutation."
           (- len 1)
           r
           len
           perm)))

;;; 
;  DEFMETHODs of tp
;

(defmethod tp ((self number) &key (perm NIL))
  (if (not perm)
    self
    (tp-error self perm)))

(defmethod tp ((self (eql NaN)) &key perm)
  (if (not perm)
    self
    (tp-error self perm)))

(defmethod tp ((self (eql +infinity)) &key perm)
  (if (not perm)
    self
    (tp-error self perm)))

(defmethod tp ((self (eql -infinity)) &key perm)
  (if (not perm)
    self
    (tp-error self perm)))

(defmethod tp ((self list) &key perm)
  (if (not perm)
    (setf perm '(1 0)))
  (cond ((equal perm '(0)) self)
        ((equal perm '(1 0)) (make-array (list 1 (length self))
                                         :initial-contents (list self)))
        (t (tp-error self perm))))

(defmethod tp ((self array) &key perm)
  (tp-dispatch self perm))

(defmethod tp ((self ref-array) &key perm)
  (tp-dispatch self perm))

(defmethod tp ((self t) &key perm)
  (missing-method 'tp self perm))
