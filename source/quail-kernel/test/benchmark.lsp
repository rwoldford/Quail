;;; ****************************** benchmark.lisp *********************************

(in-package :quail)

(defvar *n*)
(setf *n* 1000)

(defvar *small-n*)

(defvar *small-n1*)
(setf *small-n1* 50)

(defvar *small-n2*)
(setf *small-n2* 0)           ;  upper bound is max number of open files, fix later

;----------------------------------------------------------------------------------

(defun banner (test-name)
  (format *quail-terminal-io* "~&~%Testing ~A for ~S iterations:~%~%" test-name *n*))

(defun banner-small (test-name)
  (format *quail-terminal-io* "~&~%Testing ~A for ~S iterations:~%~%" test-name *small-n*))

;----------------------------------------------------------------------------------

(defclass foo ()
  ((bar :accessor bar-of
        :initarg :bar)))

(defclass foo-lish (foo)
  ())

(defun test-make-instance ()
  (banner "(make-instance 'foo :bar i)")
  (time (loop for i from 1 to *n* do
              (make-instance 'foo :bar i))))

(defun test-smarter-make-instance ()
  (banner "(make-instance {standard class FOO} :bar i)")
  (let ((std-class (find-class 'foo)))
    (time (loop for i from 1 to *n* do
                (make-instance std-class :bar i)))))

(defun test-make-sequence-10 ()
  (banner "(make-sequence 'list 10 :initial-element i)")
  (time (loop for i from 1 to *n* do
              (make-sequence 'list 10 :initial-element i))))

(defun test-make-sequence-100 ()
  (banner "(make-sequence 'list 100 :initial-element i)")
  (time (loop for i from 1 to *n* do
              (make-sequence 'list 10 :initial-element i))))

(defun test-change-class ()
  (banner "(change-class {a foo instance} 'foo-lish)")
  (let ((f (make-instance 'foo :bar 47)))
    (time (loop for i from 1 to (/ *n* 2) do
                (progn
                  (change-class f 'foo-lish)
                  (change-class f 'foo))))))

(defun test-accessor ()
  (banner "(setf (bar-of f) i), f an instance of foo,")
  (let ((f (make-instance 'foo :bar pi)))
    (time (loop for i from 1 to *n* do
                (setf (bar-of f) i)))))

(defmethod hand-coded-accessor ((self foo))
  (slot-value self 'bar))

(defmethod (setf hand-coded-accessor) (new-value (self foo))
  (setf (slot-value self 'bar) new-value))
                    
(defun test-hand-coded-accessor ()
  (banner "(setf (hand-coded-accessor f) i), f an instance of foo,")
  (let ((f (make-instance 'foo :bar pi)))
    (time (loop for i from 1 to *n* do
                (setf (hand-coded-accessor f) i)))))

(test-make-sequence-10)
(test-make-sequence-100)
(test-make-instance)
(test-smarter-make-instance)
(test-change-class)
(test-accessor)
(test-hand-coded-accessor)

;----------------------------------------------------------------------------------

(defmethod snerd ((self t) &optional bumpf)
  (declare (ignore bumpf))
  self)

(defmethod snerd ((self array) &optional bumpf)
  (declare (ignore bumpf))
  self)

(defmethod snerd ((self foo) &optional bumpf)
  (declare (ignore bumpf))
  self)

(defun test-method-t ()
  (banner "(snerd i i) [specialize on t]")
  (time (loop for i from 1 to *n* do
              (snerd i i))))

(defun test-method-array ()
  (banner "(snerd an-array i)")
  (let ((an-array (make-array nil)))
    (time (loop for i from 1 to *n* do
                (snerd an-array i)))))

(defun test-method-foo ()
  (banner "(snerd a-foo i)")
  (let ((a-foo (make-instance 'foo)))
    (time (loop for i from 1 to *n* do
                (snerd a-foo i)))))

(test-method-t)
(test-method-array)
(test-method-foo)

;----------------------------------------------------------------------------------

(defvar a)
(defvar b)
(defvar junk)

(defun test-single-direct-eref ()
  (banner-small "a single direct eref")
  (time (loop for i from 1 to *small-n* do
              (eref a 1 (- i i)))))

(defun test-single-direct-ref ()
  (banner-small "a single direct ref")
  (time (loop for i from 1 to *small-n* do
              (ref a 1 (- i i)))))

(defun test-single-direct-sel ()
  (banner-small "a single direct sel")
  (time (loop for i from 1 to *small-n* do
              (sel a 1 (- i i)))))

(defun test-single-indirect-eref ()
  (banner-small "a single indirect eref")
  (time (loop for i from 1 to *small-n* do
              (eref b 1 (- i i)))))

(defun test-single-indirect-ref ()
  (banner-small "a single indirect ref")
  (time (loop for i from 1 to *small-n* do
              (ref b 1 (- i i)))))

(defun test-single-indirect-sel ()
  (banner-small "a single indirect sel")
  (time (loop for i from 1 to *small-n* do
              (sel b 1 (- i i)))))

(defun test-submatrix-direct-ref ()
  (banner-small "a submatrix direct ref")
  (time (loop for i from 1 to *small-n* do
              (ref a '(0 1) (- i i)))))

(defun test-submatrix-direct-sel ()
  (banner-small "a submatrix direct sel")
  (time (loop for i from 1 to *small-n* do
              (sel a '(0 1) (- i i)))))

(defun test-submatrix-indirect-ref ()
  (banner-small "a submatrix indirect ref")
  (time (loop for i from 1 to *small-n* do
              (ref b '(0 1) (- i i)))))

(defun test-submatrix-indirect-sel ()
  (banner-small "a submatrix indirect sel")
  (time (loop for i from 1 to *small-n* do
              (sel b '(0 1) (- i i)))))

(defun do-the-ref-tests ()
  (test-single-direct-eref)
  (test-single-direct-ref)
  (test-single-direct-sel)
  (test-single-indirect-eref)
  (test-single-indirect-ref)
  (test-single-indirect-sel)
  (test-submatrix-direct-ref)
  (test-submatrix-direct-sel)
  (test-submatrix-indirect-ref)
  (test-submatrix-indirect-sel))

(setf *small-n* *small-n1*)

(format *quail-terminal-io* "~&~%**** Do eref, ref, and sel tests ~
                       for class num-array, ~S iterations ****~%~%"
                       *small-n*)

(setf a (array '(2 3) :initial-contents '((1.2 3.4 5.6) (7.8 9.1 2.3))))

(setf b (ref a '(0 1) '(0 2)))             ; create an indirect ref, warming up ref.

(setf junk (sel a '(0 1) '(0 2)))          ; warm up sel as well ...

(do-the-ref-tests)

(setf *small-n* *small-n2*)

(format *quail-terminal-io* "~&~%**** Do eref, ref, and sel tests ~
                       for class num-array, ~S iterations ****~%~%"
                       *small-n*)

(setf a (array '(2 3) :initial-contents '((1.2 3.4 5.6) (7.8 9.1 2.3))))

(setf b (ref a '(0 1) '(0 2)))             ; create an indirect ref, warming up ref.

(setf junk (sel a '(0 1) '(0 2)))          ; warm up sel as well ...

(do-the-ref-tests)

#|
(format *quail-terminal-io* "~&~%**** Do eref, ref, and sel tests ~
                       for class file-matrix, ~S iterations ****~%~%"
                       *small-n*)

(setf a (array '(2 3) :class 'file-matrix 
               :initial-element pi))

(setf b (ref a '(0 1) '(0 2)))             ; create an indirect ref, warming up ref.

(setf junk (sel a '(0 1) '(0 2)))          ; warm up sel as well ...

(do-the-ref-tests)
|#

;----------------------------------------------------------------------------------

(setf *small-n* *small-n1*)

(defun expander (cols-in-a b)
  (cglue-replicates cols-in-a b))

(defun test-expander ()
  (let ((b (array '(7) :initial-element 3.14))
        (cols-in-a 10))
    (banner-small "expander")
    (time (loop for i from 0 to *small-n* do
                (expander cols-in-a b)))))
