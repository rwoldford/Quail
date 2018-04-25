;;;******************************  test-linpack.lisp  **********************

(in-package :quail)

;===========================================================================

(defunf 'dchdc-hook :language :fortran
                    :entry-name "DCHDC" 
                    :arguments  '((array f-real*8)      ;a
                                  fixnum                ;lda
                                  fixnum                ;p
                                  (array f-real*8)      ;work
                                  (array f-integer)     ;jpvt
                                  fixnum                ;job
                                  (array f-integer)     ;info
                                  ))

;===========================================================================

(defun dchdc (a jpvt job)
  (let* ((dims (dimensions-of a))
         (nrow (first dims))
         (work (make-array (list nrow) :initial-element 0))
         (info (make-array '() :initial-element 0)))
    (dchdc-hook a nrow nrow work jpvt job info)
    (eref info)))

;--------------------------------------------------------------------------

(setf a (make-array '(3 3) :initial-contents '((1.523 1.897 3.456)
                                               (0.000 7.890 0.765)
                                               (0.000 0.000 10.56))))

(setf jpvt (make-array '(3) :initial-contents '(0 0 0)))

(setf job 0)

(dchdc a jpvt job)

a
jpvt

(.* (tp a) a)

