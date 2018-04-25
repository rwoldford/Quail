;;;******************************  test-linpack.lisp  **********************

(in-package :quail)

(load "ccl;Z:Z:zffi-load-all.lisp")

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

;--------------------------------------------------------------------------

(defgeneric dchdc (a jpvt job))

(defmethod dchdc ((self foreign-array) jpvt job)
  (let* ((dims (dimensions-of self))
         (nrow (first dims)))
    (with-open-quail-objects (self jpvt :temp work info) #'quail-pred3
      (setf work (array (list nrow) 'f-real*8 :class 'f-array
                                              :language :fortran))
      (setf info (array '() 'f-integer :class 'f-array 
                                       :language :fortran))
      (dchdc-hook self nrow nrow work jpvt job info)
      (eref info))))

;--------------------------------------------------------------------------

(setf a (array '(3 3) :type 'f-real*8
                      :class 'f-array
                      :language :fortran
                      :initial-contents '((1.523 1.897 3.456)
                                          (0.000 7.890 0.765)
                                          (0.000 0.000 10.56))))

(setf jpvt (array '(3) :type 'f-integer
                       :class 'f-array 
                       :language :fortran
                       :initial-contents '(0 0 0)))

(setf job 0)

(dchdc a jpvt job)

a
jpvt

(.* (tp a) a)

;==========================================================================

