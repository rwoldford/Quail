;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                      linpack-envs.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; copyright (c) 1990 statistical computing laboratory, university of waterloo
;;;
;;;
;;;  authors:
;;;     greg anglin, michael lewis 1991.
;;;     Carsten Whimster, 1996
;;;
;;;  NOTE: This file is no longer used, since moving Linpack to Lisp.
;;;
;;;
;;;--------------------------------------------------------------------------------
 
(in-package :quail)

;----------------------------------------------------------------------------------

(mapcar #'make::add-object-directory
        (make::linear-foreign-directories))

;;; unfortunately we have to split up linpack into 3 foreign function environments
;;; otherwise ff-load complains that an offset gets too big to fit into a word.

(defparameter *linpack-entry-names-1*
  (list "dposl" "dpodi" "dgesl" "dgedi" "dpoco" "dpofa" "dgeco"
                "dgefa" "dsico" "dscal" "dsifa" "daxpy" "dswap" "idamax"
                "dasum" "ddot" "dgels"))

(defparameter *linpack-entry-names-2*
  (list "dtrsl" "dtrdi" "dsisl" "dtrco" "dscal" "dasum" "dsidi"
                "dswap" "daxpy" "dcopy" "ddot" "dtrls"))

(defparameter *linpack-entry-names-3*
  (list "dchdc" "dqrsl" "dcopy" "dqrdc" "dsvdc" "dswap" "drot"
                "drotg" "daxpy" "dscal" "dnrm2" "ddot" "dqrls"))

(defparameter *linpack-object-files-1*
  #+:apple (apply #'make::object-files 
                  :fortran *linpack-entry-names-1*)
  #+(and :aclunix (not :aclpc))  (make::object-files :fortran "linpack-env-1"))

(defparameter *linpack-object-files-2*
  #+:apple (apply #'make::object-files
                  :fortran *linpack-entry-names-2*)
  #+(and :aclunix (not :aclpc)) (make::object-files :fortran "linpack-env-2"))

(defparameter *linpack-object-files-3*
  #+:apple (apply #'make::object-files
                  :fortran *linpack-entry-names-3*)
  #+(and :aclunix (not :aclpc)) (make::object-files :fortran "linpack-env-3" "dqrls"))

(defparameter *linpack-blas-and-fortran-libraries-object-files*
  (make::object-files :library
                      #+(and :aclunix (not :aclpc)) "liblin_dG0"))

;----------------------------------------------------------------------------------

(def-ffenv 'linpack-env-1
  :files *linpack-object-files-1*
  :entry-names *linpack-entry-names-1*
  :libraries *linpack-blas-and-fortran-libraries-object-files*
  :standard-libraries (list :fortran)
  :library-entry-names nil)

(pushnew 'linpack-env-1 *standard-quail-ffenvs*)

(def-ffenv 'linpack-env-2
  :files *linpack-object-files-2*
  :entry-names *linpack-entry-names-2*
  :libraries *linpack-blas-and-fortran-libraries-object-files*
  :standard-libraries (list :fortran)
  :library-entry-names nil)

(pushnew 'linpack-env-2 *standard-quail-ffenvs*)

(def-ffenv 'linpack-env-3
  :files *linpack-object-files-3*
  :entry-names *linpack-entry-names-3*
  :libraries *linpack-blas-and-fortran-libraries-object-files*
  :standard-libraries (list :fortran)
  :library-entry-names nil)

(pushnew 'linpack-env-3 *standard-quail-ffenvs*)

