;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               ffenv-std-mcl.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1990 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     Greg Anglin 1992.
;;;
;;;
;;;--------------------------------------------------------------------------------

(in-package :quail-kernel)

;;;  This calls the function which transforms the standard quail foreign
;;;  environments to use the customized libraries which have been built
;;;  for them.  

;;;  If the flag mk::*rebuild-standard-foreign-environment-libraries*
;;;  is non-nil, an MPW file called 
;;;      Quail:Foreign:ffenv-libs:standard-quail-ffenvs.make 
;;;  is created which, when its contents are evaluated in MPW, builds
;;;  the customized libraries.  This is unnecessary unless changes have
;;;  been made to a standard quail foreign environment. 
;;;  [ WARNING: despite its name, the aforementioned file is not quite a
;;;    make file in the MPW sense.  To use it, one must open it in MPW
;;;    and execute its contents. ]

#+:ccl
(standardize-foreign-environments)

#-:ccl
(warn "ffenv-standardize.lisp (Initialization) needs to be ported to this ~
       implementation")
