;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               synonyms.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1990 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1989, 1990, 1991.
;;;
;;;
;;;----------------------------------------------------------------------------
;;;
;;;  Includes:
;;;           make-synonym
;;;           alias
;;;          _           
;;;          **
;;;          ^
;;;

(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute)
 (export '(make-synonym alias _ ** ^)))


;;;----------------------------------------
;;;
;;;  Example: Some synonyms for assignment.
;;;

(make-synonym :old <- :new _ :warn nil)

;;;----------------------------------------
;;; 
;;;  A synonym for make-synonym
;;;

(make-synonym :old make-synonym :new alias :warn nil)

;;;-------------------------------------------------------------------------------
;;;
;;;  A method for exponentiation of numbers and a synonym.
;;;  - a method is used since ref-arrays have the same method for exponentiation.
;;;
;;;-------------------------------------------------------------------------------

#+:aclpc-linux (excl:without-package-locks
	(make-synonym :old expt :new ** :warn nil))
#+:sbcl-linux (sb-ext:without-package-locks
	(make-synonym :old expt :new ** :warn nil))

#-(or :aclpc-linux :sbcl-linux) (make-synonym :old expt :new ** :warn nil)

(make-synonym :old expt :new ^ :warn nil)


