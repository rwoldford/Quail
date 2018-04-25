;;;------------------------------------------------------------------------
;;;  init-return-classes.lisp
;;;
;;;  copyright 1991, r. wayne oldford
;;;------------------------------------------------------------------------

(in-package :quail-kernel)

(defmacro initialize-return-classes ()
  "Defines the return class hierarchy."
  `(make-initial-prototypes
    ,*return-classes-defined-here* 
    ,*return-precedence-list*))
  
(eval-when (eval load)
  (initialize-return-classes))

