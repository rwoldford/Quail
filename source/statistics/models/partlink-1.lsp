;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; partlink-1.lsp
;;;
;;; 21JUNE2004
;;;
;;; Holding bits of partlink.lsp which
;;; demonstrate the crash in link-checks.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; We start from all of partlink
;;; and try adding pieces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; first a package
(defpackage "WAYNE"
  (:use "COMMON-LISP")
  (:nicknames "IGOR"))

;;; then the in-package
(in-package :igor)

;;; the definition of the class link-object
;;;1 
(defclass link-object ()
  ((name :reader name-of :initarg :name))
  )

;;; now the definition of link-symbol
;;; modified for the new package
(eval-when (:compile-toplevel :load-toplevel)
  (defun link-symbol (link-identifier)
    (intern (concatenate 'string 
                         (string-upcase 
                          (if (listp link-identifier) ;; ie (quote foo) is a list
                            (second link-identifier)
                            link-identifier))
                         "-LINK")
            :igor)))

;;; and so to the macro
(defmacro def-link (link-identifier name link)
  "A macro which simplifies creation of link subclasses."
  (let ((symbol (link-symbol link-identifier)))
    `(progn
       ;; undefine previous versions ... allows easy redefine
       (defclass ,symbol () ())
       (defclass ,symbol (link-object)
         ((name :allocation :class :reader name-of 
                :initform (quote ,name))))
       (defvar ,symbol NIL ,name) 
       (setf ,symbol (make-instance (quote ,symbol)))
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (export (quote ,symbol) :igor)))))

;;; finally an example of def-link

(def-link :identity
  "Identity: mu"
  #'(lambda (z) z))  
 
