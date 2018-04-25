;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                          export-syms-from-quail.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1991 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1991.
;;;
;;;--------------------------------------------------------------------------------

(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(quail-import-export)))


(defun quail-import-export (symbols)
  "Import symbols to the Quail package and then ~
   export them from Quail.  ~
   (:see-also packages import export unexport ~
    unintern intern shadowing-import shadow)"
    (import symbols :quail)
    (export symbols :quail))

;;;
;;;  Export the external symbols of :quail-kernel from
;;;  quail as well
;;;

(defun visible-to-package-p (symbol package)
  "True if the given symbol (after coercion to a string) ~
   is visible to the specified package."
  (multiple-value-bind
    (sym type)
    (find-symbol (string symbol) package)
    (and sym (or (eq type :external)
                 (eq type :inherited)))))

(defun export-from-quail (&key symbols package)
  "Exports from the quail package ~
   all symbols in symbols that are visible to the ~
   quail package, or all external symbols ~
   of package."
  (cond
   (symbols 
    (loop for s in symbols
          when (visible-to-package-p s :quail)
          do (export s :quail)))
   (package (do-external-symbols (s package)
              (export s :quail)))))

(export-from-quail :package :quail-kernel)
(export-from-quail :package :new-math)
(export-from-quail :package :views)

;;;
;;; Selected symbols from window-basics if it exists
;;;

(if (find-package :window-basics)
  (quail-import-export
   '(wb::prompt-user wb::pick-one wb::prompt-t-or-f wb::prompt-true-or-false
     wb::check-items wb::collect-input)
   ))
