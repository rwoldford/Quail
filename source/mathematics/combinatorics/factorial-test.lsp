;;; factorial-test.lsp
;;; trying to check on the defconstant form
(defconstant *a-table-of-factorials*
  (make-array 33 :initial-element 1))

(defun show-it ()
	(format t "*a-table-of-factorials* is ~s" *a-table-of-factorials*))

(compile-file factorial-test.lsp)
(load factorial-test.fasl)

The constant *A-TABLE-OF-FACTORIALS* is being redefined (from
#(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
  1 1)
to
#(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
  1 1))
   [Condition of type DEFCONSTANT-UNEQL]