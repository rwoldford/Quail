;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               return-class.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1990 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     Greg Anglin 1989, 1990.
;;;     
;;;
;;;--------------------------------------------------------------------------------
;;;
;;;  Includes:
;;;          the-class-name
;;;          put-return-class 
;;;          get-return-class
;;;          find-return-class
;;;

(in-package :quail-kernel)

;----------------------------------------------------------------------------------
(eval-when (:compile-toplevel :load-toplevel :execute) (export '(put-return-class get-return-class)))
(defvar *return-class-table*)
(setf *return-class-table* (make-hash-table :size 20 :test #'equal))

(defun the-class-name (c)
  (if (symbolp c)
    c
    (quail-class-name c)))

(defun put-return-class (ret-class class-1 classes)
  "Assigns the first argument as the name of the class to ~
   to be returned as the default type for any binary operation ~
   involving the class named as the second argument and any of ~
   the classes named in the list constituting the third argument.~
   (:see-also get-return-class)"
  (declare (special *return-class-table*))
  (let* ((ret-class (the-class-name ret-class))
         (class-1 (the-class-name class-1))
         (classes (mapcar #'the-class-name classes))
         (expanded-classes NIL))
    (loop for class in classes
                 do
                 (cond
                  ((eq class 'number)
                   (setf expanded-classes
                         (append '(number complex rational
                                   float integer fixnum bignum)
                                 expanded-classes)))
                  ((eq class 'integer)
                   (setf expanded-classes
                         (append '(integer fixnum bignum)
                                 expanded-classes))
                   )
                  ((eq class 'list)
                   (setf expanded-classes
                         (append '(list cons)
                                 expanded-classes))
                   )
                  (T (push class expanded-classes))
                 ))
    (setf classes
          (remove-duplicates expanded-classes :test #'eq))
    (loop for class-2 in classes
          do (setf (gethash (list class-1 class-2) *return-class-table*)
                   ret-class)
             (setf (gethash (list class-2 class-1) *return-class-table*)
                   ret-class))
    t))

;;;  get-return-class returns nil if the pair of class names is not found in the
;;;  hash table.  This will happen often, and it is up to the calling routine
;;;  to decide what class is appropriate (usually use the class of the first
;;;  operand).

;;;  NOTE:  get-return-class expects CLOS class _names_, not the classes themselves!

(defun get-return-class (class-1 class-2)
  "Returns the name of the class to ~
   to be returned as the default type for any binary operation ~
   involving the classes named as the first and second arguments. ~
   If no return-class has been identified with this pair, then NIL is returned. ~
   (:see-also put-return-class)"
  (declare (special *return-class-table*))
  (let* ((classes (list class-1 class-2)))
    (gethash classes *return-class-table*)))

;----------------------------------------------------------------------------------

;;         Note that I've cheated and used class matrix before it is
;;         defined -- this may cause problems later.

(defun find-return-class (len-dim minimal-class proto-list)
  (let* ((class-list (mapcar #'quail-class-name
                             proto-list))
         ;; Here we find the minimal class for a (setf ref)-able
         ;; object of the required number of dimensions.  Use of
         ;; class matrix for zero dimensions is somewhat arbitrary,
         ;; but probably the best guess ... This needs to be
         ;; thought out more carefully when classes like, say,
         ;; symmetric matrix are around.
         (minimal-class (if minimal-class
                          (the-class-name minimal-class)
                          (case len-dim
                            (0 'matrix)
                            (1 'cons)
                            (otherwise 'array))))
         ;; NOTE: if class-list is nil, get-return-class is NOT called.
         ;; Hence get-return-class is _always_ called with 2 arguments
         ;; (see definition of reduce).
         (ret-class (reduce #'get-return-class
                            class-list
                            :initial-value minimal-class)))
    (if (not ret-class)
      (setf ret-class (first class-list)))
    ret-class))
         
;----------------------------------------------------------------------------------

;;;  Initialize some of the hash table

(put-return-class 'cons
                  'cons
                  '(cons  number integer fixnum float
                    rational complex symbol))

(put-return-class 'vector
                  'vector
                  '(vector cons  number integer fixnum float
                    rational complex symbol))

(put-return-class 'array
                  'array
                  '(array vector cons  number integer fixnum float
                    rational complex symbol))

;----------------------------------------------------------------------------------

(defun dump-return-classes ()
  (declare (special *return-class-table*
                    *quail-terminal-io*))
  (maphash #'(lambda (x y) (format *quail-terminal-io* "~&~S ~S" x y))
           *return-class-table*)
  (values))
