;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                           format-tree.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (C) 1992 Statistical Computing Laboratory, University Of Waterloo
;;;
;;;
;;;  Authors:
;;;     M.E. Lewis 1992.
;;;     R.W. Oldford 1993. 
;;;
;;;
;;;----------------------------------------------------------------------------

(in-package :quail-kernel)

;;; Formatting nodes

(defvar *node-value-formatter* NIL)

(setf *node-value-formatter* (make-hash-table :test #'equalp))

(defmethod initialize-instance :after ((me key-tree)
                                       &key)
  (flet ((install-pedigree (node)
           (with-slots (pedigree) node
             (setf pedigree
                   (family-tree me node))))
         (install-show (node)
           (with-slots (pedigree value show) node
             (let ((form (gethash pedigree *node-value-formatter*)))
               (if form
                 (setf show (funcall form node))
                 (setf show (default-node-value-formatter node)))))))
    (mapc #'install-pedigree (nodes me))
    (mapc #'install-show (nodes me)))
  me)


;;; Default formatting function

(defun default-node-value-formatter (node)
  #'(lambda ()
      (concatenate-with-separators
       (reverse (car (value node))))))

;;; Formatting utility functions

(defun concatenate-with-separators
       (string-list)
  (apply #'concatenate 'string
         (first string-list)
         (mapcar #'(lambda (x)
                     (concatenate 'string
                                  " "
                                  (if (stringp x)
                                    x
                                    (format NIL "~s" x))))
                 (cdr string-list))))

;;;; formatting functions appropriate for individual nodes

(setf (gethash '(":see-also" ":root") *node-value-formatter*)
      #'(lambda (node)
          #'(lambda ()
              (with-input-from-string (s
                                       (concatenate-with-separators
                                        (reverse (car (value node)))))
                (do ((token (read s NIL NIL)
                            (read s NIL NIL))
                     (result nil (push token result)))
                    ((null token) (nreverse result))
                  ;()
                  )
                ))))
      #|
      #'(lambda (node)
          #'(lambda ()
              (mapcar #'(lambda (x)
                          (intern (string-upcase x)))
                      (flatten (value node))))))
|#

(setf (gethash '(":files" ":examples" ":root") *node-value-formatter*)
      #'(lambda (node)
          #'(lambda ()
              (let ((result (reverse (car (value node)))))
                (loop for i from 0 to (- (length result) 1)
                      collect
                      (cond
                       ((string-equal (elt result i) "(")
                        (incf i)
                        (do ((next-string (elt result i) (elt result i))
                             (sub-list NIL (push next-string sub-list)))
                            ((string-equal next-string ")")
                             (nreverse sub-list))
                          (incf i)))
                       (t (elt result i))))))))


(setf (gethash '(":constant" ":see-also" ":root") *node-value-formatter*)
      #'(lambda (node)
          #'(lambda ()
              (mapcar #'(lambda (x)
                          `(,(intern (string-upcase x))
                            ,(intern "CONSTANT" :keyword)))
                      (flatten (value node))))))

(setf (gethash '(":variable" ":see-also" ":root") *node-value-formatter*)
      #'(lambda (node)
          #'(lambda ()
              (mapcar #'(lambda (x)
                          `(,(intern (string-upcase x))
                            ,(intern "VARIABLE" :keyword) ))
                      (flatten (value node))))))

(setf (gethash '(":parameter" ":see-also" ":root") *node-value-formatter*)
      #'(lambda (node)
          #'(lambda ()
              (mapcar #'(lambda (x)
                          `(,(intern (string-upcase x))
                            ,(intern "PARAMETER" :keyword) ))
                      (flatten (value node))))))

(setf (gethash '(":function" ":see-also" ":root") *node-value-formatter*)
      #'(lambda (node)
          #'(lambda ()
              (mapcar #'(lambda (x)
                          `(,(intern (string-upcase x))
                            ,(intern "FUNCTION" :keyword) ))
                      (flatten (value node))))))

(setf (gethash '(":special-form" ":see-also" ":root") *node-value-formatter*)
      #'(lambda (node)
          #'(lambda ()
              (mapcar #'(lambda (x)
                          `(,(intern (string-upcase x))
                            ,(intern "SPECIAL-FORM" :keyword) ))
                      (flatten (value node))))))

(setf (gethash '(":macro" ":see-also" ":root") *node-value-formatter*)
      #'(lambda (node)
          #'(lambda ()
              (mapcar #'(lambda (x)
                          `(,(intern (string-upcase x))
                            ,(intern "MACRO" :keyword) ))
                      (flatten (value node))))))

(setf (gethash '(":generic-function" ":see-also" ":root") *node-value-formatter*)
      #'(lambda (node)
          #'(lambda ()
              (mapcar #'(lambda (x)
                          `(,(intern (string-upcase x))
                            ,(intern "GENERIC-FUNCTION" :keyword) ))
                      (flatten (value node))))))

(setf (gethash '(":method" ":see-also" ":root") *node-value-formatter*)
      #'(lambda (node)
          #'(lambda ()
              (mapcar #'(lambda (x)
                          `(,(intern (string-upcase x))
                            ,(intern "METHOD" :keyword) ))
                      (flatten (value node))))))

(setf (gethash '(":built-in-class" ":see-also" ":root") *node-value-formatter*)
      #'(lambda (node)
          #'(lambda ()
              (mapcar #'(lambda (x)
                          `(,(intern (string-upcase x))
                            ,(intern "BUILT-IN-CLASS" :keyword) ))
                      (flatten (value node))))))

(setf (gethash '(":structure" ":see-also" ":root") *node-value-formatter*)
      #'(lambda (node)
          #'(lambda ()
              (mapcar #'(lambda (x)
                          `(,(intern (string-upcase x))
                            ,(intern "STRUCTURE" :keyword) ))
                      (flatten (value node))))))

(setf (gethash '(":class" ":see-also" ":root") *node-value-formatter*)
      #'(lambda (node)
          #'(lambda ()
              (mapcar #'(lambda (x)
                          `(,(intern (string-upcase x))
                            ,(intern "CLASS" :keyword) ))
                      (flatten (value node))))))

(setf (gethash '(":package" ":see-also" ":root") *node-value-formatter*)
      #'(lambda (node)
          #'(lambda ()
              (mapcar #'(lambda (x)
                          `(,(intern (string-upcase x))
                            ,(intern "PACKAGE" :keyword) ))
                      (flatten (value node))))))

(setf (gethash '(":arg" ":required" ":root") *node-value-formatter*)
      #'(lambda (node)
          #'(lambda ()
              (mapcar #'(lambda (x)
                          (list
                           (first x)
                           (concatenate-with-separators (rest x))))
                      (mapcar #'reverse (value node))))))

(setf (gethash '(":arg" ":optional" ":root") *node-value-formatter*)
      #'(lambda (node)
          #'(lambda ()
              (mapcar #'(lambda (x)
                          (list
                           (first x)
                           (second x)
                           (concatenate-with-separators (rest (rest x)))))
                      (mapcar #'reverse (value node))))))

(setf (gethash '(":arg" ":rest" ":root") *node-value-formatter*)
      #'(lambda (node)
          #'(lambda ()
              (mapcar #'(lambda (x)
                          (list
                           (first x)
                           (second x)
                           (concatenate-with-separators (rest (rest x)))))
                      (mapcar #'reverse (value node))))))

(setf (gethash '(":arg" ":key" ":root") *node-value-formatter*)
      #'(lambda (node)
          #'(lambda ()
              (mapcar #'(lambda (x)
                          (list
                           (first x)
                           (second x)
                           (concatenate-with-separators (rest (rest x)))))
                      (mapcar #'reverse (value node))))))

(setf (gethash '(":arg" ":aux" ":root") *node-value-formatter*)
      #'(lambda (node)
          #'(lambda ()
              (mapcar #'(lambda (x)
                          (list
                           (first x)
                           (second x)
                           (concatenate-with-separators (rest (rest x)))))
                      (mapcar #'reverse (value node))))))

(setf (gethash '(":arg" ":body" ":root") *node-value-formatter*)
      #'(lambda (node)
          #'(lambda ()
              (mapcar #'(lambda (x)
                          (list
                           (first x)
                           (second x)
                           (concatenate-with-separators (rest (rest x)))))
                      (mapcar #'reverse (value node))))))

;;;; various formatting functions

(defun find-and-format (key-tree trail)
  (if key-tree
    (funcall 
     (show
      (find-node key-tree trail)))))

(defun get-doc-capsule (key-tree)
  (or (find-and-format key-tree '(":capsule" ":root"))
      (get-narrative key-tree)))

(defun get-doc-elaboration (key-tree)
  (find-and-format key-tree '(":elaboration" ":root")))

(defun get-examples (key-tree)
  (let ((text (find-and-format key-tree '(":text" ":examples" ":root")))
        (files (find-and-format key-tree '(":files" ":examples" ":root")))
        (root (find-and-format key-tree '(":examples" ":root"))))
    (if (or text files root)
      (list (if (and text (not (string-equal text "")))
              (cons :text text))
            (if (and root (not (string-equal root "")))
              (cons :root root))
            (if files (cons :files 
                            (loop for file-info in files
                                  collect
                                  (list (format NIL "~{~a ~}"
                                                (if (listp file-info)
                                                  (butlast file-info)
                                                  (list file-info)))
                                        (first (if (listp file-info)
                                                  (last file-info)
                                                  (list file-info)))))))))))

(defun get-references (key-tree)
  (find-and-format key-tree '(":references" ":root")))

(defun get-see-also (key-tree)
  (find-and-format key-tree '(":see-also" ":root")))
  #|(mapcan #'(lambda (x)
              (find-and-format key-tree x))
          '((":see-also" ":root") (":constant" ":see-also" ":root")
            (":variable" ":see-also" ":root") (":parameter" ":see-also" ":root") 
            (":function" ":see-also" ":root") (":special-form" ":see-also" ":root") 
            (":macro" ":see-also" ":root") (":generic-function" ":see-also" ":root")
            (":method" ":see-also" ":root") (":built-in-class" ":see-also" ":root")
            (":structure" ":see-also" ":root") (":class" ":see-also" ":root") 
            (":package" ":see-also" ":root"))))
   |#

(defun get-side-effects (key-tree)
  (find-and-format key-tree '(":side-effects" ":root")))

(defun get-super-topics (key-tree)
  (find-and-format key-tree '(":super-topics" ":root")))

(defun get-sub-topics (key-tree)
  (find-and-format key-tree '(":sub-topics" ":root")))

(defun get-topic (key-tree)
  (find-and-format key-tree '(":topic" ":root")))

(defun get-returns (key-tree)
  (find-and-format key-tree '(":returns" ":root")))

(defun get-narrative (key-tree)
  (find-and-format key-tree '(":root")))

(defun get-required-arg (key-tree)
  (find-and-format key-tree '(":arg" ":required" ":root")))

(defun get-rest-arg (key-tree)
  (find-and-format key-tree '(":arg" ":rest" ":root")))

(defun get-key-arg (key-tree)
  (find-and-format key-tree '(":arg" ":key" ":root")))

(defun get-optional-arg (key-tree)
  (find-and-format key-tree '(":arg" ":optional" ":root")))

(defun get-aux-arg (key-tree)
  (find-and-format key-tree '(":arg" ":aux" ":root")))

(defun get-body-arg (key-tree)
  (find-and-format key-tree '(":arg" ":body" ":root")))







 


