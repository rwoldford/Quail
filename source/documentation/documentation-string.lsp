;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                           documentation-string.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (C) 1992 Statistical Computing Laboratory, University Of Waterloo
;;;
;;;
;;;  Authors:
;;;     M.E. Lewis 1992.
;;;     R.W. Oldford 1994
;;;
;;;----------------------------------------------------------------------------

(in-package :quail-kernel)

;;;; GLOBAL VARIABLE

(defvar *doc-keywords* NIL 
  "Adding to this list allows for flexibility in ~
   the documentation system.")

(setf *doc-keywords* 
      (list ":root"
            ":capsule"
            ":elaboration"
            (list ":examples" ":files" ":text")
            ":references"
            ":see-also"
            #|(list ":see-also" ":constant" ":variable" ":parameter" ":function"
                  ":special-form" ":macro" ":generic-function"
                  ":method" ":built-in-class" ":structure" ":class"
                  ":package")
            |#
            ":super-topics"
            ":topics"
            ":sub-topics"
            ":returns"
            ":side-effects"
            ":package"
            ":name"
            (list ":required" ":arg")
            (list ":optional" ":arg")
            (list ":rest" ":arg")
            (list ":key" ":arg")
            (list ":aux" ":arg")
            (list ":body" ":arg")
            ))

;;;; MAIN FUNCTION

(defun interpret-quail-documentation (doc)
  (declare (special *doc-keywords*))
  (if (null doc)
    NIL ;;(interpret-quail-documentation "(:capsule No description available.)")
    (let ((doc-tree (make-key-tree *doc-keywords*)))
      (decorate doc-tree
                (root-tree doc-tree)
                (root-pedigree doc-tree)
                (protect-parenthetical-phrases
                 (read-string
                  (protect-macro-characters doc)))))))

;;;; READING AND WRITING

(defun read-string (str)
  (let (#+(not :aclpc) (read-case (readtable-case *readtable*))
       )
    (tree-apply 
     #'(lambda (x)
         (cond ((symbolp x)
                (if (equalp (package-name (symbol-package x)) 
                            "keyword")
                  (concatenate 'string
                               ":"
                               (string x))
                  (string x)))
               (t x)))
     (unwind-protect
       (progn
         #+(not :aclpc) (setf (readtable-case *readtable*) :preserve)
         (with-input-from-string (s str)
           (do ((token (read s NIL NIL)
                       (read s NIL NIL))
                (result nil (push token result)))
               ((null token) (nreverse result))
             ())))
       #+(not :aclpc) (setf (readtable-case *readtable*) read-case)
       ))))

;;;; MISCELLANEOUS FUNCTIONS 

(defun protect-macro-characters (str &aux result)
  (let ((str (format nil str)))
    (concatenate 'string
                 (dotimes (i (length str) (nreverse result))
                   (let ((ch (elt str i)))
                     (cond ((member ch '(#\Newline #\Space) :test #'char=)
                            (push ch result))
                           ((member ch '(#\) #\() :test #'char=)
                            (push #\Space result)
                            (push ch result)
                            (push #\Space result))
                           ((not (alphanumericp ch))
                            (push #\\ result)
                            (push ch result))
                           (t
                            (push ch result))))))))

(defun protect-parenthetical-phrases (seq)
  (labels ((ppp (item)
             (let ((keywords (flatten *doc-keywords*)))
               (cond ((null item) nil)
                     ((atom item) (list item))
                     ((and (stringp (car item))
                           (find (car item) keywords :test #'string=))
                      (list (mapcan #'ppp item)))
                     (t
                      (append (list "(") (mapcan #'ppp item) (list ")")))
                     ))))
    (mapcan #'ppp seq)))
