;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                                doc-tex-index.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; copyright (c) 1990 statistical computing laboratory, university of waterloo
;;;
;;;
;;;  authors:
;;;     m.e. lewis 1991.
;;;     r.w. oldford 1991.
;;;
;;;
;;;----------------------------------------------------------------------------


(in-package :quail-kernel)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(make-tex-doc-from-index)))


      
(defun make-tex-doc-from-index (index-file
                                &key
                                (destination nil)
                                (documenttype 'article)
                                (documentstyles '(fullpage))
                                (point-size 10)
                                (input-files nil))
  "Constructs a TeX file from a sorted documentation index file~
   The result is stored in the index.tex file in the documentation ~
   directory."
  
  (declare (special tex-doc-style))
  
  (setf documentstyles
        (concatenate 'list
                     (list (format nil "~apt" point-size))
                     documentstyles
                     tex-doc-style))
  (if (null destination)
    (setf destination
          (concatenate 'string
                       (directory-namestring index-file)
                       "index.tex")))
  
  (with-open-file (ifile index-file
                         :direction :input
                         :if-does-not-exist nil)
    (with-open-file (ofile destination
                           :direction :output
                           :if-does-not-exist :create)
      ;;
      ;; Set up the header of the document.
      ;;
      (format ofile "~&\\documentstyle")
      (write-tex-args ofile documentstyles)
      (format ofile "{~a}" documenttype)
      (format ofile "~&\\begin{document}")
      ;;
      ;;  Set up the input files
      ;;
      (when input-files
        (if (not (listp input-files))
          (setf input-files (list input-files)))
        (loop for file in input-files
              do (format ofile "~&\\input{~a}" file)))
      
      (do* ((i (read ifile nil nil) 
               (read ifile nil nil))
            (j (read ifile nil nil)
               (read ifile nil nil))
            (current-doc-type j))
           ((or (null i) ( null j))
            (format ofile "~&\\end{document}"))
        (when (not (eql current-doc-type j))
          (setf current-doc-type j)
          (format ofile "~&\\newpage"))
        (write-tex-header-box ofile :left i :right j))
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Some examples
;;;
;;;

#|

(make-tex-doc-from-index "melewis:quail:doc-index.lisp")
|#
