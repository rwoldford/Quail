;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                                tex-basic.lisp                               
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

(eval-when (:compile-toplevel :load-toplevel :execute) (export '()))

;;;----------------------------------------------------------------------------
;;;
;;;  Writing lisp values as TeX commands with escapes
;;;  for TeX's special characters
;;;
;;;----------------------------------------------------------------------------

(defvar *tex-special-char*
  '(#\# #\$ #\% #\& #\~ #\_ #\^ #\\ #\{ #\} #\| #\< #\>)
  "Characters that must be escaped to be understood by TeX.")

(defun tex-special-char-p (char)
  "Test whether argument is a special character in TeX."
  (declare (special *tex-special-char*))
  (if (member char *tex-special-char* :test #'char=)
    T
    NIL))

(defun write-tex-value (destination value)
  "Writes the value to the destination inserting a TeX escape ~
   character in front of anything that is a TeX special character."
    (with-input-from-string (ifile (if (stringp value)
                                     value
                                     (format NIL "~s" value)))
      (do ((next-char (read-char ifile nil nil)
                      (read-char ifile nil nil)))
          ((null next-char))
        (cond ((tex-special-char-p next-char)
               (write-char #\\ destination)
               (write-char next-char destination))
              (t
               (write-char next-char destination))))))


;;;------------------------------------------------------------------------------
;;;
;;;  TeX arg lists
;;;
;;;------------------------------------------------------------------------------

(defun write-tex-args (destination &optional (args NIL))
  "Writes the second argument as ~
   items in a TeX arglist (i.e. as [item-1, item-3, ..., item-n])."
  (if args
    (do ((rest-of-args args (cdr rest-of-args))
         (char "[" ", "))
        ((null rest-of-args)
         (format destination "]"))
      (format destination char)
      (write-tex-value destination (car rest-of-args)))
    (format destination "[]")))


;;;------------------------------------------------------------------------------
;;;
;;;  TeX environments
;;;
;;;-----------------------------------------------------------------------------

(defun write-tex-begin-env (destination environment)
  "Writes the TeX commands to destination that are necessary to begin ~
   a TeX environment of the specified type."
  (format destination "~&\\begin{~a} ~%" environment))


(defun write-tex-end-env (destination environment)
  "Writes the TeX commands to destination that are necessary to begin ~
   a TeX environment of the specified type (default itemize)."
  (format destination "~&\\end{~a} ~%" environment))


;;;-----------------------------------------------------------------------------
;;;
;;; TeX Lists
;;;
;;;----------------------------------------------------------------------------


(defun write-tex-begin-list (destination &key (type 'itemize))
  "Writes the TeX commands to destination that are necessary to begin ~
   a TeX list of the specified type (default itemize)."
  (write-tex-begin-env destination type))


(defun write-tex-end-list (destination &key (type 'itemize))
  "Writes the TeX commands to destination that are necessary to begin ~
   a TeX list of the specified type (default itemize)."
  (write-tex-end-env destination type))

(defun write-tex-item (destination item)
  "Writes the TeX commands to destination that are necessary to produce ~
   a single item of a TeX list."
  (format destination "~&\\item ~%")
  (write-tex destination item))

(defun write-tex-list (destination
                       &key (type 'itemize)
                            (items nil))
  "Writes the TeX commands necessary to produce a TeX list ~
   of the given type (default itemize) of items."
  
  (write-tex-begin-list destination :type type)
  (loop for item in items do (write-tex-item destination item))
  (write-tex-end-list destination :type type))

