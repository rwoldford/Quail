;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                                doc.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (C) 1991 Statistical Computing Laboratory, University Of Waterloo
;;;
;;;
;;;  Authors:
;;;     M.E. Lewis 1991.
;;;     R.W. Oldford 1991.
;;;
;;;
;;;----------------------------------------------------------------------------


(in-package :quail-kernel)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(doc clear-doc *user-doc-path-functions*)))
;;;-----------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;;
;;;  THE routines for accessing and setting the documentation.
;;;
;;;-----------------------------------------------------------------------------

(defun set-doc (thing doc-type documentation-object)
  (setf (get thing doc-type) documentation-object))

(defsetf doc set-doc)

(defvar *user-doc-path-functions*
  NIL
  "A variable whose value is either NIL or is a list of functions of two arguments, ~
   a symbol and a doc-type (e.g. :function :macro  :topic, et cetera).  ~
   If non-NIL this list of functions will be used on a symbol and a doc-type ~
   to determine the complete path name ~
   where the documentation can be found.  ~
   Each function is tried from the beginning to the end of the list until ~
   a file is found.  ~
   If either the list is NIL (the default), or no function on it ~
   produces an existing file, then the quail system functions are tried.  ~
   This variable allows the user to access quail documentation for ~
   his or her own symbols, or even to override the provided quail~
   documentation.")

(defun set-doc-from-file (symbol doc-type)
  (declare (special *user-doc-path-functions*))
  (let* ((quail-file-functions
          ;; The default quail doc path functions
          (if (eq doc-type :topic)
            ;; topics are symbols that may not yet be in any package
            (list #'doc-general-topics-path-name
                  #'doc-path-name
                  #'doc-auto-path-name)
            (list #'doc-path-name
                  #'doc-auto-path-name)))
         (file-functions
          (if (and *user-doc-path-functions*
                   (listp *user-doc-path-functions*))
            (append *user-doc-path-functions* quail-file-functions)
            quail-file-functions)))
    (if
      ;; Is there documentation available on file?
      (loop
        for f in file-functions
        when
        ;; If successful, the following load attaches the documentation
        ;; to the symbol
        (load (funcall f symbol doc-type) :if-does-not-exist nil)
        return T)
      ;; Then use it
      (get symbol doc-type)
      ;; Else return NIL
      NIL)))

(defun clear-doc (thing &optional doc-type)
  "Clears the Quail documentation for its first argument ~
   of type given by its second argument.  If doc-type is missing, ~
   then all Quail documentation is cleared."
  (if doc-type
    (remprop thing doc-type)
    (loop for dt in (quail-doc-types)
          do (remprop thing dt))))

(defun doc (thing doc-type)
  "Accesses the quail documentation for its first argument ~
   of type given by its second argument."
  (if (symbolp thing)
    (or (get thing doc-type)
        (and (set-doc-from-file thing doc-type)
             (get thing doc-type))
        (and 
         ;; Don't want to make new topics on the fly.
         (not (eq doc-type :topic))
         (setf (doc thing doc-type)
               (make-doc thing doc-type))))
    (make-doc thing doc-type)))
