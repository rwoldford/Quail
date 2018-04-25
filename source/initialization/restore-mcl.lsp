(in-package :qk)

;;  This file is part of the Initialization system for find-quail functionality in aclpc

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(
           ;; reset-mcl-root reset-mpw-root
            ))
  )

#|
(defun reset-mcl-root (new-root)
  (if (not new-root)
    (setf new-root (logical-pathname "Home:**;*.*")))
  (mk::reset-logical-pathname-root "ccl" new-root)
  (if (not (probe-file 
            ccl::*records-index-file*))
    (flet ((path-mac-internals ()
             (mk::append-directories
              (mk::path-foreign)
              "Macintosh internals;")))
      
      ;;  Indexes for Macintosh internals
      
      (setf ccl::*mactypes-index-file* (merge-pathnames (path-mac-internals) "mactypes.idx"))
      (setf ccl::*constants-index-file* (merge-pathnames (path-mac-internals) "constants.idx"))
      (setf ccl::*records-index-file* (merge-pathnames (path-mac-internals) "records.idx"))
      (setf ccl::*traps-index-file* (merge-pathnames (path-mac-internals) "traps.idx"))
      )))

(defun reset-mpw-root (new-root)
  (if (not new-root)
    (setf new-root (logical-pathname "Home:MPW3.2;MPW;**;*.*")))
  (mk::reset-logical-pathname-root "mpw" new-root))
|#


(defun find-quail ()
   "A dummy for now. in restore-pc there is a real job to do"
   NIL)
