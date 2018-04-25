;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               release.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1992 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1992.
;;;
;;;
;;;--------------------------------------------------------------------------------

(in-package :quail-kernel)

(eval-when (:compile-toplevel :load-toplevel :execute) 
    (export '(*months* *days* current-day current-month current-date
          get-quail-release-info auto-doc-release
          make-release quail-release)))

;--------------------------------------------------------------------------------

(defconstant *months*
  '("January" "February" "March" "April" "May" "June" "July" "August" "September"
    "October" "November" "December")
  "A list containing the names of the twelve months of the year.")

(defconstant *days*
  '("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday")
  "A list containing the days of the week beginning with Monday.")

(defun current-month ()
  "Returns the current calendar month"
  (declare (special *months*))
  (elt *months* (- (elt (multiple-value-list (get-decoded-time)) 4) 1)))

(defun current-day ()
  "Returns the current day of the week."
  (declare (special *days*))
  (elt *days* (elt (multiple-value-list (get-decoded-time)) 6)))

(defun current-date ()
  "Returns the current date as a string: Dayname month day, year."
  (declare (special *months* *days*))
  (let* ((date-info (multiple-value-list (get-decoded-time)))
         (month (elt *months* (- (elt date-info 4) 1)))
         (day-number (elt date-info 3))
         (day-name (elt *days* (elt date-info 6)))
         (year (elt date-info 5)))
    (concatenate 'string
                 day-name
                 ", "
                 month
                 " "
                 (format nil "~s" day-number)
                 ", "
                 (format nil "~s" year))))

(defun get-quail-release-info ()
  "Returns a list containing the current Quail release information: ~&~
   (version distribution date *quail-systems* *features*)."
  (let ((fname (mk::quail-release-file)))
    (if (probe-file fname)
      (with-open-file (ofile fname :direction :input)
        (read ofile)))))

(defun put-quail-release-info (release-info)
  "Stores the given Quail release information."
  (with-open-file (ofile (mk::quail-release-file)
                             :direction :output
                             :if-exists :supersede
                             :if-does-not-exist :create)
        (write release-info :stream ofile)))

(defun quail-release ()
  "Returns a string giving the current Quail release identification."
  (let* ((info (get-quail-release-info))
         (version (first info))
         (type (second info))
         (date (third info))
         release)
    (if type
      (setf release (concatenate 'string 
                                 (format nil "~s" version) 
                                 (case type
                                   (:alpha "(alpha)")
                                   (:beta "(beta)")
                                   (t (format nil "~S" type)))))
      (setf release version))
    (format NIL "Quail Release ~a  ~a" release date)))

(defvar *quail-release-printed* t)

;;  MCL seems to do the restore functions twice.  This should 
;;  avoid printing the release info more than once, but I can't
;;  seem to get this to happen properly.

(defun quail-release-print ()
  (or *quail-release-printed*
      (format *terminal-io* "~&~A" (quail-release)))
  (setf *quail-release-printed* t))

;; this is useful if user makes an image of their own

(defun quail-release-unprint ()
  (setf *quail-release-printed* nil))

(defun auto-doc-release (&key (auto-index? T)
                              (auto-topics? T)
                              (auto-doc-symbols? T))
  "Saves the current Quail image in the named file and declares it a release.~
   (:key ~
   (:arg auto-index? T Non-NIL if the indices of external variables are to be produced ~
    for major Quail packages: Quail, Quail-Kernel, Views, and Window-Basics.)~
   (:arg auto-topics? T Non-NIL if the Quail source files are to be used to produce ~
   topic documentation automatically.) ~
   (:arg auto-doc-symbols? T Non-NIL if the external symbols of the Quail package ~
   are to have documentation files automatically produced for them.)~
   )~
   "
  (let ((packages-to-doc (list :quail :quail-kernel :window-basics :views)))
    ;; auto-index?
    (when auto-index?
      (loop for package in packages-to-doc
            do
            (vw:inform-user
             (format NIL "Creating sorted index for the package ~s." package))
            (make-sorted-documentation-index :package package)
            (vw:inform-user (format NIL "Finished index for ~s" package))
            )
      )
    ;; auto-topics?
    (when auto-topics?
      (vw:inform-user "Creating topics for the Quail system.")
      (auto-create-and-file-topics)
      (vw:inform-user "Finished topics for the Quail system.")
      )
    ;; auto-doc-symbols?
    (when auto-doc-symbols?
      (loop for package in packages-to-doc
            do
            (vw:inform-user
             (format NIL "Creating documentation for all external symbols~
                          of the package ~s."
                     package))
            (document-symbols :package package
                              :types '(:macro :function :generic-function
                                       :special-form :constant :variable
                                       :topic :structure :built-in-class
                                       :package))
            (vw:inform-user
             (format NIL "Finished documenting symbols for ~s." package))
            )
      )
    (vw:inform-user "Done.")
    )
  )

(defun make-release (&key (file NIL)
                          (version NIL)
                          (distribution NIL)
                          (auto-index? NIL)
                          (auto-topics? NIL)
                          (auto-doc-symbols? NIL)
                          (update? NIL))
  "Saves the current Quail image in the named file and declares it a release.~
   (:key ~
   (:arg file The name to be given to the Quail image file.) ~
   (:arg version NIL Version number if non-NIL.  If NIL, the version number ~
   of the last release is used.) ~
   (:arg distribution NIL Breadth of distribution: :alpha for developers, ~
   :beta for beta-test sites, and NIL for true releases.)
   (:arg auto-index? NIL Non-NIL if the indices of external variables are to be produced ~
   for major Quail packages: Quail, Quail-Kernel, Views, and Window-Basics.)~
   (:arg auto-topics? NIL Non-NIL if the Quail source files are to be used to produce ~
   topic documentation automatically.) ~
   (:arg auto-doc-symbols? NIL Non-NIL if all external symbols of the Quail package ~
   are to have documentation files automatically produced for them.)~
   (:arg update? NIL Non-NIL if the release information is to be updated on file.)~
   )~
   "
  (declare (special *features* mk::*quail-systems*))
  
  (auto-doc-release :auto-index? auto-index?
                    :auto-topics? auto-topics?
                    :auto-doc-symbols? auto-doc-symbols?)
  (vw:inform-user "TIME TO MAKE THE IMAGE.")
  (let ((release-info (get-quail-release-info))
        old-version
        (date (current-date)))
    (when release-info
      (unless version
        (setf old-version  (first release-info))
        (if (numberp old-version)
          (setf version (float
                         (/ (round (+ old-version 0.01) 0.01)
                            100)))
          (setf version (format nil "~s+" old-version)))))
    
    (if update?
      (put-quail-release-info
       (list  version distribution date mk::*quail-systems* *features*)))
    (setf vw::*info-window* NIL)
    (setf vw::*info-text* NIL)
    (save file)
    ))


