;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               table-mixin.lisp                              
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1992 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     Michael Lewis 1991.
;;;     Greg Anglin 1992.
;;;     R.W. Oldford 1993, 1994.
;;;
;;;
;;;--------------------------------------------------------------------------------
;;;

(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(table-mixin ensure-table clear-table add-table-entry find-table-entry
          remove-table-entry hash-table-mixin)))



;---------------------------------------------------------------------------
;Mixin adds tables to an object.
;---------------------------------------------------------------------------

(defclass table-mixin (quail-object)
  ()
  (:documentation   
   "This mixin class allows a slot ~
   containing a table to be added ~
   to an object.  Table-mixin has no ~
   slots, only methods.  The slots are ~
   defined by subclasses."))

;---------------------------------------------------------------------------
;Generic methods for accessing the table.
;---------------------------------------------------------------------------

(defgeneric ensure-table (self slot-name &rest inits)
  (:documentation
   "Args: self, an object with tables, a ~
   slot-name, and additional initialization info for ~
   creating an empty table.  If slot-name does not contain ~
   a table, an empty one is instantiated there. ~
   Returns the table."))

(defgeneric clear-table (self slot-name)
  (:documentation
   "Args: self, an object with tables, and ~
   slot-name, the slot containing the particular ~
   table to clear."))

(defgeneric add-table-entry (self slot-name key datum)
  (:documentation
   "Args: self, an object with tables; ~
   slot-name, the slot containing the particular ~
   table; and the key to create or change an ~
   entry in the table."))

(defgeneric find-table-entry (self slot-name key)
  (:documentation
   "Args: self, an object with tables; ~
   slot-name, the slot containing the particular ~
   table; and the key to find an ~
   entry in the table."))

(defgeneric remove-table-entry (self slot-name key)
  (:documentation
   "Args: self, an object with tables; ~
   slot-name, the slot containing the particular ~
   table; and the key to erase an ~
   entry in the table."))

;---------------------------------------------------------------------------
;Mixin adds hash-tables to an object.
;---------------------------------------------------------------------------

(defclass hash-table-mixin (table-mixin)
  ()
  (:documentation
   "This mixin class allows a slot ~
   containing a hash table to be added ~
   to an object.  Hash-table-mixin has no ~
   slots, only methods.  The slots are ~
   defined by subclasses."))

;---------------------------------------------------------------------------
;Methods for accessing the hash table.
;---------------------------------------------------------------------------

(defmethod ensure-table ((self hash-table-mixin) slot-name &rest list-test)
  (or (slot-value self slot-name)
      (setf (slot-value self slot-name)
            (if list-test
              (make-hash-table :test (first list-test) :size 10)
              (make-hash-table :test #'equal :size 10)))))
        
(defmethod clear-table ((self hash-table-mixin) slot-name)
  (clrhash (ensure-table self slot-name)))

(defmethod add-table-entry ((self hash-table-mixin) slot-name key datum)
  (setf (gethash key (ensure-table self slot-name)) datum))

(defmethod find-table-entry ((self hash-table-mixin) slot-name key)
  (gethash key (ensure-table self slot-name)))

(defmethod remove-table-entry ((self hash-table-mixin) slot-name key)
  (remhash key (ensure-table self slot-name)))


