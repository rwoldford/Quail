;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               num-array-math.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1990 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     Greg Anglin 1989, 1990, 1991.
;;;
;;;
;;;--------------------------------------------------------------------------------
;;;
;;;  Includes:
;;;          (see export)
;;;

(in-package :quail-kernel)

;;;  This file creates methods for num-arrays which call extended ops directly
;;;  when we are sure that both arguments are numeric.

;------------------------------------------------------------------------------

#|

;;;  All of these operators used to have a method like this.  They differ from
;;;  the ones for specialization of b as dimensioned-ref-object by doing
;;; 
;;;    (setf (ref result) (sel b))
;;;
;;;  rather than just
;;;
;;;    (setf (ref result) b).
;;;
;;;  I can't figure why the sel was there, so I just left these methods out,
;;;  thereby inheriting from dimensioned-ref-object ... dga 91 07 16

(defmethod plus-object ((a (eql :identity)) (b num-array))
  (let ((result (make-dimensioned-result (dimensions-of b) b)))
    (setf (ref result) (sel b)).
    result))
|#

;------------------------------------------------------------------------------

(defmethod-multi plus-object ((a (symbol number num-array))
                      (b num-array))
  (map-element #'ext_+ nil a b))
                                                   
(defmethod-multi plus-object ((a num-array)
                      (b (symbol number)))
  (map-element #'ext_+ nil a b))

;------------------------------------------------------------------------------

(defmethod-multi minus-object ((a (symbol number num-array))
                      (b num-array))
  (map-element #'ext_- nil a b))
                                                   
(defmethod-multi minus-object ((a num-array)
                      (b (symbol number)))
  (map-element #'ext_- nil a b))

;------------------------------------------------------------------------------

(defmethod-multi times-object ((a (symbol number num-array))
                      (b num-array))
  (map-element #'ext_* nil a b))
                                                   
(defmethod-multi times-object ((a num-array)
                      (b (symbol number)))
  (map-element #'ext_* nil a b))

;------------------------------------------------------------------------------

(defmethod-multi divides-object ((a (symbol number num-array))
                      (b num-array))
  (map-element #'ext_/ nil a b))
                                                   
(defmethod-multi divides-object ((a num-array)
                      (b (symbol number)))
  (map-element #'ext_/ nil a b))


                                                   
         





