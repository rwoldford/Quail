;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               cursor-mcl.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;
;;;  This file is part of the Window-Basics package intended to provide a uniform
;;;  window environment for Common Lisp programming.
;;;  
;;;  Authors:
;;;     R.W. Oldford 1991
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :wb)

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(make-cursor with-cursor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Note that with-cursor is already in MCL
;;;



(defun make-cursor (bitmap &key mask (hotspot (h-draw:make-point 0 0)))
  "Returns a cursor structure for use with with-cursor."
(format *terminal-io* "~&make-cursor unimplemented in X~%")
#|
  (flet ((get-scaled-raw-bitmap (bm)
           (if (= 16 (bitmap-height bm) (bitmap-width bm))
             (bitmap-host-bitmap bm)
             (let ((rbm (bitmap-host-bitmap bm))
                   (sm (h-draw::make-bitmap 0 0 15 15)))
               (rlet ((rect-rbm :rect
                                :top 0 :left 0
                                :bottom (- (bitmap-height bm) 1)
                                :right (- (bitmap-width bm) 1))
                      (rect-sbm :rect :top 0 :left 0 :bottom 15 :right 15))
                 (h-draw::copy-bits rbm sm rect-rbm rect-sbm))
               sm))))
    (ccl:make-record :cursor
                 :data (get-scaled-raw-bitmap bitmap)
                 :mask (if mask
                         (get-scaled-raw-bitmap mask)
                         (h-draw::make-bitmap 0 0 15 15))
                 :hotspot hotspot))
|#
)
