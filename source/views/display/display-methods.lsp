;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  
;;;                            display-methods.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;  
;;;
;;;  Authors:
;;;     R.W. Oldford 1993, 1995
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :views)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(dataset-view)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;;   DISPLAY default behaviours
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod display ((thing T)
                    &rest keyword-args &key (draw? T)
                    &allow-other-keys)
  "A not very clever default display that just shows the printed representation ~
   of thing in a text-view."
  (apply #'text-view :viewed-object thing
                  :text (format NIL "~&~s" thing)
                  :draw? draw? keyword-args)
  )

(defmethod display :around ((thing T)
                            &rest keyword-args
                            &key
                            (viewed-object NIL viewed-object?)
                            (draw? T)
                            (new? NIL)
                            (as-dataset? NIL)
                            (signposts? NIL)
                            &allow-other-keys)
  (if (and as-dataset? (dataset-p thing))
    (apply #'dataset-view :data thing keyword-args)
    (if signposts?
      (let ((result (apply #'call-next-method thing
                           :new? new?
                           :draw? NIL
                           :signposts? NIL
                           keyword-args))
            (signposts (apply #'signposts thing :draw? NIL keyword-args))
            (hline1 (line :intercept 0.5 :slope 0 :draw? NIL))
            (hline2 (line :intercept 0.5 :slope 0 :draw? NIL))
            signposts-view)
        (unless viewed-object? (setf viewed-object thing))
        (setf (viewed-object-of hline1) viewed-object)
        (setf (viewed-object-of hline2) viewed-object)
        (setf
         signposts-view
         (cond 
          ((and signposts (listp signposts))
           (grid-layout :ncols 5 :subviews signposts :draw? NIL
                        :box-views? NIL :gap-x .05 :gap-y .05
                        :viewed-object viewed-object))
          (signposts signposts)))
        (if signposts-view
          (apply #'view-layout
                 :viewed-object viewed-object
                 :subviews (list result hline1 signposts-view hline2)
                 :positions '((0 100 20 100)
                              (0 100 18 20)
                              (10 90 2 18)
                              (0 100 0 2))
                 :draw? draw?
                 keyword-args)
          signposts))
      (call-next-method))
    )
  )
