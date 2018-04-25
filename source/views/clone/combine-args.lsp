;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               combine-args.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;
;;;  This file is part of the Views system, a toolkit for interactive statistical graphics.
;;;  
;;;
;;;  Authors:
;;;     C.B. Hurley 1994 
;;;     
;;;
;;;
;;;------------------------------------------------


(in-package :views)
(eval-when (:compile-toplevel :load-toplevel :execute) (export  '( combine-subview-args)))

(defgeneric combine-subview-args(view &rest args)
  (:documentation "If any of the keys in subview-keys appear more than once in args~
   all values for that key are combined into a list"))



(defun csa-arg-p(arg)
  (and (not (eq t arg))
       (or 
        (symbolp arg) (functionp arg) ;;(not (listp arg))
           (keyword-list-p arg))
       :single))




(defun layer-arg-p(arg)
  (and (listp arg) (every #'csa-arg-p arg) :layer))

(defun csa-add-args(a b)
  (cond 
   ((and (listp a) (listp b))
    (loop for key in a by #'cddr
          for val in (cdr a) by #'cddr
          do
          (setf (getf b key) val)) 
    b)
   ((and (listp b) (symbolp a))
    (setf (getf b :type) a)
    b)
   ((and (listp a) (symbolp b))
    (unless (member :type a)
      (setf (getf a :type) b))
      a)
   (t a)))
 

(defun delete-extra-keyword-args(args)
  (loop with key-list = nil
        with k = 0
        while (< k (length args))
        for key = (elt args k) do
        (if (member key key-list)
          (setf args
                (concatenate 'list
                             (subseq args 0 k)
                             (subseq args (+ k 2))))
          (progn
            (push key key-list)
            (incf k 2))))
  args)

(defmethod combine-subview-args ((self view)  &rest args)
  
  (loop for key in (subview-keys-of self)
        for new-key-arg =
        (loop with other-vals
              for arg-val in (cdr args) by #'cddr
              for arg-key in args by #'cddr
              when (eq arg-key key) 
              do
              (cond ((csa-arg-p arg-val)
                     (setq other-vals (csa-add-args other-vals  arg-val )))
                    (t nil))
              finally  (return other-vals))
        
        when new-key-arg do
          (setf (second (member key args)) new-key-arg))
  (delete-extra-keyword-args args)
  )

(defmethod combine-subview-args ((self plot)  &rest args)
  
  (loop for key in (subview-keys-of self)
        for new-key-arg =
        (loop with other-vals with basic-type 
              for arg-val in (cdr args) by #'cddr
              for arg-key in args by #'cddr
              when (eq arg-key key) 
              do
               (cond ((csa-arg-p arg-val)
                     (setq other-vals 
                           (cond ((eq basic-type :single)
                                  (csa-add-args other-vals  arg-val ))
                                 ((eq basic-type :layer)
                                  (loop for o in other-vals collect
                                        (csa-add-args o  arg-val) ))
                                 (t (setq basic-type :single)
                                    arg-val))))
                    
                    ((layer-arg-p  arg-val)
                     (setq other-vals 
                           (cond ((eq basic-type :single) other-vals)
                                 ((eq basic-type :layer) 
                                  (if (= (length other-vals) (length arg-val))
                                    (loop for o in other-vals
                                          for a in arg-val
                                          collect (csa-add-args o a))
                                    other-vals))
                                 
                                 (t (setq basic-type :layer)
                                    arg-val))))
                    
                    (t nil))
              finally  (return other-vals))
        
        when new-key-arg do
        (setf (second (member key args)) new-key-arg))
  (delete-extra-keyword-args args) 
  )


(defmethod combine-subview-args ((self view-layout)  &rest args)
  
  (loop for key in (subview-keys-of self)
        for new-key-arg =
        (loop with other-vals with basic-type 
              for arg-val in (cdr args) by #'cddr
              for arg-key in args by #'cddr
              when (eq arg-key key) 
              do
              (cond 
               ((csa-arg-p arg-val)
                (setq other-vals 
                      (cond 
                       ((eq basic-type :single)
                        (csa-add-args other-vals  arg-val ))
                       ((eq basic-type :list)
                        (loop for o in other-vals collect
                              (csa-add-args o  arg-val) ))
                       ((and basic-type (listp basic-type))
                        (loop for other in other-vals
                              for b in basic-type
                              when b collect
                              (cond ((eq b :single)
                                     (csa-add-args other  arg-val))
                                    ((eq b :layer)
                                     (loop for o in other collect
                                           (csa-add-args o  arg-val)))
                                    (t nil))))
                       (t (setq basic-type :single)
                          arg-val))))
               
               ((layer-arg-p  arg-val)
                (setq other-vals 
                      (cond 
                       ((eq basic-type :single) other-vals)
                       ((eq basic-type :list)
                        (if (= (length other-vals) (length arg-val))
                          (loop for o in other-vals
                                for a in arg-val collect
                                (csa-add-args o a) )
                          other-vals))
                       
                       ((and basic-type (listp basic-type))
                        (loop for other in other-vals
                              for b in basic-type
                              when b
                              collect
                              (cond ((eq b :single) b)
                                    ((= (length other) (length arg-val))
                                     (loop for o in other
                                           for a in arg-val collect
                                           (csa-add-args o a) ))
                                    (t other))))
                       
                       (t (setq basic-type :list)
                          arg-val))))
               
               ((and basic-type (listp basic-type))
                (setq other-vals 
                      (loop for b in basic-type
                            for a in arg-val
                            for o in other-vals
                            when b
                            collect
                            (cond ((and (eq b :single) (csa-arg-p a ))
                                   (csa-add-args o a))
                                  ((and (eq b :layer) (layer-arg-p a))
                                   (if (= (length o) (length a))
                                     (loop for oi in o for ai in a
                                           collect (csa-add-args oi ai))
                                     o))
                                  (t o)))))
               (basic-type nil)
               ((setq basic-type
                      (loop for a in arg-val collect
                            (or (csa-arg-p a ) (layer-arg-p a))))
                (setq other-vals arg-val))
               
               (t nil))
              finally  (return other-vals))
        
        when new-key-arg do
        (setf (second (member key args)) new-key-arg))
  (delete-extra-keyword-args args)
  )



