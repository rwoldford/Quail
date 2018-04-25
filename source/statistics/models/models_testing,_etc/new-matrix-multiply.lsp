;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               matrix-multiply.lisp                              
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1990 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     Greg Anglin 1992.
;;;
;;;
;;;--------------------------------------------------------------------------------
;;;

(in-package qk)

(defmacro dot-times-instantiate-target (target rows cols u v)
  (let ((dims (gensym)))
    `(let ((,dims (delete 1 (list rows cols))))
       ;; avoid instantiating a complex instance when it would be
       ;; thrown away anyway
       (if (or ,dims  ;; usual case
               (not (eql 1 ,rows))
               (not (eql 1 ,cols))
               ,target)  ;; if we're this far, target is an atom for sure.   
         (make-dimensioned-result ,dims ,u ,v)
         (make-list 1)))))

(labels 
  ((erefer (source-rank source-rows)
     (case source-rank
       (0 #'0d-eref)
       (1 (if (eql source-rows 1)
            #'1d-row-eref
            #'1d-column-eref))
       (2 #'eref)))
   (setf-erefer (target-rank target-rows)
     (case target-rank
       (0 #'0d-eset)
       (1 (if (eql target-rows 1)
            #'1d-row-eset
            #'1d-column-eset))
       (2 *setf-eref*))))
  (defun dot-times-instantiate-1 (u v &optional target)
    (declare (optimize speed))
    (let* ((udim (or (dimensions-of u) '(1)))  ;; fake out zero-dim case
           (vdim (or (dimensions-of v) '(1)))
           (u2d (eql (length udim) 2))
           (v2d (eql (length vdim) 2))
           p rows cols)
      (if (atom target)
        (progn
          (multiple-value-setq
            (rows cols p)
            ;; use m=2d matrix, c=1d column, r=1d row
            (cond
             ;; the rc and cr case
             ((and (not u2d) (not v2d))
              (cond 
               ;; rc == simple dot product
               ((eql (first udim) (first vdim))
                (values 1 1 (first udim)))
               ;; cr case
               ((or (eql (first udim) 1) (eql (first vdim) 1))
                (values (first udim) (first vdim) 1))
               ;; bomb out
               (t (error "The matrices ~S and ~S are not conformable for matrix ~
                          multiplication." u v))))
             ;; the mm and mc cases
             ((and u2d (eql (second udim) (first vdim)))
              (values (first udim) (if v2d (second vdim) 1) (second udim)))
             ;; the cm case
             ((and v2d (eql 1 (first vdim)))
              (values (first udim) (second vdim) 1))
             ;; the rm case
             ((and v2d (eql (first udim) (first vdim)))
              (values 1 (second vdim) (first udim)))
             ;; the mr case
             ((and u2d (eql 1 (second udim)))
              (values (first udim) (second vdim) 1))
             ;; bomb out
             ((or (> (length udim) 2) (> (length vdim) 2))
              (error "One of the arrays ~S or ~S has too many dimensions to be ~
                      multiplied." u v))
             (t (error "The matrices ~S and ~S are not conformable for matrix ~
                        multiplication." u v))))
          (setf target
                (dot-times-instantiate-target target rows cols u v)))
        (progn
          ;; I think the case where we have a target but no dot-times-env
          ;; will be rare, so I haven't tried to optimize this part of the
          ;; code.  At least the math ops are the CL ones.
          (setf p (isqrt (/ (* (apply #'* udim)
                               (apply #'* vdim))
                            (* (apply #'* (dimensions-of target))))))
          (setf rows (/ (apply #'* udim) p))
          (setf cols (/ (apply #'* vdim) p))))
      (values target
              rows
              cols
              p
              (setf-erefer (array-rank target) rows)
              (erefer (array-rank u) rows)
              (erefer (array-rank v) p)))))

;; the interface for clever users.
(defun dot-times-instantiate (u v &optional target)
  (declare (optimize speed))
  (let (rows cols p target-setf-eref u-eref v-eref)
    (multiple-value-setq (target rows cols p 
                                 target-setf-eref u-eref v-eref)
      (dot-times-instantiate-1 u v target))
    (values target
            (list rows cols p target-setf-eref u-eref v-eref))))
 
(defun dot-times-compute (u v &optional
                            (target nil target-provided-p)
                            dot-times-env
                            (plus-op #'ext_+)
                            (times-op #'ext_*))
  (declare (optimize speed))
  (let* (accum rows cols p target-setf-eref u-eref v-eref)
    (cond (dot-times-env
           (multiple-value-setq (rows cols p 
                                      target-setf-eref u-eref v-eref)
             (apply #'values dot-times-env))
           (if (atom target)
             (setf target
                   (dot-times-instantiate-target target rows cols u v))))
          (t
           (multiple-value-setq (target rows cols p 
                                        target-setf-eref u-eref v-eref)
             (dot-times-instantiate-1 u v target))))
    ;; On the Mac anyway, loop is faster than dotimes !!!
    ;; ... and do is faster than loop for straight iteration. CW.
    ; (loop for ri from 0 to (1- rows) do
    (do ((ri 0 (incf fi)))
        ((= ri rows))
      ; (loop for ci from 0 to (1- cols) do
      (do ((ci 0 (incf ci)))
          ((= ci cols))
        (funcall target-setf-eref
                 ;; new-value
                 (progn
                   ;; this structure is a little ugly, but it saves one addition
                   ;; for each time around ie. saves rows * cols additions in
                   ;; total
                   (setf accum (funcall times-op
                                        (funcall u-eref u ri 0)
                                        (funcall v-eref v 0 ci)))
                   ; (loop for pp from 1 to (1- p) do
                   (do ((pp 1 (incf pp)))
                       ((= pp p))
                     (setf accum 
                           (funcall
                            plus-op
                            accum
                            (funcall 
                             times-op
                             (funcall u-eref u ri pp)
                             (funcall v-eref v pp ci)))))
                   accum)
                 ;; rest
                 target ri ci)))
    (if (and (eql rows 1)
             (eql cols 1)
             (not target-provided-p))
      (eref target 0)      ;;; in this case, target's a list of length 1 (fastest)
      target)))