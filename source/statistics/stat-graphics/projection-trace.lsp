;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               projection-trace.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1990 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1991.
;;;
;;;
;;;----------------------------------------------------------------------------
;;;

(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(projection-trace prompt-projection-trace)))

(defun projection-trace (&rest keyword-args
                               &key data
                               (viewed-objects (list-cases data))
                               (domain '(0.0 1.0))
                               (projection-fun :andrews)
                               (title NIL)
                               (bottom-label "Projection domain")
                               (left-label "Projected value")
                               n-ordinates (nlines 100)
                               &allow-other-keys)
  "Produces a projection trace for the data set specified by ~
   the keyword argument data. ~
   The projection vector is specified by the value of the keyword ~
   projection-fun.  Currently supported projection-funs are :andrews, :tukey, ~
   :parallel-coordinates, and a user-defined function.  ~
   If user supplies a function it is applied to each case in the ~
   dataset, data, to return a function for that case."
  (unless data (setf data (choose-dataset)))
  (unless (typep data 'matrix)
    (quail-error "Sorry the data must be given as a matrix, not as ~
                  ~s" data))
  (let*
    ((dims (dimensions-of data))
     (n (or (first dims) 1))
     (p (or (second dims) 1))
     proj-vector fun-maker)
    
    (unless (= n (length viewed-objects))
      (quail-error "Number of viewed-objects is ~s /= ~s, the number of data points!"
                   (length viewed-objects) n))
    
    (if (functionp projection-fun)
      ;; The user supplied function is to be used.
      (setf fun-maker projection-fun)
      ;; else use one of our built in projection-funs
      (if 
        (null
         (case projection-fun
           
           (:andrews
            ;; Set up the original Andrews plot.
            
            (setf proj-vector
                  (cons (/ 1.0 (sqrt 2.0))
                        (loop for i from 2 to p collect
                              (if (evenp i)
                                `(sin (* pi t_i , i))
                                `(cos (* pi t_i , (- i 1)))))))
            (setf fun-maker
                  #'(lambda (x)
                      `#'(lambda (t_i)
                           (declare (special pi))
                           (.* (tp (list ,@proj-vector )) ,x))))
            (if (null title) (setf title "Andrews function plot")))
           (:tukey
            
            ;; Set up J.W. Tukey's suggested modification.
            
            (setf proj-vector
                  (loop for i from 1 to p collect
                        `(cos (* t_i 2.0 pi (sqrt , i)))))
            (setf fun-maker
                  #'(lambda (x)
                      `#'(lambda (t_i)
                           (declare (special pi))
                           (let ((p-vector (list ,@proj-vector )))
                             (/ (.* (tp p-vector) ,x)
                                (sqrt (.* (tp p-vector) p-vector)))))))
            (if (null title) (setf title "Tukey's function plot")))
           (:parallel-coordinates
            
            ;; Set up a parallel coordinate plot.
            (let (mins );;maxs)
              (setf mins (reduce-slices #'min data :slices 0 ))
              ;;(setf maxs (reduce-slices #'max data :slices 0 ))
              (setf data (- data mins))
              (setf proj-vector
                    (loop for i from 1 to p collect
                          `(cos (* t_i 2.0 pi (sqrt , i)))))
              (setf fun-maker
                    #'(lambda (x)
                        `#'(lambda (t_i)
                             (declare (special pi))
                             (let ((p-vector (list ,@proj-vector )))
                               (/ (.* (tp p-vector) ,x)
                                (sqrt (.* (tp p-vector) p-vector)))))))
              (if (null title) (setf title "Parallel coordinate plot"))))))
        (quail-error "Projection-trace of projection-fun ~s is unknown"
                     projection-fun)))
    
    (if (null n-ordinates)
      (setf n-ordinates nlines))
    
    (let* ((vec (ref data 0))
           (the-plot
            (apply
             #'function-plot
             :nlines n-ordinates
             :left-label (label :text left-label :orientation :vertical)
             :bottom-label (label :text bottom-label)
             :title title
             :domain  domain
             :axis :x
             :function
             (eval (funcall fun-maker vec))
             :viewed-object (first viewed-objects)
             keyword-args)))
      (when (> n 1)
        (setf (viewed-object-of the-plot) data)
        (loop for i from 1 to (- n 1)
              as vo in (rest viewed-objects)
              do
              (let ((vec (ref data i)))
                (add-function (interior-view-of the-plot)
                              :function
                              (eval (funcall fun-maker vec))
                              :viewed-object vo
                              :nlines n-ordinates
                              ))))
      (loop for v in (interior-views-of the-plot) do
            (setf (vw::linkable-view-p v) t))
      (if (eq projection-fun :parallel-coordinates)
        (dotimes (i (choose p 2))
          (add-line (interior-view-of the-plot)
                    :orientation :vertical :intercept i))))))
                             
 

(defun prompt-projection-trace (fn &key data vars cases &allow-other-keys)
   (let* ((sel-plots (top-views)))
    (setq data (or  data (selected-dataset sel-plots) (choose-dataset)))
     (setq cases (or cases (or (list-plot-cases sel-plots :highlit? t)
                       (list-plot-cases sel-plots)))))
   (multiple-value-setq (data cases vars)
      (prompt-plot-args data cases vars))
  (let ((new-data data)
         (ids nil) new-cases new-values
        (variates (list-variates data)))
    (if (eql vars :prompt)
      (setq vars (choose-some-variables data 2)))
            
    (setq vars (or vars variates))
   (loop for c in (or cases (list-cases data))
                for vals = (value-of c vars :vars variates)
                when (every #'numberp vals)
                collect c into cases-loc and
                collect vals into values-loc
                and collect (identifier-of c) into ids-loc
                finally (setq new-cases cases-loc new-values values-loc ids ids-loc))
    (setq new-data (array new-values))
     (dataset   new-data :identifiers ids :variates vars :save? nil)

   (q::projection-trace
    :projection-fun fn
    :data new-data
    :viewed-objects new-cases
    :domain 
    (loop for l in
          (wb::collect-input
           (list (cons "from" "0.0")
                 (cons "to" "1.0")))
          collect
          (eval (read-from-string (cdr l))))
    :nlines
    (wb:prompt-user
     :result-type 'number ;24NOV2024
     :read-type :eval
     :prompt-string
     "Number of line segments to use to plot the ~
      each function."
     :initial-string "100")
    :left-view t :bottom-view t
    )))
