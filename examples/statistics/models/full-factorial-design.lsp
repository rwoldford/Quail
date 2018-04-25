(in-package q-user)

(defun code-an-effect (design column levels inner-reps outer-reps)
  (let ((count 0)
        (design-col (ref design t column)))
    (loop repeat outer-reps
          do
          (loop for lev in levels
                do (loop repeat inner-reps
                         do (setf (eref design-col count) lev)
                         (incf count))))))

(defun full-factorial-design (&rest levels-list)
  (let* ((levels-numlevels (mapcar #'length levels-list))
         (numvars (length levels-list))
         (numruns (apply #'* levels-numlevels))
         (design (array 0 :dimensions (list numruns numvars)))
         inner-reps)
    (loop for current-column upfrom 0
          as levels in levels-list
          as rem-levels on levels-numlevels
          do 
          (setf inner-reps (apply #'* (rest rem-levels)))
          (code-an-effect design
                          current-column
                          levels
                          inner-reps
                          (/ numruns inner-reps (first rem-levels))))
    design))
          
          
