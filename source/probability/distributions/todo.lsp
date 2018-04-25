

::: Begin optimization of files after poisson  
#|
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0)))
  (with-CL-functions (+ * / - exp expt sqrt > < = /=  >= <= abs log)
