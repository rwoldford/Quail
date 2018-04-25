;;; ****************************** do-benchmark.lisp ******************************

#+:ccl
(progn
  (compile-file "ccl;Z:Z Source:Z Kernel:Z Test:benchmark.lisp")
  (load "ccl;Z:Z Source:Z Kernel:Z Test:benchmark.fasl"))

#+(and :aclunix (not :aclpc))
(load "benchmark" 
      :search-list (:first 
                    (:newest-do-compile
                     #.(make-pathname 
                        :directory "/usr/people/dganglin/lisp/new/z-test/"
                        :type "fasl"))
                    (:newest-do-compile
                     #.(make-pathname 
                        :directory "/usr/people/dganglin/lisp/new/z-test/"
                        :type "lisp"))))

;----------------------------------------------------------------------------------
