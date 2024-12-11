;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                         quail-menus-sblx.lsp                            
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1991 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     G.W. Bennett 2024.
;;;
;;;--------------------------------------------------------------------------------
;;; McCLIM makes menus in a way which does not seem to fit easily with the
;;; Quail code for making menus under Allegro Common Lisp and 
;;; MacLisp- see source/window-basics/menus/menu.lsp
;;; 
;;; McCLIM uses command-tables to hold commands, which play the role of menu-items,
;;; and other command-tables, which play the role of sub-menus.
;;;
;;; In Quail's menus there may be several menu-items known as black-color.
;;; Here GWB has chosen to codify the particular position of the corresponding
;;; command using Quail's menu structure. For example, there are commands
;;; com-Black-QEHWBSB and com-Black-QEHWPCSB among other com-Blacks.
;;;
;;; Initially Quail expects to show menus for Quail and Plots on its menu-bar
;;; These correspond to the McCLIM command-tables q-command-table and p-command-table.
;;; When displays derived from Quail's canvas class, Quail expects to add a menu
;;; for Canvas along side the other two. There is, therefore, a c-command-table too.

;;; It is (apparently) not possible to add something like the c-command-table to
;;; a menu-bar which has been instantiated with q-command-table and p-command-table.
;;; However, it is possible to replace the command table of q- and p- command-tables
;;; with one holding q- ,p- , and c- command-tables.

;;; Accordingly there are several files in which command-tables are defined.
;;; All are in the source/top-level directory:
;;; quail-menu-sblx.lsp  makes only the Quail menu through the q-command-table
;;; plots-menu-sblx.lsp  makes only the Plots menu through the p-command-table
;;; canvas-menu-sblx.lsp  makes on the Canvas menu through the c-command-table
;;; quail-plots-menu-sblx.lsp  makes the table for Quail and Plots menus
;;; quail-plots-canvas-menus-sbls.lsp  makes the table for Quail, Plots, and Canvas
;;; change-menu-bar-sblx.lsp  contains code for replacing a command-table

(in-package :wb) ;(in-package :quail)
(eval-when (:compile-toplevel :load-toplevel :execute) (export '(q-command-table)))

;;; START Utilities
(defun thread-from-name (name)
	(dolist (x (sb-thread::list-all-threads))
		(if (string-equal name (sb-thread::thread-name x))
			(return x))))

(defun cleanup (name)
	(sb-thread:terminate-thread (thread-from-name name)))


;;; In case we need it
(defvar *the-frame* wb::*system-default-menubar*)
;;; Menu stuff
  ;;; Here is a blank spacer
 (define-qmbar-command com-divider ()
  NIL)

;;; START Q/About Quail
(define-qmbar-command com-AboutQ-QAQ ()
	(inform-user
   #|
       (format NIL
               "~&~a ~%~
                ~%~
                Copyright ~%~(format NIL
               "~&~a ~%~
                ~%~
                Copyright ~%~
                Statistical Computing Laboratory ~%~
                University of Waterloo ~%~
                ~%~
                From a cast of ones, including:~%~
                R.W. Oldford, C.B. Hurley, D.G. Anglin, M.E. Lewis,~%~
                G. Desvignes, H.A.Chipman, P. Poirier, N.G. Bennett, ~
                G.W. Bennett, C. Whimster, and, of course, Bob White."
               ;(qk::quail-release)
               )
                Statistical Computing Laboratory ~%~
                University of Waterloo ~%~
                ~%~
                From a cast of ones, including:~%~
                R.W. Oldford, C.B. Hurley, D.G. Anglin, M.E. Lewis,~%~
                G. Desvignes, H.A.Chipman, P. Poirier, N.G. Bennett, ~
                G.W. Bennett, C. Whimster, and, of course, Bob White."
               ;(qk::quail-release)
               )
       |#
       (format NIL
               "Copyright ~%~
                Statistical Computing Laboratory ~%~
                University of Waterloo ~%~
                ~%~
                From a cast of ones, including:~%~
                R.W. Oldford, C.B. Hurley, D.G. Anglin, M.E. Lewis,~%~
                G. Desvignes, H.A.Chipman, P. Poirier, N.G. Bennett, ~
                G.W. Bennett, C. Whimster, and, of course, Bob White."
               ;(qk::quail-release)
               )
       )
      "Some credit information on Quail.")

;(make-command-table 'qaq-command-table :errorp nil
;	:menu '(("About Quail" :command com-AboutQ-QAQ)
;		("" :command com-divider)))

;;; End Q/about Quail

;;; START Q/Information

;(define-qmbar-command com-Info-QI ()
;	(help))
 ;;; START command table Q/I/Help/Symbol indices sb
 (define-qmbar-command com-qextsym-QIHSISB ()
 	(view-doc-index :package :quail))

 (define-qmbar-command com-qkextsym-QIHSISB ()
 	(view-doc-index :package :quail-kernel))

 (define-qmbar-command com-vextsym-QIHSISB ()
 	(view-doc-index :package :views))

 (define-qmbar-command com-wbextsym-QIHSISB ()
 	(view-doc-index :package :window-basics))

 (make-command-table 'qiextsym-command-table :errorp nil
        :menu '(("Quail external symbols" :command com-qextsym-QIHSISB)
        	("Quail-kernel external symbols" :command com-qkextsym-QIHSISB)
        	("Views external symbols" :command com-vextsym-QIHSISB)
        	("Window Basics external symbols" :command com-wbextsym-QIHSISB)))
 ;;; END command table Q/I/Help/Help/Symbol indices sb


 ;;; START command table Q/I/H/Symbol indices
;(define-qmbar-command com-si-QIHHSI ()
;	(view-doc-index :package :quail))

;(make-command-table 'qihsi-command-table :errorp nil
;	:menu '(;("Symbol indices" :command com-si-QIHSI)
;		("Symbol indices" :menu qiextsym-command-table)))
 ;;; END command table Q/I/H/Symbol indices


 ;;; START command table Q/I/H/H
;(define-qmbar-command com-qih-QIH ()
;	(help))
;
(define-qmbar-command com-qihtopic-QIH ()
	(help 'Quail :topic))

;(make-command-table 'qihh-command-table :errorp nil
;	:menu '(("Help" :command com-qihh-QIH)
;		("Help topic" :command com-qihtopic-QIH)
;		("Symbol indices" :menu qihhsi-command-table)))

 ;;; END command table Q/I/H/H

 ;;; START command table Q/I/H
 ;;; the initial command
 (define-qmbar-command com-qih-QIH ()
 	(help))
 ;;; the command table
 (make-command-table 'qih-command-table :errorp nil
 	:menu '(("Help" :command com-qih-QIH)
 		("Help topic" :command  com-qihtopic-QIH)
 		("Symbol indices" :menu qiextsym-command-table)))

 ;;; END command table Q/I/H

 ;;; START command table Q/I/Packages
    ;;; the initial command
(define-qmbar-command com-pkgs-QIP ()
	(help (find-symbol "PACKAGES"
                                    (find-package "Q-USER"))
                       :topic))    
    ;;; START the  Packages sub-menu
(define-qmbar-command com-AbtPkgs-QIPSB ()
	(help (find-symbol "PACKAGES"
                                    (find-package "Q-USER"))
                       :topic))

(define-qmbar-command com-qipquser-QIPSB ()
	(and (this-may-take-a-while)
                          ;; avoid interning 'quail-user in the :quail package
                          (help :q-user :package)))

(define-qmbar-command com-qipqkernel-QIPSB ()
	(and (this-may-take-a-while)
                            (help :quail-kernel :package)))

(define-qmbar-command com-qipwbasics-QIPSB ()
	(and (this-may-take-a-while)
                             (help :window-basics :package)))

(define-qmbar-command com-qipviews-QIPSB ()
	(and (this-may-take-a-while)
                     (help :views :package)))
;;; the command table
(make-command-table 'qipsb-command-table :errorp nil
	:menu '(("About packages" :command com-AbtPkgs-QIPSB)
		("Quail-User" :command com-qipquser-QIPSB)
	    ("Quail-Kernel" :command com-qipqkernel-QIPSB)
	    ("Window-Basics" :command com-qipwbasics-QIPSB)
	    ("Views" :command com-qipviews-QIPSB)
	))
    ;;; END the About Packages sub-menu
;;; the qip command-table
;(make-command-table 'qip-command-table :errorp nil
;	:menu '(;("Packages" :command com-qipquser-QIPSB)
;		("Packages" :menu qipsb-command-table)))    
 ;;; END command table Q/I/Packages

 ;;; START Q/I/Browser
 ;;; make the initial command
 ;(define-qmbar-command com-qibb-QIB ()
 ;	(make-browser))

    ;;; START Q/I/B submenu
;;; the commands
(define-qmbar-command com-qibcb-QIBSB ()
	(make-browser))

(define-qmbar-command com-qiballq-QISB ()
	(apply #'class-browse
               (append
                (qk::list-symbols :window-basics
                                  :test
                                  #'(lambda (s) (and (eq (symbol-package 
s)
                                                         (find-package 
:window-basics))
                                                     (find-class s 
nil))))
                (qk::list-symbols
                 :quail
                 :test
                 #'(lambda (s)
                     (and (or
                           (eq (symbol-package s)
                               (find-package :quail))
                           (eq (symbol-package s)
                               (find-package :quail-kernel))
                           (eq (symbol-package s)
                               (find-package :views)))
                          (find-class s nil)))))))

(define-qmbar-command com-qibqpkg-QIBSB ()
	(apply #'class-browse
               (qk::list-symbols
                :quail
                :test
                #'(lambda (s)
                    (and (or
                          (eq (symbol-package s)
                              (find-package :quail))
                          )
                         (find-class s nil))))))

(define-qmbar-command com-qibqkpkg-QIBSB ()
	(apply #'class-browse
               (qk::list-symbols
                :quail-kernel
                :test
                #'(lambda (s)
                    (and (or
                          (eq (symbol-package s)
                              (find-package :quail-kernel))
                          )
                         (find-class s nil))))))

(define-qmbar-command com-qibvpkg-QIBSB ()
	(apply #'class-browse
               (qk::list-symbols
                :views
                :test
                #'(lambda (s)
                    (and (eq (symbol-package s)
                             (find-package :views))
                         (find-class s nil))))))

(define-qmbar-command com-qibwbpkg-QIBSB ()
	(apply #'class-browse
               (qk::list-symbols :window-basics
                                 :test
                                 #'(lambda (s) (and (eq (symbol-package 
s)
                                                        (find-package 
:window-basics))
                                                    (find-class s 
nil))))))

;;; make the sub-menu command-table
(make-command-table 'qibsb-command-table :errorp nil
	:menu '(("Class browser" :command com-qibcb-QIBSB)
		("Class browse all of quail" :command com-qiballq-QISB)
		("Class browse the quail package" :command com-qibqpkg-QIBSB)
		("Class browse the quail-kernel package" :command com-qibqkpkg-QIBSB)
		("Class browse the views package" :command com-qibvpkg-QIBSB)
		("Class browse the window-basics package" :command com-qibwbpkg-QIBSB)))
    ;;; END Q/I/B submenu
 ;;; END Q/I/Browser

 ;;; START command table Q/I
(make-command-table 'qi-command-table :errorp nil
	:menu '(("Help" :menu qih-command-table)
		("Packages" :menu qipsb-command-table)
		("Browser" :menu qibsb-command-table)
		))
;;; END command table Q/I
;;; END Q/Information

;;; START Q/Examples
   ;;; START Q/EX/AR/O/I/submenu Examples/Arrays/Overview/Iteration
       ;;; which has a command and a submenu
;;; commands for the sub-menu
(define-qmbar-command com-elts-QEXAROISB ()
	(edit-file "eg:Arrays;iter-elements.lsp"))

(define-qmbar-command com-genrl-QEXAROISB ()
	(edit-file "eg:Arrays;iter-general.lsp"))

(define-qmbar-command com-mapfns-QEXAROISB ()
	(edit-file "eg:Arrays;iter-map.lsp"))

(define-qmbar-command com-modslices-QEXAROISB ()
	(edit-file "eg:Arrays;iter-modify.lsp"))

(define-qmbar-command com-slices-QEXAROISB ()
	(edit-file "eg:Arrays;iter-slices.lsp"))

;;; make the submenu
(make-command-table 'qexaroisb-command-table :errorp nil
	:menu '(("Elements" :command com-elts-QEXAROISB)
		("General" :command com-genrl-QEXAROISB)
		("Mapping functions" :command com-mapfns-QEXAROISB)
		("Modifying slices" :command com-modslices-QEXAROISB)
		("Slices" :command com-slices-QEXAROISB)))
   ;;; END Q/EX/AR/O/I/submenu
   ;;; START Q/EX/AR/O/I command table
;;; the initial command
(define-qmbar-command com-iter-QEXAROI ()
	(edit-file "eg:Arrays;iter-elements.lsp"))



   ;;; START Q/EX/AR/O command table
;;; a bunch of commands
(define-qmbar-command com-oview-QEXARO ()
	(edit-file "eg:Arrays;overview.lsp"))

(define-qmbar-command com-intro-QEXARO ()
	(edit-file "eg:Arrays;intro.lsp"))

(define-qmbar-command com-advarrcreate-QEXARO ()
	(edit-file "eg:Arrays;array.lsp"))

(define-qmbar-command com-arithops-QEXARO ()
	(edit-file "eg:Arrays;arith-ops.lsp"))

(define-qmbar-command com-glueops-QEXARO ()
	(edit-file "eg:Arrays;glue.lsp"))

(define-qmbar-command com-mathops-QEXARO ()
	(edit-file "eg:Arrays;math-funs.lsp"))

(define-qmbar-command com-matrices-QEXARO ()
	(edit-file "eg:Arrays;Matrices;overview.lsp"))

(define-qmbar-command com-numpreds-QEXARO ()
	(edit-file "eg:Arrays;num-preds.lsp"))

(define-qmbar-command com-refcopy-QEXARO ()
	(edit-file "eg:Arrays;ref.lsp"))

(define-qmbar-command com-search-QEXARO ()
	(edit-file "eg:Arrays;search.lsp"))

(define-qmbar-command com-seltest-QEXARO ()
	(edit-file "eg:Arrays;select-by-pred.lsp"))

(define-qmbar-command com-handyarr-QEXARO ()
	(edit-file "eg:Arrays;handy-arrays.lsp"))

(define-qmbar-command com-sortrnkperm-QEXARO ()
	(edit-file "eg:Arrays;sort.lsp"))

;;; make the command table
(make-command-table 'qexaro-command-table :errorp nil
	:menu '(("Overview" :command com-oview-QEXARO)
		("Introduction" :command com-intro-QEXARO)
		("" :command com-divider)
		("Advanced array creation" :command com-advarrcreate-QEXARO)
		("Arithmetic operations" :command com-arithops-QEXARO)
		("Glueing operations" :command com-glueops-QEXARO)
		("Iteration" :menu qexaroisb-command-table)
		("Mathematical operations" :command com-mathops-QEXARO)
		("Matrices" :command com-mathops-QEXARO)
		("Numerical predicates" :command com-numpreds-QEXARO)
		("Referencing and copying" :command com-refcopy-QEXARO)
		("Searching arrays" :command com-search-QEXARO)
		("Selecting elements by test" :command com-seltest-QEXARO)
		("Some handy arrays" :command com-handyarr-QEXARO)
		("Sorting, ranking, permuting" :command com-sortrnkperm-QEXARO)))													
   ;;; END Q/EX/AR/O command table



            ;;; START Q/EX/M/AR/M/O/D Sub-table Overview command table
;;; the initial command
(define-qmbar-command com-oview-QEXMARMODSB ()
	(edit-file
             "eg:Arrays;Matrices;Decompositions;overview.lsp"))
;;; menu commands

(define-qmbar-command com-chol-QEXMARMODSB ()
	(edit-file
             "eg:Arrays;Matrices;Decompositions;cholesky.lsp"))

(define-qmbar-command com-lu-QEXMARMODSB ()
	(edit-file
             "eg:Arrays;Matrices;Decompositions;lu.lsp"))

(define-qmbar-command com-qr-QEXMARMODSB ()
	(edit-file
             "eg:Arrays;Matrices;Decompositions;qr.lsp"))

(define-qmbar-command com-svd-QEXMARMODSB ()
	(edit-file
             "eg:Arrays;Matrices;Decompositions;svd.lsp"))                                                    
;;; the command table
(make-command-table 'qexmarmodsb-command-table :errorp nil
	:menu '(("Overview" :command com-oview-QEXMARMODSB)
		("" :command com-divider)
		("Cholesky decomposition" :command com-chol-QEXMARMODSB)
		("LU decomposition" :command com-lu-QEXMARMODSB)
		("QR decomposition" :command com-qr-QEXMARMODSB)
		("Singular value decomposition" :command com-svd-QEXMARMODSB)))
            ;;; END Q/EX/M/AR/M/O/D Sub-menu command table

         ;;; START Q/EX/M/AR/M/O/D Decompositions command table
;;; initial command
(define-qmbar-command com-decomp-QEXMARMOD ()
	(edit-file
           "eg:Arrays;Matrices;Decompositions;overview.lsp"))

;;; the command table
(make-command-table 'qexmarmod-command-table :errorp nil
	:menu '(("Various Decompositions" :menu qexmarmodsb-command-table)))
         ;;; END Q/EX/M/AR/M/O/D command table

       ;;; START Q/EX/M/AR/M Sub-menu Overview command table
;;; initial commands
(define-qmbar-command com-oview-QEXMARMSB ()
	(edit-file "eg:Arrays;Matrices;overview.lsp"))

(define-qmbar-command com-intro-QEXMARMSB ()
	(edit-file "eg:Arrays;Matrices;intro.lsp"))

;;; the command table
(make-command-table 'qexmarmsb-command-table :errorp nil
	:menu '(("Overview" :command com-oview-QEXMARMSB)
		("Introduction" :command com-intro-QEXMARMSB)
		("" :command com-divider)
		("Decompositions" :menu qexmarmod-command-table)))

     ;;; START Q/EX/M/AR/M Matrices command table
;;; the initial command
(define-qmbar-command com-matrices-QEXMARM ()
	(edit-file "eg:Arrays;Matrices;overview.lsp"))

;;; the final command
(define-qmbar-command com-matops-QEXMARM ()
	(edit-file "eg:Arrays;Matrices;operations.lsp"))

;;; the command table
(make-command-table 'qexmarm-command-table :errorp nil
	:menu '(("Matrices" :menu qexmarmsb-command-table)
		("Matrix operations" :command com-matops-QEXMARM)))	

     ;;; END Q/EX/M/AR/M command table

     ;;; START Q/EX/M/AR/SF Special functions command table
;;; the initial command
(define-qmbar-command com-spfns-QEXMARSF ()
	(edit-file "eg:Mathematics;Special-Functions;overview.lsp"))

;;; the command-table items
(define-qmbar-command com-oview-QEXMARSFSB ()
	(edit-file "eg:Mathematics;Special-Functions;overview.lsp"))

(define-qmbar-command com-beta-QEXMARSFSB ()
	(edit-file "eg:Mathematics;Special-Functions;beta.lsp"))

(define-qmbar-command com-contfrac-QEXMARSFSB ()
	(edit-file 
"eg:Mathematics;Special-Functions;continued-fraction.lsp"))

(define-qmbar-command com-errfn-QEXMARSFSB ()
	(edit-file "eg:Mathematics;Special-Functions;error-fun.lsp"))

(define-qmbar-command com-gam-QEXMARSFSB ()
	(edit-file "eg:Mathematics;Special-Functions;gamma.lsp"))

;;; the sub-menu command table
(make-command-table 'qexmarsfsb-command-table :errorp nil
	:menu '(("Overview" :command com-oview-QEXMARSFSB)
		("" :command com-divider)
		("Beta functions" :command com-beta-QEXMARSFSB)
		("Continued fractions" :command com-contfrac-QEXMARSFSB)
		("Error functions" :command com-errfn-QEXMARSFSB)
		("Gamma functions" :command com-gam-QEXMARSFSB)))

     ;;; END Q/EX/M/AR/SF command table

     ;;; START Q/EX/M/AR/CM Combinatorics command table
;;; the initial command
(define-qmbar-command com-comb-QEXMARCM ()
	(edit-file "eg:Mathematics;Combinatorics;counting.lsp"))

;;; the sub-menu command table
(define-qmbar-command com-count-QEXMARCMSB ()
	(edit-file "eg:Mathematics;Combinatorics;counting.lsp"))

(define-qmbar-command com-factor-QEXMARCMSB ()
	(edit-file "eg:Mathematics;Combinatorics;counting.lsp"))

(make-command-table 'qexmarcmsb-command-table :errorp nil
	:menu '(("Counting" :command com-count-QEXMARCMSB)
		("Factors" :command com-factor-QEXMARCMSB)))

     ;;; START Q/EX/M/AR/CL Calculus command table
;;; make the sub-menu commands
(define-qmbar-command com-deriv-QEXMARCLSB ()
	(edit-file "eg:Mathematics;Calculus;deriv.lsp"))

(make-command-table 'qexmarclsb-command-table :errorp nil
	:menu '(("Calculus" :command com-deriv-QEXMARCLSB)))

   ;;; START Q/EX/M/AR command table
;;; the initial command
(define-qmbar-command com-arithops-QEXMAR ()
	(edit-file "eg:Arrays;arith-ops.lsp"))
;;; other commands
(define-qmbar-command com-extended-QEXMAR ()
	(edit-file "eg:Mathematics;extended-arithmetic.lsp"))

(define-qmbar-command com-mathops-QEXMAR ()
	(edit-file "eg:Arrays;math-funs.lsp"))

;;; the command table
(make-command-table 'qexmar-command-table :errorp nil
	:menu '(("Arithmetic operations" :command com-arithops-QEXMAR)
		("Calculus" :menu qexmarclsb-command-table)
		("Combinatorics" :menu qexmarcmsb-command-table)
		("Extended arithmetic" :command com-extended-QEXMAR)
		("Mathematical operations" :command com-mathops-QEXMAR)
		("Matrices" :menu qexmarm-command-table)
		("Special functions" :menu qexmarsfsb-command-table)))
	;;; END Q/EX/M/AR command table
	
	;;; START Q/EX/M command table	

   ;;; END Q/EX/Math command table

;;; START work on Probability

   ;;; START Q/EX/P/O/BI SB Probabiliy/Overview/BuiltIn/sub-menu
(define-qmbar-command com-oview-QEXPOBISB ()
	(edit-file "eg:Probability;Distributions;stock.lsp"))

(define-qmbar-command com-ctsdists-QEXPOBISB ()
	(edit-file "eg:Probability;Distributions;stock-cts.lsp"))

(define-qmbar-command com-beta-QEXPOBISB ()
	(edit-file "eg:Probability;Distributions;beta.lsp"))

(define-qmbar-command com-cauchy-QEXPOBISB ()
	(edit-file "eg:Probability;Distributions;cauchy.lsp"))

(define-qmbar-command com-chisq-QEXPOBISB ()
	(edit-file "eg:Probability;Distributions;chi-squared.lsp"))

(define-qmbar-command com-exp-QEXPOBISB ()
	(edit-file "eg:Probability;Distributions;exponential.lsp"))

(define-qmbar-command com-f-QEXPOBISB ()
	(edit-file "eg:Probability;Distributions;F-dist.lsp"))

(define-qmbar-command com-gamma-QEXPOBISB ()
	(edit-file "eg:Probability;Distributions;gamma.lsp"))

(define-qmbar-command com-gauss-QEXPOBISB ()
	(edit-file "eg:Probability;Distributions;gaussian.lsp"))

(define-qmbar-command com-k-QEXPOBISB ()
	(edit-file "eg:Probability;Distributions;K-dist.lsp"))

(define-qmbar-command com-pareto-QEXPOBISB ()
	(edit-file "eg:Probability;Distributions;pareto.lsp"))

(define-qmbar-command com-student-QEXPOBISB ()
	(edit-file "eg:Probability;Distributions;student.lsp"))

(define-qmbar-command com-uniform-QEXPOBISB ()
	(edit-file "eg:Probability;Distributions;uniform.lsp"))

(define-qmbar-command com-weibull-QEXPOBISB ()
	(edit-file "eg:Probability;Distributions;weibull.lsp"))

(define-qmbar-command com-discr-QEXPOBISB ()
	(edit-file "eg:Probability;Distributions;stock-disc.lsp"))

(define-qmbar-command com-bern-QEXPOBISB ()
	(edit-file "eg:Probability;Distributions;bernoulli.lsp"))

(define-qmbar-command com-bin-QEXPOBISB ()
	(edit-file "eg:Probability;Distributions;binomial.lsp"))

(define-qmbar-command com-geom-QEXPOBISB ()
	(edit-file "eg:Probability;Distributions;geometric.lsp"))

(define-qmbar-command com-hyperg-QEXPOBISB ()
	(edit-file "eg:Probability;Distributions;hypergeometric.lsp"))

(define-qmbar-command com-negbin-QEXPOBISB ()
	(edit-file 
"eg:Probability;Distributions;negative-binomial.lsp"))

(define-qmbar-command com-poisson-QEXPOBISB ()
	(edit-file "eg:Probability;Distributions;poisson.lsp"))

(define-qmbar-command com-unifdiscr-QEXPOBISB ()
	(edit-file 
"eg:Probability;Distributions;discrete-uniform.lsp"))

;;; make the command table
(make-command-table 'qexpobisb-command-table :errorp nil
	:menu '(("" :command com-divider)
		("Continuous distributions" :command com-ctsdists-QEXPOBISB)
		("Beta" :command com-beta-QEXPOBISB)
		("Cauchy" :command com-cauchy-QEXPOBISB)
		("Chi-squared" :command com-chisq-QEXPOBISB)
		("Exponential" :command com-exp-QEXPOBISB)
		("F" :command com-f-QEXPOBISB)
		("Gamma" :command com-gamma-QEXPOBISB)
		("Gaussian (normal)" :command com-gauss-QEXPOBISB)
		("K" :command com-k-QEXPOBISB)
		("Pareto" :command com-pareto-QEXPOBISB)
		("Student's t" :command com-student-QEXPOBISB)
		("Uniform" :command com-uniform-QEXPOBISB)
		("Weibull" :command com-weibull-QEXPOBISB)
		("" :command com-divider)
		("Discrete distributions" :command com-discr-QEXPOBISB)
		("Bernoulli" :command com-bern-QEXPOBISB)
		("Binomial" :command com-bin-QEXPOBISB)
		("Geometric" :command com-geom-QEXPOBISB)
		("Hypergeometric" :command com-hyperg-QEXPOBISB)
		("Negative binomial" :command com-negbin-QEXPOBISB)
		("Poisson" :command com-poisson-QEXPOBISB)
		("Uniform (discrete)" :command com-unifdiscr-QEXPOBISB)))
   ;;; END Q/EX/P/O/BI SB sub-menu

   ;;; START Q/EX/P/O/BI command table
;;; the initial command
(define-qmbar-command com-builtin-QEXPOBI ()
	(edit-file "eg:Probability;Distributions;stock.lsp"))

   ;;; END Q/EX/P/O/BI command table

   ;;; START Q/EX/P/O command table
;;; initial commands
(define-qmbar-command com-oview-QEXPO ()
	(edit-file "eg:Probability;Distributions;overview.lsp"))

(define-qmbar-command com-intro-QEXPO ()
	(edit-file "eg:Probability;Distributions;intro.lsp"))

;;; the command table
(make-command-table 'qexpo-command-table :errorp nil
	:menu '(("Overview" :command com-oview-QEXPO)
		("" :command com-dists-QEXPD)
		("Introduction" :command com-intro-QEXPO)
		("Built in distributions" :menu qexpobisb-command-table)))
   ;;; END Q/EX/P/O command table

   ;;; I MISSED ONE LEVEL
   ;;; START Q/EX/P/D Distributions command table
;;; the initial command
(define-qmbar-command com-dists-QEXPD ()
	(edit-file "eg:Probability;Distributions;overview.lsp"))

;;; the command table
(make-command-table 'qexpd-command-table :errorp nil
	:menu '(("Distributions" :menu qexpo-command-table)))	
   ;;; END Q/EX/P/D command table

   ;;; START Q/EX/Probability command table
;;; no initial command

   ;;; END Q/EX/Probability command table

   ;;; START Q/EX/S/SU/RM SB command table Statistics/Summary Statistics/Response Models sub-menu
;;; no initial command
;;; the table commands
(define-qmbar-command com-oview-QEXSSURMSB ()
	(edit-file "eg:Statistics;Models;overview.lsp"))

(define-qmbar-command com-binlogit-QEXSSURMSB ()
	(edit-file 
"eg:Statistics;Models;eg-glm-kyphosis.lsp"))

(define-qmbar-command com-poisslog-QEXSSURMSB ()
	(edit-file 
"eg:Statistics;Models;eg-glm-ship-data.lsp"))

(define-qmbar-command com-gamrecip-QEXSSURMSB ()
	(edit-file 
"eg:Statistics;Models;eg-glm-car.lsp"))

;;; the command table
(make-command-table 'qexssurmsb-command-table :errorp nil
	:menu '(("Overview" :command com-oview-QEXSSURMSB)
		("" :command com-dists-QEXPD)
		("binary/logit" :command com-binlogit-QEXSSURMSB)
		("poisson/log" :command com-poisslog-QEXSSURMSB)
		("gamma/reciprocal" :command com-gamrecip-QEXSSURMSB)
		))     		
   ;;; END Q/EX/S/SU/RMSB command table

   ;;; START Q/EX/S/SU/RM command table
;;; the initial command
(define-qmbar-command com-respmod-QEXSSURM ()
	(edit-file "eg:Statistics;Models;overview.lsp"))

;;; the command table	
;
   ;;; END Q/EX/S/SU/RM command table

   ;;; START Q/EX/S/SU/A SB command table Statistics/Summary Statistics/Analysis sub-menu
;;; the commands
(define-qmbar-command com-lightspeed-QEXSSUASB ()
	(edit-file "eg:Statistics;Analyses;meta-analysis.lsp"))
;;; the command table	
(make-command-table 'qexssuasb-command-table :errorp nil
	:menu '(("Speed of light meta analysis" :command com-lightspeed-QEXSSUASB)))
   ;;; END Q/EX/S/SU/A SB command table

   ;;; START Q/EX/S/SU/A command table
;;; no initial command

	;;; START Q/EX/S/SU Summary statistics command table
;;; the initial commands	   
(define-qmbar-command com-sumstat-QEXSSU ()
	(edit-file "eg:Statistics;summary-statistics.lsp"))

;;; the command table
(make-command-table 'qexssu-command-table :errorp nil
	:menu '(("Summary statistics" :command com-sumstat-QEXSSU)
		("Response Models" :menu qexssurmsb-command-table)
		("Analyses" :menu qexssuasb-command-table)))
	;;; END Q/EX/S/SU command table
	
	;;; START Q/EX/S Statistics command table
;;; no ititial command

	;;; END Q/EX/S command table

	;;; START Q/EX/V/O/PSB Views//Overview /Plots Sub-menu command table
;;; the commands
(define-qmbar-command com-plots-QEXVOPSB ()
	(edit-file "eg:Views;Plots;general.lsp"))

(define-qmbar-command com-scatter-QEXVOPSB ()
	(edit-file "eg:Views;Plots;scatterplot.lsp"))

(define-qmbar-command com-surface-QEXVOPSB ()
	(edit-file "eg:Views;Plots;surface.lsp"))

(define-qmbar-command com-grid-QEXVOPSB ()
	(edit-file "eg:Views;Plots;grid-plot.lsp"))

;;; make the command table
(make-command-table 'qexvopsb-command-table :errorp nil
	:menu '(("Plots" :command com-plots-QEXVOPSB)
		("" :command com-divider)
		("Scatterplots" :command com-scatter-QEXVOPSB)
		("Surface plots" :command com-surface-QEXVOPSB)
		("Grid plots" :command com-grid-QEXVOPSB)))				
	;;; END Q/EX/V/O/PSB command table

	;;; START Q/EX/V/O/P Plots command table
;;; the initial command
(define-qmbar-command com-plots-QEXVOP ()
	(edit-file "eg:Views;Plots;general.lsp"))

	;;; END Q/EX/V/O/P command table

	;;; START Q/EX/V/O/SVSB Views/Overview/Simple-Views Sub-menu command table
;;; the commands
(define-qmbar-command com-bars-QEXVOSVSB ()
	(edit-file "eg:Views;Simple-Views;bar.lsp"))

(define-qmbar-command com-pies-QEXVOSVSB ()
	(edit-file "eg:Views;Simple-Views;pie.lsp"))

(define-qmbar-command com-labels-QEXVOSVSB ()
	(edit-file "eg:Views;Simple-Views;label.lsp"))

;;; make the command table
(make-command-table 'qexvosvsb-command-table :errorp nil
	:menu '(("Simple-Views" :command com-bars-QEXVOSVSB)
		("Pies" :command com-pies-QEXVOSVSB)
		("Labels" :command com-labels-QEXVOSVSB)))
	;;; END Q/EX/V/O/SVSB command table

	;;; START Q/EX/V/O/SV Simple-Views command table
;;; no initial command

	;;; END Q/EX/V/O/SV command table

	;;; START Q/EX/V/O/B Views/Overview/Basics Sub-menu command table
;;; the commands
(define-qmbar-command com-intro-QEXVOBSB ()
	(edit-file "eg:Views;Basics;introduction.lsp"))

(define-qmbar-command com-classes-QEXVOBSB ()
	(edit-file "eg:Views;Basics;classes.lsp"))

(define-qmbar-command com-drawstyles-QEXVOBSB ()
	(edit-file "eg:Views;Basics;drawing-styles.lsp"))

(define-qmbar-command com-mouse-QEXVOBSB ()
	(edit-file "eg:Views;Basics;mouse.lsp"))

(define-qmbar-command com-movecopy-QEXVOBSB ()
	(edit-file "eg:Views;Basics;move-copy.lsp"))

(define-qmbar-command com-vpvw-QEXVOBSB ()
	(edit-file "eg:Views;Basics;vp-vw.lsp"))

(define-qmbar-command com-select-QEXVOBSB ()
	(edit-file "eg:Views;Basics;selection.lsp"))

(define-qmbar-command com-data-QEXVOBSB ()
	(edit-file "eg:Views;Basics;data.lsp"))

;;; make the command table
(make-command-table 'qexvobsb-command-table :errorp nil
	:menu '(("Introduction" :command com-intro-QEXVOBSB)
		("" :command com-dists-QEXPD)
		("Classes" :command com-classes-QEXVOBSB)
		("Drawing styles" :command com-drawstyles-QEXVOBSB)
		("Mouse interaction" :command com-mouse-QEXVOBSB)
		("Moving and copying" :command com-movecopy-QEXVOBSB)
		("Viewports and Windows" :command com-vpvw-QEXVOBSB)
		("Selection of views" :command com-select-QEXVOBSB)
		("Dealing with data" :command com-data-QEXVOBSB)))
	;;; END Q/EX/V/O/BSB command table

	;;; START Q/EX/V/O/B Views/Overview/Basics command table
;;; the initial command
(define-qmbar-command com-basics-QEXVOB ()
	(edit-file "eg:Views;Basics;introduction.lsp"))
;;; make the command table

	;;; START Q/EX/V/O/GL Sub-menu Views/Overview/Graphical Layout command table
;;; the initial command
(define-qmbar-command com-grid-QEXVOGLSB ()
	(edit-file "eg:Views;Plots;grid-plot.lsp"))
;;; make the command table
(make-command-table 'qexvoglsb-command-table :errorp nil
	:menu '(("Grid plot" :command com-graphlay-QEXVOGLSB)))	
	;;; END Q/EX/V/O/GLSB command table
	;;; START Q/EX/V/O/GL Grpaical layout command table
;;; the initial command
(define-qmbar-command com-graphlay-QEXVOGLSB ()
	(inform-user "Sorry no overview written yet."))
;;; make the command table

	;;; END Q/EX/V/O/GL command table

	;;; START Q/EX/V/O/AD SB Views/Overview/ADvanced Sub-menu
;;; the commands
(define-qmbar-command com-interact-QEXVOADSB ()
	(edit-file "eg:Views;Advanced;interaction-plots.lsp"))

(define-qmbar-command com-trellis-QEXVOADSB ()
	(edit-file "eg:Views;Advanced;trellis.lsp"))

(define-qmbar-command com-suicide-QEXVOADSB ()
	(edit-file "eg:Views;Advanced;trellis.lsp"))

(define-qmbar-command com-categ-QEXVOADSB ()
	(edit-file "eg:Views;Advanced;categorical.lsp"))

(define-qmbar-command com-mosaic-QEXVOADSB ()
	(edit-file "eg:Views;Advanced;mosaic.lsp"))

;;; make the command table
(make-command-table 'qexvoadsb-command-table :errorp nil
	:menu '(("Interaction plots" :command com-interact-QEXVOADSB)
		("A trellis example" :command com-trellis-QEXVOADSB)
		("Suicide data" :command com-suicide-QEXVOADSB)
		("Categorical data" :command com-categ-QEXVOADSB)
		("Mosaic displays" :command com-mosaic-QEXVOADSB)))					
	;;; END Q/EX/V/O/ADSB command table

	;;;START Q/EX/V/O/AD ADvanced command table
;;; the initial command
(define-qmbar-command com-advanced-QEXVOAD ()
	(inform-user "Various advanced displays."))

	;;; END Q/EX/V/O/AD command table

	;;; START Q/EX/V/O/AP SB Views/Overview/APplications Sub-menu command table
;;; the commands
(define-qmbar-command com-boxcox-QEXVOAPSB ()
	(edit-file "eg:Views;Applications;boxcox.lsp"))

(define-qmbar-command com-eusymb-QEXVOAPSB ()
	(edit-file "eg:Views;Applications;euro.lsp"))

(define-qmbar-command com-smoke-QEXVOAPSB ()
	(edit-file "eg:Views;Applications;smoke.lsp"))
;;; make the command table
(make-command-table 'qexvoapsb-command-table :errorp nil
	:menu '(("Box-Cox plots" :command com-boxcox-QEXVOAPSB)
		("EU symbol" :command com-eusymb-QEXVOAPSB)
		("Smoker analysis" :command com-smoke-QEXVOAPSB)))			
	;;; END Q/EX/V/O/APSB command table

	;;; START Q/EX/V/O/AP Views/Overview/APplications command table
;;; the initial command
(define-qmbar-command com-apps-QEXVOAP ()
	(inform-user "Sorry no overview written yet."))
;;; the command table

	;;; END Q/EX/V/P/AP command table

	;;; START Q/EX/V/O Views/Overview command table
;;; the initial command 
(define-qmbar-command com-oview-QEXVO ()
	(edit-file "eg:Views;overview.lsp"))
;;; and another one
(define-qmbar-command com-text-QEXVO ()
	(edit-file "eg:Views;Simple-Views;label.lsp"))
;;; make the command table
(make-command-table 'qexvo-command-table :errorp nil
	:menu '(("Overview" :command com-oview-QEXVO)
		("" :command com-divider)
		("Plots" :menu qexvopsb-command-table)
		("Simple views" :menu qexvosvsb-command-table)
		("Basics" :menu qexvobsb-command-table)
		("Graphical Layout" :menu qexvoglsb-command-table)
		("Text" :command com-text-QEXVO)
		("Advanced" :menu qexvoadsb-command-table)
		("Applications" :menu qexvoapsb-command-table)
		))	
	;;; END Q/EX/V/O command table

   ;;; START Q/EX/Views command table
;
   ;;; END Q/EX/Views command table


   ;;; START Q/EX command table
;;; define the commands
(define-qmbar-command com-files-QEX ()
	(inform-user 
       (format NIL
               "Examples can be found in the directory ~a ~&~
                or more simply \"eg:\". ~&~
                There are more files there than are presented here."
               (truename (pathname "eg:")))))


;;; make the command table
;;; a command
(define-qmbar-command com-doc-QEX ()
	(edit-file "eg:Documentation;doc-example.lsp"))


(make-command-table 'qex-command-table :errorp nil
	:menu '(("Where are the files ?" :command com-files-QEX)
		("" :command com-divider)
		("Arrays" :menu qexaro-command-table)
		("Documentation" :command com-doc-QEX)
		("Mathematics" :menu qexmar-command-table)
		("Probability" :menu qexpd-command-table)
		("Statistics" :menu qexssu-command-table)
		("Views" :menu qexvo-command-table)))
   ;;; END Q/EX command table

   ;;; START Q/EX command table
;;; the initial command
(define-qmbar-command com-examples-QEX ()
	(inform-user 
       (format NIL
               "Examples can be found in the directory eg: ~&~
                There are more files there than are presented here.")))

;;; make the command table

   ;;; END Q/EX command table
;;; END Q/Examples

;;; START Q/Datasets
   ;;; START Q/D SB Files/Datasets Sub-menu
;;; the commands
(define-qmbar-command com-michel-QDSB ()
	(edit-file "q:Data;michelson-1879.lsp"))

(define-qmbar-command com-apple-QDSB ()
	(edit-file "q:Data;apple.lsp"))

(define-qmbar-command com-arms-QDSB ()
	(edit-file "q:Data;arms.lsp"))

(define-qmbar-command com-brain-QDSB ()
	(edit-file "q:Data;bbwgt.lsp"))

(define-qmbar-command com-cigs-QDSB ()
	(edit-file "q:Data;cigs.lsp"))

(define-qmbar-command com-coal-QDSB ()
	(edit-file "q:Data;coal.lsp"))

(define-qmbar-command com-light-QDSB ()
	(edit-file "q:Data;light-speeds.lsp"))

(define-qmbar-command com-nile-QDSB ()
	(edit-file "q:Data;nile-river.lsp"))

(define-qmbar-command com-react-QDSB ()
	(edit-file "q:Data;reaction-times.lsp"))

(define-qmbar-command com-smoker-QDSB ()
	(edit-file "q:Data;smoker.lsp"))

(define-qmbar-command com-solar-QDSB ()
	(edit-file "q:Data;solar.lsp"))

(define-qmbar-command com-squid-QDSB ()
	(edit-file "q:Data;squid.lsp"))

(define-qmbar-command com-usprod-QDSB ()
	(edit-file "q:Data;US-production.lsp"))

;;; make the command table
(make-command-table 'qdsb-command-table :errorp nil
	:menu '(("A.A. Michelson's 1879 speed of light" :command com-michel-QDSB)
		("Apple data" :command com-apple-QDSB)
		("Arms race" :command com-arms-QDSB)
		("Brain and Body average weights" :command com-brain-QDSB)
		("Cigarette Chemicals" :command com-cigs-QDSB)
		("Coal mine data" :command com-coal-QDSB)
		("Historical measures of speed of light" :command com-light-QDSB)
		("Nile river annual flow" :command com-nile-QDSB)
		("Reaction times" :command com-react-QDSB)
		("Smoker data" :command com-smoker-QDSB)
		("Solar data" :command com-solar-QDSB)
		("Squids eaten by sharks" :command com-squid-QDSB)
		("U.S. annual production" :command com-usprod-QDSB)))													
   ;;; END Q/EX/D sub-menu

   ;;; START Q/D Files/Datasets command table
;;; the initial command
(define-qmbar-command com-datasets-QD ()
	(inform-user "Datasets can be found in the directory q:Data;"))
;;; make the command table
;(make-command-table 'qd-command-table :errorp nil
;	:menu '(;("Datasets" :command com-datasets-QD)
;		("Datasets" :menu qdsb-command-table)))	
;;; END Q/Datasets

;;; START Q/Environment
  ;;; START Q/E/Help window

   ;;; START Q/E/HW/ ..Background color
;;; The initial command 
(define-qmbar-command com-HelpBkgCol-QEHWBG ()
	(set-help-background-color (wb:prompt-user-for-color)))
;;; the sub-menu
(define-qmbar-command com-Black-QEHWBCSB ()
 	(set-help-background-color wb:*black-color*))

 (define-qmbar-command com-Gray-QEHWBCSB ()
 	(set-help-background-color wb:*gray-color*))

 (define-qmbar-command com-White-QEHWBCSB ()
 	(set-help-background-color wb:*white-color*))

 (define-qmbar-command com-User-defined-QEHWBCSB ()
 	(set-help-background-color (wb:prompt-user-for-color)))

 (define-qmbar-command com-Blue-QEHWBCSB ()
 	(set-help-background-color wb:*blue-color*))

 (define-qmbar-command com-Bluel-QEHWBCSB ()
 	(set-help-background-color wb:*light-blue-color*))

 (define-qmbar-command com-Brown-QEHWBCSB ()
 	(set-help-background-color wb:*brown-color*))

 (define-qmbar-command com-Brownl-QEHWBCSB ()
 	(set-help-background-color wb:*tan-color*))

 (define-qmbar-command com-Grayd-QEHWBCSB ()
 	(set-help-background-color wb:*dark-gray-color*))

 (define-qmbar-command com-Grayl-QEHWBCSB ()
 	(set-help-background-color wb:*light-gray-color*))

 (define-qmbar-command com-Green-QEHWBCSB ()
 	(set-help-background-color wb:*green-color*))

 (define-qmbar-command com-Greend-QEHWBCSB ()
 	(set-help-background-color wb:*dark-green-color*))

 (define-qmbar-command com-Orange-QEHWBCSB ()
 	(set-help-background-color wb:*orange-color*))

 (define-qmbar-command com-Pink-QEHWBCSB ()
 	(set-help-background-color wb:*pink-color*))

 (define-qmbar-command com-Purple-QEHWBCSB ()
 	(set-help-background-color wb:*purple-color*))

 (define-qmbar-command com-Red-QEHWBCSB ()
 	(set-help-background-color wb:*red-color*))

 (define-qmbar-command com-Yellow-QEHWBCSB ()
 	(set-help-background-color wb:*yellow-color*))

 ;;; There are the colors
 ;;; now for the menus/command-tables
(make-command-table 'qehwbgsb-command-table
                     :errorp nil
                     :menu '(("Black" :command com-Black-QEHWBCSB)
                             ("Gray"      :command com-Gray-QEHWBCSB)
                             ("White" :command com-White-QEHWBCSB)
                             ("" :command :com-divider)
                             ("User defined" :command com-User-defined-QEHWBCSB)
                             ("" :command :com-divider)
                             ("Black" :command com-Black-QEHWBCSB)
                             ("Blue" :command com-Blue-QEHWBCSB)
                             ("Blue (light)" :command com-Bluel-QEHWBCSB)
                             ("Brown" :command com-Brown-QEHWBCSB)
                             ("Brown (light)" :command com-Brownl-QEHWBCSB)
                             ("Gray" :command com-Gray-QEHWBCSB)
                             ("Gray (dark)" :command com-Grayd-QEHWBCSB)
                             ("Gray (light)" :command com-Grayl-QEHWBCSB)
                             ("Green" :command com-Green-QEHWBCSB)
                             ("Green (dark)" :command com-Greend-QEHWBCSB)
                             ("Orange" :command com-Orange-QEHWBCSB)
                             ("Pink" :command com-Pink-QEHWBCSB)
                             ("Purple" :command com-Purple-QEHWBCSB)
                             ("Red" :command com-Red-QEHWBCSB)
                             ("White" :command com-White-QEHWBCSB)
                             ("Yellow" :command com-Yellow-QEIWPCSB)))
;;; and ..
(make-command-table 'qehwbg-command-table :errorp nil
	:menu '(;("Background color" :command com-HelpBkgCol-QEHWBG)
		("Background color" :menu qehwbgsb-command-table)))
   ;;; END Q/E/HW/ ..Background color

   ;;; START Q/E/HW/ ..Pen color
;;; The initial command 
(define-qmbar-command com-HelpPenCol-QEHWPC ()
	(set-help-pen-color (wb:prompt-user-for-color)))
;;; the sub-menu
(define-qmbar-command com-Black-QEHWPCSB ()
 	(set-help-pen-color wb:*black-color*))

 (define-qmbar-command com-Gray-QEHWPCSB ()
 	(set-help-pen-color wb:*gray-color*))

 (define-qmbar-command com-White-QEHWPCSB ()
 	(set-help-pen-color wb:*white-color*))

 (define-qmbar-command com-User-defined-QEHWPCSB ()
 	(set-help-pen-color (wb:prompt-user-for-color)))

 (define-qmbar-command com-Blue-QEHWPCSB ()
 	(set-help-pen-color wb:*blue-color*))

 (define-qmbar-command com-Bluel-QEHWPCSB ()
 	(set-help-pen-color wb:*light-blue-color*))

 (define-qmbar-command com-Brown-QEHWPCSB ()
 	(set-help-pen-color wb:*brown-color*))

 (define-qmbar-command com-Brownl-QEHWPCSB ()
 	(set-help-pen-color wb:*tan-color*))

 (define-qmbar-command com-Grayd-QEHWPCSB ()
 	(set-help-pen-color wb:*dark-gray-color*))

 (define-qmbar-command com-Grayl-QEHWPCSB ()
 	(set-help-pen-color wb:*light-gray-color*))

 (define-qmbar-command com-Green-QEHWPCSB ()
 	(set-help-pen-color wb:*green-color*))

 (define-qmbar-command com-Greend-QEHWPCSB ()
 	(set-help-pen-color wb:*dark-green-color*))

 (define-qmbar-command com-Orange-QEHWPCSB ()
 	(set-help-pen-color wb:*orange-color*))

 (define-qmbar-command com-Pink-QEHWPCSB ()
 	(set-help-pen-color wb:*pink-color*))

 (define-qmbar-command com-Purple-QEHWPCSB ()
 	(set-help-pen-color wb:*purple-color*))

 (define-qmbar-command com-Red-QEHWPCSB ()
 	(set-help-pen-color wb:*red-color*))

 (define-qmbar-command com-Yellow-QEHWPCSB ()
 	(set-help-pen-color wb:*yellow-color*))

 ;;; There are the colors
 ;;; now for the menus/command-tables
(make-command-table 'qehwpcsb-command-table
                     :errorp nil
                     :menu '(("Black" :command com-Black-QEHWPCSB)
                             ("Gray"      :command com-Gray-QEHWPCSB)
                             ("White" :command com-White-QEHWPCSB)
                             ("" :command :com-divider)
                             ("User defined" :command com-User-defined-QEHWPCSB)
                             ("" :command :com-divider)
                             ("Black" :command com-Black-QEHWPCSB)
                             ("Blue" :command com-Blue-QEHWPCSB)
                             ("Blue (light)" :command com-Bluel-QEHWPCSB)
                             ("Brown" :command com-Brown-QEHWPCSB)
                             ("Brown (light)" :command com-Brownl-QEHWPCSB)
                             ("Gray" :command com-Gray-QEHWPCSB)
                             ("Gray (dark)" :command com-Grayd-QEHWPCSB)
                             ("Gray (light)" :command com-Grayl-QEHWPCSB)
                             ("Green" :command com-Green-QEHWPCSB)
                             ("Green (dark)" :command com-Greend-QEHWPCSB)
                             ("Orange" :command com-Orange-QEHWPCSB)
                             ("Pink" :command com-Pink-QEHWPCSB)
                             ("Purple" :command com-Purple-QEHWPCSB)
                             ("Red" :command com-Red-QEHWPCSB)
                             ("White" :command com-White-QEHWPCSB)
                             ("Yellow" :command com-Yellow-QEIWPCSB)))
;;; and ..
;(make-command-table 'qehwpc-command-table :errorp nil
;	:menu '(("Pen color" :command com-HelpPenCol-QEHWPC)
;		("Pen color" :menu qehwpcsb-command-table)))
   ;;; END Q/E/HW/ ..Pen color

(define-qmbar-command com-InfUser-QEHWIW ()
	(inform-user "You must select a sub-item on this menu!"))   

(define-qmbar-command com-HelpWin-QEHWHIW ()
	(setf *help-in-windows* (not *help-in-windows*)))

(define-qmbar-command com-Yes-QEHWIWSB ()
	(setf *help-in-windows* T))

(define-qmbar-command com-No-QEHWIWSB ()
	(setf *help-in-windows* nil))

(make-command-table 'qehwhiwsb-command-table :errorp nil
	:menu '(("Yes .. in Windows" :command com-Yes-QEHWIWSB)
		("No .. in the listener" :command com-No-QEHWIWSB)))	   

(make-command-table 'qehwhiw-command-table :errorp nil
	:menu '(("Help in Windows" :command com-HelpWin-QEHWHIW)
		("Help Windows" :menu qehwhiwsb-command-table)
		("Background color" :menu qehwbgsb-command-table)
		("Pen color" :menu qehwpcsb-command-table)))


(make-command-table 'qehw-command-table :errorp nil
	:menu '(("Inform User" :command com-InfUser-QEHWIW)
		("Help" :menu qehwhiw-command-table)))

  ;;; END Q/E/Help window

  ;;; START Q/E/Information window
   ;;; START Q/E/IW/Background color
;;; for testing purposes
(defvar *default-info-background-color* nil)
(defvar *default-info-pen-color* nil)
(defvar *default-help-pen-color* nil)
(defvar *help-in-windows* nil)
;;; The initial command 
(define-qmbar-command com-InfoBgCol-QEIWBG ()
	(set-info-background-color
              (wb:prompt-user-for-color)))
;;; the sub-menu
(define-qmbar-command com-Black-QEIWBGSB ()
 	(setf *default-info-background-color* wb:*black-color*))

 (define-qmbar-command com-Gray-QEIWBGSB ()
 	(setf *default-info-background-color* wb:*gray-color*))

 (define-qmbar-command com-White-QEIWBGSB ()
 	(setf *default-info-background-color* wb:*white-color*))

 (define-qmbar-command com-User-defined-QEIWBGSB ()
 	(setf *default-info-background-color* (wb:prompt-user-for-color)))

 (define-qmbar-command com-Blue-QEIWBGSB ()
 	(setf *default-info-background-color* wb:*blue-color*))

 (define-qmbar-command com-Bluel-QEIWBGSB ()
 	(setf *default-info-background-color* wb:*light-blue-color*))

 (define-qmbar-command com-Brown-QEIWBGSB ()
 	(setf *default-info-background-color* wb:*brown-color*))

 (define-qmbar-command com-Brownl-QEIWBGSB ()
 	(setf *default-info-background-color* wb:*tan-color*))

 (define-qmbar-command com-Grayd-QEIWBGSB ()
 	(setf *default-info-background-color* wb:*dark-gray-color*))

 (define-qmbar-command com-Grayl-QEIWBGSB ()
 	(setf *default-info-background-color* wb:*light-gray-color*))

 (define-qmbar-command com-Green-QEIWBGSB ()
 	(setf *default-info-background-color* wb:*green-color*))

 (define-qmbar-command com-Greend-QEIWBGSB ()
 	(setf *default-info-background-color* wb:*dark-green-color*))

 (define-qmbar-command com-Orange-QEIWBGSB ()
 	(setf *default-info-background-color* wb:*orange-color*))

 (define-qmbar-command com-Pink-QEIWBGSB ()
 	(setf *default-info-background-color* wb:*pink-color*))

 (define-qmbar-command com-Purple-QEIWBGSB ()
 	(setf *default-info-background-color* wb:*purple-color*))

 (define-qmbar-command com-Red-QEIWBGSB ()
 	(setf *default-info-background-color* wb:*red-color*))

 (define-qmbar-command com-Yellow-QEIWBGSB ()
 	(setf *default-info-background-color* wb:*yellow-color*))

 ;;; There are the colors
 ;;; now for the menus/command-tables
(make-command-table 'qeiwbgsb-command-table
                     :errorp nil
                     :menu '(("Black" :command com-Black-QEIWBGSB)
                             ("Gray"      :command com-Gray-QEIWBGSB)
                             ("White" :command com-White-QEIWBGSB)
                             ("" :command :com-divider)
                             ("User defined" :command com-User-defined-QEIWBGSB)
                             ("" :command :com-divider)
                             ("Black" :command com-Black-QEIWBGSB)
                             ("Blue" :command com-Blue-QEIWBGSB)
                             ("Blue (light)" :command com-Bluel-QEIWBGSB)
                             ("Brown" :command com-Brown-QEIWBGSB)
                             ("Brown (light)" :command com-Brownl-QEIWBGSB)
                             ("Gray" :command com-Gray-QEIWBGSB)
                             ("Gray (dark)" :command com-Grayd-QEIWBGSB)
                             ("Gray (light)" :command com-Grayl-QEIWBGSB)
                             ("Green" :command com-Green-QEIWBGSB)
                             ("Green (dark)" :command com-Greend-QEIWBGSB)
                             ("Orange" :command com-Orange-QEIWBGSB)
                             ("Pink" :command com-Pink-QEIWBGSB)
                             ("Purple" :command com-Purple-QEIWBGSB)
                             ("Red" :command com-Red-QEIWBGSB)
                             ("White" :command com-White-QEIWBGSB)
                             ("Yellow" :command com-Yellow-QEIWBGSB)))
;;; and ..
;(make-command-table 'qeiwbg-command-table :errorp nil
;	:menu '(("Background color" :command com-InfoBgCol-QEIWBG)
;		("Background color" :menu qeiwbgsb-command-table)))

   ;;; END Q/E/IW/Background color

   ;;; START Q/E/IW/Pen color

;;; The initial command 
(define-qmbar-command com-InfoPenCol-QEIWPC ()
	(set-info-pen-color
              (wb:prompt-user-for-color)))
;;; the sub-menu
(define-qmbar-command com-Black-QEIWPCSB ()
 	(setf *default-info-pen-color* wb:*black-color*))

 (define-qmbar-command com-Gray-QEIWPCSB ()
 	(setf *default-info-pen-color* wb:*gray-color*))

 (define-qmbar-command com-White-QEIWPCSB ()
 	(setf *default-info-pen-color* wb:*white-color*))

 (define-qmbar-command com-User-defined-QEIWPCSB ()
 	(setf *default-info-pen-color* (wb:prompt-user-for-color)))

 (define-qmbar-command com-Blue-QEIWPCSB ()
 	(setf *default-info-pen-color* wb:*blue-color*))

 (define-qmbar-command com-Bluel-QEIWPCSB ()
 	(setf *default-info-pen-color* wb:*light-blue-color*))

 (define-qmbar-command com-Brown-QEIWPCSB ()
 	(setf *default-info-pen-color* wb:*brown-color*))

 (define-qmbar-command com-Brownl-QEIWPCSB ()
 	(setf *default-info-pen-color* wb:*tan-color*))

 (define-qmbar-command com-Grayd-QEIWPCSB ()
 	(setf *default-info-pen-color* wb:*dark-gray-color*))

 (define-qmbar-command com-Grayl-QEIWPCSB ()
 	(setf *default-info-pen-color* wb:*light-gray-color*))

 (define-qmbar-command com-Green-QEIWPCSB ()
 	(setf *default-info-pen-color* wb:*green-color*))

 (define-qmbar-command com-Greend-QEIWPCSB ()
 	(setf *default-info-pen-color* wb:*dark-green-color*))

 (define-qmbar-command com-Orange-QEIWPCSB ()
 	(setf *default-info-pen-color* wb:*orange-color*))

 (define-qmbar-command com-Pink-QEIWPCSB ()
 	(setf *default-info-pen-color* wb:*pink-color*))

 (define-qmbar-command com-Purple-QEIWPCSB ()
 	(setf *default-info-pen-color* wb:*purple-color*))

 (define-qmbar-command com-Red-QEIWPCSB ()
 	(setf *default-info-pen-color* wb:*red-color*))

 (define-qmbar-command com-Yellow-QEIWPCSB ()
 	(setf *default-info-pen-color* wb:*yellow-color*))

 ;;; There are the colors
 ;;; now for the menus/command-tables
(make-command-table 'qeiwpcsb-command-table
                     :errorp nil
                     :menu '(("Black" :command com-Black-QEIWPCSB)
                             ("Gray"      :command com-Gray-QEIWPCSB)
                             ("White" :command com-White-QEIWPCSB)
                             ("" :command :com-divider)
                             ("User defined" :command com-User-defined-QEIWPCSB)
                             ("" :command :com-divider)
                             ("Black" :command com-Black-QEIWPCSB)
                             ("Blue" :command com-Blue-QEIWPCSB)
                             ("Blue (light)" :command com-Bluel-QEIWPCSB)
                             ("Brown" :command com-Brown-QEIWPCSB)
                             ("Brown (light)" :command com-Brownl-QEIWPCSB)
                             ("Gray" :command com-Gray-QEIWPCSB)
                             ("Gray (dark)" :command com-Grayd-QEIWPCSB)
                             ("Gray (light)" :command com-Grayl-QEIWPCSB)
                             ("Green" :command com-Green-QEIWPCSB)
                             ("Green (dark)" :command com-Greend-QEIWPCSB)
                             ("Orange" :command com-Orange-QEIWPCSB)
                             ("Pink" :command com-Pink-QEIWPCSB)
                             ("Purple" :command com-Purple-QEIWPCSB)
                             ("Red" :command com-Red-QEIWPCSB)
                             ("White" :command com-White-QEIWPCSB)
                             ("Yellow" :command com-Yellow-QEIWPCSB)))
;;; and ..
;(make-command-table 'qeiwpc-command-table :errorp nil
;	:menu '(("Background color" :command com-InfoPenCol-QEIWPC)
;		("Pen color" :menu qeiwpcsb-command-table)))

   ;;; END Q/E/IW/Pen color
;;; an initial command

(define-qmbar-command com-InfUser-QEIW ()
	(inform-user "You must select a sub-item on this menu!"))

;;; the QEIW command-table

(make-command-table 'qeiw-command-table 
	:errorp nil
	:menu '(("Inform User" :command com-InfUser-QEIW)
		("Background color" :menu qeiwbgsb-command-table)
		("Pen color" :menu qeiwpcsb-command-table)))

  ;;; END Q/E/Information window

  ;;; START Q/E/Canvas parameters
(defvar *default-canvas-background-color* nil)
(defvar *default-canvas-pen-color* nil)
  ;;; START Q/E/CP/Default background color
(define-qmbar-command com-QECPDBC ()
        (setf wb:*default-canvas-background-color*
              (wb:prompt-user-for-color)))
;;                
(define-qmbar-command com-Black-QECPBCSB ()
 	(setf *default-canvas-background-color* wb:*black-color*))

 (define-qmbar-command com-Gray-QECPBCSB ()
 	(setf *default-canvas-background-color* wb:*gray-color*))

 (define-qmbar-command com-White-QECPBCSB ()
 	(setf *default-canvas-background-color* wb:*white-color*))

 (define-qmbar-command com-User-defined-QECPBCSB ()
 	(setf *default-canvas-background-color* (wb:prompt-user-for-color)))

 (define-qmbar-command com-Blue-QECPBCSB ()
 	(setf *default-canvas-background-color* wb:*blue-color*))

 (define-qmbar-command com-Bluel-QECPBCSB ()
 	(setf *default-canvas-background-color* wb:*light-blue-color*))

 (define-qmbar-command com-Brown-QECPBCSB ()
 	(setf *default-canvas-background-color* wb:*brown-color*))

 (define-qmbar-command com-Brownl-QECPBCSB ()
 	(setf *default-canvas-background-color* wb:*tan-color*))

 (define-qmbar-command com-Grayd-QECPBCSB ()
 	(setf *default-canvas-background-color* wb:*dark-gray-color*))

 (define-qmbar-command com-Grayl-QECPBCSB ()
 	(setf *default-canvas-background-color* wb:*light-gray-color*))

 (define-qmbar-command com-Green-QECPBCSB ()
 	(setf *default-canvas-background-color* wb:*green-color*))

 (define-qmbar-command com-Greend-QECPBCSB ()
 	(setf *default-canvas-background-color* wb:*dark-green-color*))

 (define-qmbar-command com-Orange-QECPBCSB ()
 	(setf *default-canvas-background-color* wb:*orange-color*))

 (define-qmbar-command com-Pink-QECPBCSB ()
 	(setf *default-canvas-background-color* wb:*pink-color*))

 (define-qmbar-command com-Purple-QECPBCSB ()
 	(setf *default-canvas-background-color* wb:*purple-color*))

 (define-qmbar-command com-Red-QECPBCSB ()
 	(setf *default-canvas-background-color* wb:*red-color*))

 (define-qmbar-command com-Yellow-QECPBCSB ()
 	(setf *default-canvas-background-color* wb:*yellow-color*))

 (make-command-table 'qecpbcsb-command-table
                     :errorp nil
                     :menu '(("Black" :command com-Black-QECPBCSB)
                             ("Gray"      :command com-Gray-QECPBCSB)
                             ("White" :command com-White-QECPBCSB)
                             ("" :command :com-divider)
                             ("User defined" :command com-User-defined-QECPBCSB)
                             ("" :command :com-divider)
                             ("Black" :command com-Black-QECPBCSB)
                             ("Blue" :command com-Blue-QECPBCSB)
                             ("Blue (light)" :command com-Bluel-QECPBCSB)
                             ("Brown" :command com-Brown-QECPBCSB)
                             ("Brown (light)" :command com-Brownl-QECPBCSB)
                             ("Gray" :command com-Gray-QECPBCSB)
                             ("Gray (dark)" :command com-Grayd-QECPBCSB)
                             ("Gray (light)" :command com-Grayl-QECPBCSB)
                             ("Green" :command com-Green-QECPBCSB)
                             ("Green (dark)" :command com-Greend-QECPBCSB)
                             ("Orange" :command com-Orange-QECPBCSB)
                             ("Pink" :command com-Pink-QECPBCSB)
                             ("Purple" :command com-Purple-QECPBCSB)
                             ("Red" :command com-Red-QECPBCSB)
                             ("White" :command com-White-QECPBCSB)
                             ("Yellow" :command com-Yellow-QECPBCSB)))
;;; The command table has a command and then a sub-menu
;;; Ultimately all this needs to become a sub-menu 
;(make-command-table 'qecpbc-command-table :errorp nil
;	:menu '(("Default background color" :command com-QECPDBC)
;		("Default canvas background color" :menu qecpbcsb-command-table)))
	                              
  ;;; END Q/E/CP/Default background color
  ;;; START Q/E/CP/Default pen color
(define-qmbar-command com-QECPDPC ()
        (setf wb:*default-canvas-pen-color*
              (wb:prompt-user-for-color)))
                
(define-qmbar-command com-Black-QECPPCSB ()
 	(setf *default-canvas-pen-color* wb:*black-color*))

 (define-qmbar-command com-Gray-QECPPCSB ()
 	(setf *default-canvas-pen-color* wb:*gray-color*))

 (define-qmbar-command com-White-QECPPCSB ()
 	(setf *default-canvas-pen-color* wb:*white-color*))

 (define-qmbar-command com-User-defined-QECPPCSB ()
 	(setf *default-canvas-pen-color* (wb:prompt-user-for-color)))

 (define-qmbar-command com-Blue-QECPPCSB ()
 	(setf *default-canvas-pen-color* wb:*blue-color*))

 (define-qmbar-command com-Bluel-QECPPCSB ()
 	(setf *default-canvas-pen-color* wb:*light-blue-color*))

 (define-qmbar-command com-Brown-QECPPCSB ()
 	(setf *default-canvas-pen-color* wb:*brown-color*))

 (define-qmbar-command com-Brownl-QECPPCSB ()
 	(setf *default-canvas-pen-color* wb:*tan-color*))

 (define-qmbar-command com-Grayd-QECPPCSB ()
 	(setf *default-canvas-pen-color* wb:*dark-gray-color*))

 (define-qmbar-command com-Grayl-QECPPCSB ()
 	(setf *default-canvas-pen-color* wb:*light-gray-color*))

 (define-qmbar-command com-Green-QECPPCSB ()
 	(setf *default-canvas-pen-color* wb:*green-color*))

 (define-qmbar-command com-Greend-QECPPCSB ()
 	(setf *default-canvas-pen-color* wb:*dark-green-color*))

 (define-qmbar-command com-Orange-QECPPCSB ()
 	(setf *default-canvas-pen-color* wb:*orange-color*))

 (define-qmbar-command com-Pink-QECPPCSB ()
 	(setf *default-canvas-pen-color* wb:*pink-color*))

 (define-qmbar-command com-Purple-QECPPCSB ()
 	(setf *default-canvas-pen-color* wb:*purple-color*))

 (define-qmbar-command com-Red-QECPPCSB ()
 	(setf *default-canvas-pen-color* wb:*red-color*))

 (define-qmbar-command com-Yellow-QECPPCSB ()
 	(setf *default-canvas-background-color* wb:*yellow-color*))

 (make-command-table 'qecppcsb-command-table
                     :errorp nil
                     :menu '(("Black" :command com-Black-QECPPCSB)
                             ("Gray"      :command com-Gray-QECPPCSB)
                             ("White" :command com-White-QECPPCSB)
                             ("" :command :com-divider)
                             ("User defined" :command com-User-defined-QECPPCSB)
                             ("" :command :com-divider)
                             ("Black" :command com-Black-QECPPCSB)
                             ("Blue" :command com-Blue-QECPPCSB)
                             ("Blue (light)" :command com-Bluel-QECPPCSB)
                             ("Brown" :command com-Brown-QECPPCSB)
                             ("Brown (light)" :command com-Brownl-QECPPCSB)
                             ("Gray" :command com-Gray-QECPPCSB)
                             ("Gray (dark)" :command com-Grayd-QECPPCSB)
                             ("Gray (light)" :command com-Grayl-QECPPCSB)
                             ("Green" :command com-Green-QECPPCSB)
                             ("Green (dark)" :command com-Greend-QECPPCSB)
                             ("Orange" :command com-Orange-QECPPCSB)
                             ("Pink" :command com-Pink-QECPPCSB)
                             ("Purple" :command com-Purple-QECPPCSB)
                             ("Red" :command com-Red-QECPPCSB)
                             ("White" :command com-White-QECPPCSB)
                             ("Yellow" :command com-Yellow-QECPPCSB)))
;;; The command table has a command and then a sub-menu
;(make-command-table 'qecppc-command-table :errorp nil
;	:menu '(("Default pen color" :command com-QECPDPC)
;		("Default canvas pen color" :menu qecppcsb-command-table)))
;;; Ultimately all this needs to become a sub-menu 
  ;;; END Q/E/CP/Default pen color

  ;;; START Q/E/CP/Device type
   ;;; Device type's initial command
(define-qmbar-command com-Devicetype-QECPDT ()
	(wb::set-device-type
                       (wb::prompt-user
                        :prompt-string
                        "Enter one of :color, :gray-scale, or 
:black&white."
                        :result-type 'symbol ;09dec2024
                        :read-type :eval)))

   ;;; its submenu
(define-qmbar-command com-Color-QECPDTSB ()
	(wb::set-device-type :color))

(define-qmbar-command com-Grayscale-QECPDTSB ()
	(wb::set-device-type :gray-scale))

(define-qmbar-command com-Blackwhite-QECPDTSB ()
	(wb::set-device-type :black&white))

(define-qmbar-command com-Other-QECPDTSB ()
	(wb::set-device-type
                   (wb::prompt-user
                    :prompt-string
                    (format NIL
                            "Enter one of ~s."
                            wb::*device-types*)
                    :result-type 'symbol ;09dec2024
                    :read-type :eval)))

(make-command-table 'qecpdtsb-command-table :errorp nil
	:menu '(("Color" :command com-Color-QECPDTSB)
		("Gray scale" :command com-Grayscale-QECPDTSB)
		("Black and White" :command com-Blackwhite-QECPDTSB)
		("Other" :command com-Other-QECPDTSB)))

;(make-command-table 'qecpdt-command-table :errorp nil
;	:menu '(("Device type" :command com-Devicetype-QECPDT)
;		("Set device type" :menu qecpdtsb-command-table)))

  ;;; END Q/E/CP/Device type

  ;;; START Q/E/CP/Default canvas position

   ;;; for testing purposes
   (defvar wb::*default-canvas-region* nil)
   ;;; The initial command
(define-qmbar-command com-DefCnvPos-QECPCP ()
	(quail-print "You must select a sub-item on this menu!"))

   ;;; the sub-menu
(define-qmbar-command com-BotLeftCnr-QECPCPSB ()
	(wb::set-up-default-canvas-region  10 10 400 300))

(define-qmbar-command com-TopLeftCnr-QECPCPSB ()
	(wb::set-up-default-canvas-region
           10
           (- (wb::screen-height) 300)
           400 300))

(define-qmbar-command com-BotRightCnr-QECPCPSB ()
	(wb::set-up-default-canvas-region
           (- (wb::screen-width) 400)
           10
           400 300))

(define-qmbar-command com-CntrDply-QECPCPSB ()
	(wb::set-up-default-canvas-region
           (- (round (/ (wb::screen-width) 2)) 200)
           (- (round (/ (wb::screen-height) 2)) 150)
           400 300))

(define-qmbar-command com-Choose-QECPCPSB ()
	(setf wb::*default-canvas-region* NIL))

;;; The command-tables
(make-command-table 'qecpcpsb-command-table :errorp nil
	:menu '(("Bottom left corner" :command com-BotLeftCnr-QECPCPSB)
		("Top left corner" :command com-TopLeftCnr-QECPCPSB)
		("Bottom right corner" :command com-BotRightCnr-QECPCPSB)
		("Center of display" :command com-CntrDply-QECPCPSB)
		("Choose at creation" :command com-Choose-QECPCPSB)))

;(make-command-table 'qecpcp-command-table :errorp nil
;	:menu '(("Default canvas position" :command com-DefCnvPos-QECPCP)
;		("Default canvas position" :menu qecpcpsb-command-table)))

  ;;; END Q/E/CP/Default canvas position

(make-command-table 'qecp-command-table :errorp nil
	:menu '(("Default background color" :menu qecpbcsb-command-table)
		("Default pen color" :menu qecppcsb-command-table)
		("Device type" :menu qecpdtsb-command-table)
		("Default-canvas-region" :menu qecpcpsb-command-table)))

  ;;; END Q/E/Canvas parameters

  ;;; START Q/E/Default colors for views
;;;
;;; and I'll need to establish *default-X-color* or sbcl will complain
(defvar *default-label-color* nil)
(defvar *default-curve-color* nil)
(defvar *default-point-color* nil)
(defvar *default-highlight-color* nil)




 ;;; (Labels Thus we have to write
 (define-qmbar-command com-Black-QEVCL ()
 	(setf *default-label-color* wb:*black-color*))

 (define-qmbar-command com-Gray-QEVCL ()
 	(setf *default-label-color* wb:*gray-color*))

 (define-qmbar-command com-White-QEVCL ()
 	(setf *default-label-color* wb:*white-color*))

 (define-qmbar-command com-User-defined-QEVCL ()
 	(setf *default-label-color* (wb:prompt-user-for-color)))

 (define-qmbar-command com-Blue-QEVCL ()
 	(setf *default-label-color* wb:*blue-color*))

 (define-qmbar-command com-Bluel-QEVCL ()
 	(setf *default-label-color* wb:*light-blue-color*))

 (define-qmbar-command com-Brown-QEVCL ()
 	(setf *default-label-color* wb:*brown-color*))

 (define-qmbar-command com-Brownl-QEVCL ()
 	(setf *default-label-color* wb:*tan-color*))

 (define-qmbar-command com-Grayd-QEVCL ()
 	(setf *default-label-color* wb:*dark-gray-color*))

 (define-qmbar-command com-Grayl-QEVCL ()
 	(setf *default-label-color* wb:*light-gray-color*))

 (define-qmbar-command com-Green-QEVCL ()
 	(setf *default-label-color* wb:*green-color*))

 (define-qmbar-command com-Greend-QEVCL ()
 	(setf *default-label-color* wb:*dark-green-color*))

 (define-qmbar-command com-Orange-QEVCL ()
 	(setf *default-label-color* wb:*orange-color*))

 (define-qmbar-command com-Pink-QEVCL ()
 	(setf *default-label-color* wb:*pink-color*))

 (define-qmbar-command com-Purple-QEVCL ()
 	(setf *default-label-color* wb:*purple-color*))

 (define-qmbar-command com-Red-QEVCL ()
 	(setf *default-label-color* wb:*red-color*))

 (define-qmbar-command com-Yellow-QEVCL ()
 	(setf *default-label-color* wb:*yellow-color*))

 ;;; There are the colors
 ;;; now for the menus/command-tables
 ;;; (Labels colors command-tble)
(make-command-table 'qevcl-command-table
                     :errorp nil
                     :menu '(("Black" :command com-Black-QEVCL)
                             ("Gray"      :command com-Gray-QEVCL)
                             ("White" :command com-White-QEVCL)
                             ("" :command :com-divider)
                             ("User defined" :command com-User-defined-QEVCL)
                             ("" :command :com-divider)
                             ("Black" :command com-Black-QEVCL)
                             ("Blue" :command com-Blue-QEVCL)
                             ("Blue (light)" :command com-Bluel-QEVCL)
                             ("Brown" :command com-Brown-QEVCL)
                             ("Brown (light)" :command com-Brownl-QEVCL)
                             ("Gray" :command com-Gray-QEVCL)
                             ("Gray (dark)" :command com-Grayd-QEVCL)
                             ("Gray (light)" :command com-Grayl-QEVCL)
                             ("Green" :command com-Green-QEVCL)
                             ("Green (dark)" :command com-Greend-QEVCL)
                             ("Orange" :command com-Orange-QEVCL)
                             ("Pink" :command com-Pink-QEVCL)
                             ("Purple" :command com-Purple-QEVCL)
                             ("Red" :command com-Red-QEVCL)
                             ("White" :command com-White-QEVCL)
                             ("Yellow" :command com-Yellow-QEVCL)))
;;;
;;; (Curves Thus we have to write
 (define-qmbar-command com-Black-QEVCC ()
 	(setf *default-curve-color* wb:*black-color*))

 (define-qmbar-command com-Gray-QEVCC ()
 	(setf *default-curve-color* wb:*gray-color*))

 (define-qmbar-command com-White-QEVCC ()
 	(setf *default-curve-color* wb:*white-color*))

 (define-qmbar-command com-User-defined-QEVCC ()
 	(setf *default-curve-color* (wb:prompt-user-for-color)))

 (define-qmbar-command com-Blue-QEVCC ()
 	(setf *default-curve-color* wb:*blue-color*))

 (define-qmbar-command com-Bluel-QEVCC ()
 	(setf *default-curve-color* wb:*light-blue-color*))

 (define-qmbar-command com-Brown-QEVCC ()
 	(setf *default-curve-color* wb:*brown-color*))

 (define-qmbar-command com-Brownl-QEVCC ()
 	(setf *default-curve-color* wb:*tan-color*))

 (define-qmbar-command com-Grayd-QEVCC ()
 	(setf *default-curve-color* wb:*dark-gray-color*))

 (define-qmbar-command com-Grayl-QEVCC ()
 	(setf *default-curve-color* wb:*light-gray-color*))

 (define-qmbar-command com-Green-QEVCC ()
 	(setf *default-curve-color* wb:*green-color*))

 (define-qmbar-command com-Greend-QEVCC ()
 	(setf *default-curve-color* wb:*dark-green-color*))

 (define-qmbar-command com-Orange-QEVCC ()
 	(setf *default-curve-color* wb:*orange-color*))

 (define-qmbar-command com-Pink-QEVCC ()
 	(setf *default-curve-color* wb:*pink-color*))

 (define-qmbar-command com-Purple-QEVCC ()
 	(setf *default-curve-color* wb:*purple-color*))

 (define-qmbar-command com-Red-QEVCC ()
 	(setf *default-curve-color* wb:*red-color*))

 (define-qmbar-command com-Yellow-QEVCC ()
 	(setf *default-curve-color* wb:*yellow-color*))

 ;;; (Curves color-command-table
(make-command-table 'qevcc-command-table
                     :errorp nil
                     :menu '(("Black" :command com-Black-QEVCC)
                             ("Gray"      :command com-Gray-QEVCC)
                             ("White" :command com-White-QEVCC)
                             ("" :command :com-divider)
                             ("User defined" :command com-User-defined-QEVCC)
                             ("" :command :com-divider)
                             ("Black" :command com-Black-QEVCC)
                             ("Blue" :command com-Blue-QEVCC)
                             ("Blue (light)" :command com-Bluel-QEVCC)
                             ("Brown" :command com-Brown-QEVCC)
                             ("Brown (light)" :command com-Brownl-QEVCC)
                             ("Gray" :command com-Gray-QEVCC)
                             ("Gray (dark)" :command com-Grayd-QEVCC)
                             ("Gray (light)" :command com-Grayl-QEVCC)
                             ("Green" :command com-Green-QEVCC)
                             ("Green (dark)" :command com-Greend-QEVCC)
                             ("Orange" :command com-Orange-QEVCC)
                             ("Pink" :command com-Pink-QEVCC)
                             ("Purple" :command com-Purple-QEVCC)
                             ("Red" :command com-Red-QEVCC)
                             ("White" :command com-White-QEVCC)
                             ("Yellow" :command com-Yellow-QEVCC)))
;;; (Point symbols
(define-qmbar-command com-Black-QEVCP ()
 	(setf *default-point-color* wb:*black-color*))

 (define-qmbar-command com-Gray-QEVCP ()
 	(setf *default-point-color* wb:*gray-color*))

 (define-qmbar-command com-White-QEVCP ()
 	(setf *default-point-color* wb:*white-color*))

 (define-qmbar-command com-User-defined-QEVCP ()
 	(setf *default-point-color* (wb:prompt-user-for-color)))

 (define-qmbar-command com-Blue-QEVCP ()
 	(setf *default-point-color* wb:*blue-color*))

 (define-qmbar-command com-Bluel-QEVCP ()
 	(setf *default-point-color* wb:*light-blue-color*))

 (define-qmbar-command com-Brown-QEVCP ()
 	(setf *default-point-color* wb:*brown-color*))

 (define-qmbar-command com-Brownl-QEVCP ()
 	(setf *default-point-color* wb:*tan-color*))

 (define-qmbar-command com-Grayd-QEVCP ()
 	(setf *default-point-color* wb:*dark-gray-color*))

 (define-qmbar-command com-Grayl-QEVCP ()
 	(setf *default-point-color* wb:*light-gray-color*))

 (define-qmbar-command com-Green-QEVCP ()
 	(setf *default-point-color* wb:*green-color*))

 (define-qmbar-command com-Greend-QEVCP ()
 	(setf *default-point-color* wb:*dark-green-color*))

 (define-qmbar-command com-Orange-QEVCP ()
 	(setf *default-point-color* wb:*orange-color*))

 (define-qmbar-command com-Pink-QEVCP ()
 	(setf *default-point-color* wb:*pink-color*))

 (define-qmbar-command com-Purple-QEVCP ()
 	(setf *default-point-color* wb:*purple-color*))

 (define-qmbar-command com-Red-QEVCP ()
 	(setf *default-point-color* wb:*red-color*))

 (define-qmbar-command com-Yellow-QEVCP ()
 	(setf *default-point-color* wb:*yellow-color*))

 ;;; (Point symbols color-command-table
(make-command-table 'qevcp-command-table
                     :errorp nil
                     :menu '(("Black" :command com-Black-QEVCP)
                             ("Gray"      :command com-Gray-QEVCP)
                             ("White" :command com-White-QEVCP)
                             ("" :command :com-divider)
                             ("User defined" :command com-User-defined-QEVCP)
                             ("" :command :com-divider)
                             ("Black" :command com-Black-QEVCP)
                             ("Blue" :command com-Blue-QEVCP)
                             ("Blue (light)" :command com-Bluel-QEVCP)
                             ("Brown" :command com-Brown-QEVCP)
                             ("Brown (light)" :command com-Brownl-QEVCP)
                             ("Gray" :command com-Gray-QEVCP)
                             ("Gray (dark)" :command com-Grayd-QEVCP)
                             ("Gray (light)" :command com-Grayl-QEVCP)
                             ("Green" :command com-Green-QEVCP)
                             ("Green (dark)" :command com-Greend-QEVCP)
                             ("Orange" :command com-Orange-QEVCP)
                             ("Pink" :command com-Pink-QEVCP)
                             ("Purple" :command com-Purple-QEVCP)
                             ("Red" :command com-Red-QEVCP)
                             ("White" :command com-White-QEVCP)
                             ("Yellow" :command com-Yellow-QEVCP))) 

;;; (Highlighting
(define-qmbar-command com-Black-QEVCH ()
 	(setf *default-highlight-color* wb:*black-color*))

 (define-qmbar-command com-Gray-QEVCH ()
 	(setf *default-highlight-color* wb:*gray-color*))

 (define-qmbar-command com-White-QEVCH ()
 	(setf *default-highlight-color* wb:*white-color*))

 (define-qmbar-command com-User-defined-QEVCH ()
 	(setf *default-highlight-color* (wb:prompt-user-for-color)))

 (define-qmbar-command com-Blue-QEVCH ()
 	(setf *default-highlight-color* wb:*blue-color*))

 (define-qmbar-command com-Bluel-QEVCH ()
 	(setf *default-highlight-color* wb:*light-blue-color*))

 (define-qmbar-command com-Brown-QEVCH ()
 	(setf *default-highlight-color* wb:*brown-color*))

 (define-qmbar-command com-Brownl-QEVCH ()
 	(setf *default-highlight-color* wb:*tan-color*))

 (define-qmbar-command com-Grayd-QEVCH ()
 	(setf *default-highlight-color* wb:*dark-gray-color*))

 (define-qmbar-command com-Grayl-QEVCH ()
 	(setf *default-highlight-color* wb:*light-gray-color*))

 (define-qmbar-command com-Green-QEVCH ()
 	(setf *default-highlight-color* wb:*green-color*))

 (define-qmbar-command com-Greend-QEVCH ()
 	(setf *default-highlight-color* wb:*dark-green-color*))

 (define-qmbar-command com-Orange-QEVCH ()
 	(setf *default-highlight-color* wb:*orange-color*))

 (define-qmbar-command com-Pink-QEVCH ()
 	(setf *default-highlight-color* wb:*pink-color*))

 (define-qmbar-command com-Purple-QEVCH ()
 	(setf *default-highlight-color* wb:*purple-color*))

 (define-qmbar-command com-Red-QEVCH ()
 	(setf *default-highlight-color* wb:*red-color*))

 (define-qmbar-command com-Yellow-QEVCH ()
 	(setf *default-highlight-color* wb:*yellow-color*))

 ;;; (Highlighting color-command-table
(make-command-table 'qevch-command-table
                     :errorp nil
                     :menu '(("Black" :command com-Black-QEVCH)
                             ("Gray"      :command com-Gray-QEVCH)
                             ("White" :command com-White-QEVCH)
                             ("" :command :com-divider)
                             ("User defined" :command com-User-defined-QEVCH)
                             ("" :command :com-divider)
                             ("Black" :command com-Black-QEVCH)
                             ("Blue" :command com-Blue-QEVCH)
                             ("Blue (light)" :command com-Bluel-QEVCH)
                             ("Brown" :command com-Brown-QEVCH)
                             ("Brown (light)" :command com-Brownl-QEVCH)
                             ("Gray" :command com-Gray-QEVCH)
                             ("Gray (dark)" :command com-Grayd-QEVCH)
                             ("Gray (light)" :command com-Grayl-QEVCH)
                             ("Green" :command com-Green-QEVCH)
                             ("Green (dark)" :command com-Greend-QEVCH)
                             ("Orange" :command com-Orange-QEVCH)
                             ("Pink" :command com-Pink-QEVCH)
                             ("Purple" :command com-Purple-QEVCH)
                             ("Red" :command com-Red-QEVCH)
                             ("White" :command com-White-QEVCH)
                             ("Yellow" :command com-Yellow-QEVCH))) 



;;; And now move one level up
(make-command-table 'qevc-command-table
 		    :errorp nil
 		    :menu '(("Highlighting" :menu qevch-command-table)
 		    	("Point symbols" :menu qevcp-command-table)
 		    	("Curves" :menu qevcc-command-table)
 		    	("Labels" :menu qevcl-command-table)
 		    	)
 		    )

  ;;; END Q/E/Default colors for views
;;; END Q/Environment



(make-command-table 'qe-command-table
 		    :errorp nil
 		    :menu '(("Help window" :menu qehw-command-table)
 		    	("Information window" :menu qeiw-command-table)
 		    	("Canvas parameters" :menu qecp-command-table)
 		    	("Default colors for Views" :menu qevc-command-table))
 		    )

;;; for testing purposes
;(define-qmbar-command com-QSTUB ()
;	(format *standard-output* "This is a command stub.~%")
;	(finish-output *standard-output*))

(make-command-table 'q-command-table
 		    :errorp nil
 		    :menu '(("About Quail" :command com-AboutQ-QAQ);("About Quail" :menu qaq-command-table)
 		    	("Information" :menu qi-command-table)
 		    	("Examples" :menu qex-command-table)
 		    	("Datasets" :menu qdsb-command-table)
 		    	("Environment" :menu qe-command-table)
 		    	))
;;; End of Quail menu