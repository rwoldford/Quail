;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;
;;;
;;;                         gwb1-28Jan15.lsp                               

;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;
;;;
;;; 
;;; Copyright (c) 1991 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1991.
;;;
;;;----------------------------------------------------------------------
----------

(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export 
'(*quail-menu-items* add-to-quail-menu delete-from-quail-menu)))

(defun this-may-take-a-while ()
  (inform-user (format NIL
                       "This may take a while. ~%~
                        There is a lot of information to collect and 
load. ~%~
                        Patience please :-)."))
  T)


(defvar *quail-menu-items* NIL
  "The current menu items to be used to construct the Quail menu ~
   to be used in the Quail menubar.")

(defun quail-menu ()
  "Creates and returns the default Quail menu."
  (wb::make-menu
   :menu-type :title
   :items *quail-menu-items*
   :title "Quail")
  )


(defun add-to-quail-menu (item &rest other-items)
  "Adds one or more items to the list of items in the ~
   quail-menu-items. Returns the new menu-items list. ~
   Destructive to *quail-menu-items* ~
   (:required (:arg item The menu item list to be added.) )~
   (:rest (:arg other-items NIL Other item lists to be added.)) ~
   (:see-also wb:make-menu delete-from-quail-menu *quail-menu-items* ~
   install-default-quail-menubar) ~
   "
  (declare (special *quail-menu-items*))
  (nconc *quail-menu-items* (list item) other-items)
  *quail-menu-items*)


(defun delete-from-quail-menu (item &rest other-items)
  "Deletes one or more items from the list of items in the ~
   quail-menu-items. Returns the new menu-items list. ~
   Destructive to *quail-menu-items* ~
   (:required (:arg item The menu item list to be deleted.) )~
   (:rest (:arg other-items NIL Other item lists to be deleted.)) ~
   (:see-also wb:make-menu add-to-quail-menu *quail-menu-items* ~
   install-default-quail-menubar) ~
   "
  (declare (special *quail-menu-items*))
  (delete item *quail-menu-items* :test #'equal)
  (when other-items
    (dolist (item other-items)
      (delete item *quail-menu-items* :test #'equal)))
  *quail-menu-items*)




(<-
 *quail-menu-items*
 '(("About Quail"
      (inform-user
       (format NIL
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
               (qk::quail-release)))
      "Some credit information on Quail.")
   ("-" () "")
   ("Information" (help) "Get general help information."
    :sub-items
    (("Help" (help)
      "Get help on some symbol."
      :sub-items
      (("Help" (help)
        "Get help on some symbol.")
       ("Organized by topic" 
        (help 'Quail :topic)
        "Get the quail topic.")
       ("Symbol indices"
        (view-doc-index :package :quail)
        "View the most recent index of all the Quail symbols."
        :sub-items
        (("Quail external symbols"
          (view-doc-index :package :quail)
          "View the most recent index of all the Quail symbols.")
         ("Quail-Kernel external symbols"
          (view-doc-index :package :quail-kernel)
          "View the most recent index of all the Quail-Kernel symbols.")
         ("Views external symbols"
          (view-doc-index :package :views)
          "View the most recent index of all the views symbols.")
         ("Window Basics external symbols"
          (view-doc-index :package :window-basics)
          "View the most recent index of all the Window-basics 
symbols."))
        ))
      )
     ("Packages" (help (find-symbol "PACKAGES"
                                    (find-package "Q-USER"))
                       :topic)
      "General package information."
      :sub-items
      (("About packages"
        (help (find-symbol "PACKAGES"
                                    (find-package "Q-USER"))
                       :topic)
        "General package information.")
       ("Quail-User" (and (this-may-take-a-while)
                          ;; avoid interning 'quail-user in the :quail package
                          (help :q-user :package))
        "Get help on the quail-user package.")
       ("Quail" (and (this-may-take-a-while)
                     (help :quail :package)) "Get help on the quail 
package.")
       ("Quail-Kernel" (and (this-may-take-a-while)
                            (help :quail-kernel :package))
        "Get help on the quail-kernel package.")
       ("Window-Basics" (and (this-may-take-a-while)
                             (help :window-basics :package))
        "Get help on the window-basics package.")
       ("Views" (and (this-may-take-a-while)
                     (help :views :package))
        "Get help on the views package.")
       ))
     ("Browser" (make-browser) "General class browser."
      :sub-items
      (("Class browser" (make-browser) "General class browser.")
       ("Class browse all of Quail"
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
                          (find-class s nil))))))
        "Browse all classes defined in Quail.")
       ("Class browse the Quail package"
        (apply #'class-browse
               (qk::list-symbols
                :quail
                :test
                #'(lambda (s)
                    (and (or
                          (eq (symbol-package s)
                              (find-package :quail))
                          )
                         (find-class s nil)))))
        "Browse all classes defined in Quail.")
       ("Class browse the Quail-kernel package"
        (apply #'class-browse
               (qk::list-symbols
                :quail-kernel
                :test
                #'(lambda (s)
                    (and (or
                          (eq (symbol-package s)
                              (find-package :quail-kernel))
                          )
                         (find-class s nil)))))
        "Browse all classes defined in the Quail-kernel package.")
       ("Class browse the views package"
        (apply #'class-browse
               (qk::list-symbols
                :views
                :test
                #'(lambda (s)
                    (and (eq (symbol-package s)
                             (find-package :views))
                         (find-class s nil)))))
        "Browse all classes in the views package.")
       ("Class browse window-basics"
        (apply #'class-browse
               (qk::list-symbols :window-basics
                                 :test
                                 #'(lambda (s) (and (eq (symbol-package 
s)
                                                        (find-package 
:window-basics))
                                                    (find-class s 
nil)))))
        "Browse all classes in the window-basics package.")
       
       ))
     ))
   ("Examples" 
    (inform-user 
       (format NIL
               "Examples can be found in the directory eg: ~&~
                There are more files there than are presented here."))
    "Examples can be found in the directory eq:"
    :sub-items
    (("Where are the files?" 
      (inform-user 
       (format NIL
               "Examples can be found in the directory ~a ~&~
                or more simply \"eg:\". ~&~
                There are more files there than are presented here."
               (truename (pathname "eg:"))))
      "Examples can be found in the directory eg:")
     ("-" () "")
     ("Arrays in Quail" (edit-file "eg:Arrays;overview.lsp")
      "Overview and entry point into example files on arrays in Quail"
      :sub-items
      (("Overview" (edit-file "eg:Arrays;overview.lsp")
        "Overview and entry point into example files on arrays in Quail")

       ("Introduction" (edit-file "eg:Arrays;intro.lsp")
        "Introduction to array creation and array attributes.")
       ("-" () "")
       ("Advanced array creation" (edit-file "eg:Arrays;array.lsp")
        "Advanced use of the array function.")
       ("Arithmetic operations" (edit-file "eg:Arrays;arith-ops.lsp")
        "Arithmetic operations on arrays")
       ("Glueing operations"
        (edit-file "eg:Arrays;glue.lsp")
        "Putting arrays together with glue.")
       ("Iteration"
        (edit-file "eg:Arrays;iter-elements.lsp")
        "Macros useful in iterating over elements."
        :sub-items
        (("Elements"
          (edit-file "eg:Arrays;iter-elements.lsp")
          "Macros useful in iterating over elements.")
         ("General"
          (edit-file "eg:Arrays;iter-general.lsp")
          "General iteration macros.")
         ("Mapping functions"
          (edit-file "eg:Arrays;iter-map.lsp")
          "Mapping functions over arrays.")
         ("Modifying slices"
          (edit-file "eg:Arrays;iter-modify.lsp")
          "Removal and substitution of slices.")
         ("Slices"
          (edit-file "eg:Arrays;iter-slices.lsp")
          "Macros useful in iterating over slices.")
         ))
       ("Mathematical operations" (edit-file "eg:Arrays;math-funs.lsp")
        "Mathematical operations on arrays")
       ("Matrices"
        (edit-file "eg:Arrays;Matrices;overview.lsp")
        "Focus on matrix arrays.")
       ("Numerical predicates" (edit-file "eg:Arrays;num-preds.lsp")
        "Numerical predicates like =, <, <=, >, >=")
       ("Referencing and copying" (edit-file "eg:Arrays;ref.lsp")
        "Referencing and setting blocks of an array.")
       ("Searching arrays"
        (edit-file "eg:Arrays;search.lsp")
        "Finding and counting slices; slice positions.")
       ("Selecting elements by test"
        (edit-file "eg:Arrays;select-by-pred.lsp")
        "Selecting elements by predicate testing.")
       ("Some handy arrays" (edit-file "eg:Arrays;handy-arrays.lsp")
        "Some handy arrays.")
       ("Sorting, ranking, permuting"
        (edit-file "eg:Arrays;sort.lsp")
        "Sorting, ranking, permuting slices of an array.")
       )
      )
     ("Documentation"
      (edit-file "eg:Documentation;doc-example.lsp")
      "Quail's extended documentation facility.")
     ("Mathematics"
      ()
      "Examples illustrating some mathematical functionality."
      :sub-items
      (("Arithmetic operations" (edit-file "eg:Arrays;arith-ops.lsp")
        "Arithmetic operations on arrays")
       ("Calculus"
        ()
        "Collection of tools from the differential calculus."
        :sub-items
        (("Derivatives"
          (edit-file "eg:Mathematics;Calculus;deriv.lsp")
          "Symbolic and numerical differentiation.")
        ;; ("Integration"
        ;;  (edit-file "eg:Mathematics;Calculus;integrate.lsp")
        ;;  "Numerical integration.")
         )
        )
       ("Combinatorics"
        (edit-file "eg:Mathematics;Combinatorics;counting.lsp")
        "Collection of some simple combinatorial tools."
        :sub-items
        (("Counting"
          (edit-file "eg:Mathematics;Combinatorics;counting.lsp")
          "Collection of some simple counting tools.")
         ("Factors"
          (edit-file "eg:Mathematics;Combinatorics;factor.lsp")
          "Collection of some simple tools related to factoring 
integers."
          )
         )
        )
       ("Extended arithmetic"
        (edit-file "eg:Mathematics;extended-arithmetic.lsp")
        "Quail's handling of extended arithmetic.")
       ("Mathematical operations (CL)" 
        (edit-file "eg:Arrays;math-funs.lsp")
        "Mathematical operations on arrays")
       ("Matrices"
        (edit-file "eg:Arrays;Matrices;overview.lsp")
        "Focus on matrix arrays."
        :sub-items
        (("Overview"
          (edit-file "eg:Arrays;Matrices;overview.lsp")
          "Focus on matrix arrays.")
         ("Introduction"
          (edit-file "eg:Arrays;Matrices;intro.lsp")
          "Introduction to properties of Quail matrices.")
         ("-" () "")
         ("Decompositions"
          (edit-file
           "eg:Arrays;Matrices;Decompositions;overview.lsp")
          "Various matrix decompositions."
          :sub-items
          (("Overview"
            (edit-file
             "eg:Arrays;Matrices;Decompositions;overview.lsp")
            "Various matrix decompositions.")
           ("-" () "")
           ("Cholesky decomposition"
            (edit-file
             "eg:Arrays;Matrices;Decompositions;cholesky.lsp")
            "The Cholesky decomposition of a symmetric matrix.")
           ("LU decomposition"
            (edit-file
             "eg:Arrays;Matrices;Decompositions;lu.lsp")
            "The LU decomposition of a square matrix.")
           ("QR decomposition"
            (edit-file
             "eg:Arrays;Matrices;Decompositions;qr.lsp")
            "The QR decomposition of a rectangular matrix.")
           ("Singular value decomposition"
            (edit-file
             "eg:Arrays;Matrices;Decompositions;svd.lsp")
            "The Singular value decomposition of a rectangular matrix.")
           )
          )
         ("Matrix operations"
          (edit-file "eg:Arrays;Matrices;operations.lsp")
          "Focus on matrix operations, mostly mathematical.")
         )
        )
       ("Special functions" 
        (edit-file "eg:Mathematics;Special-Functions;overview.lsp")
        "Special Mathematical functions."
        :sub-items
        (("Overview"
          (edit-file "eg:Mathematics;Special-Functions;overview.lsp")
          "Special mathematical functions.")
         ("-" () "")
         ("Beta functions"
          (edit-file "eg:Mathematics;Special-Functions;beta.lsp")
          "Beta and incomplete beta functions.")
         ("Continued fractions"
          (edit-file 
"eg:Mathematics;Special-Functions;continued-fraction.lsp")
          "Approximating arbitrary continued fraction expansions.")
         ("Error functions"
          (edit-file "eg:Mathematics;Special-Functions;error-fun.lsp")
          "Error function and its complement.")
         ("Gamma functions"
          (edit-file "eg:Mathematics;Special-Functions;gamma.lsp")
          "Gamma and incomplete gamma functions."))
        )
       )
      )
     ("Probability" ()
      "Probability calculations and random variables."
      :sub-items
      (("Distributions"
        (edit-file "eg:Probability;Distributions;overview.lsp")
        "Overview and entry point into example files on statistical 
distributions in Quail"
        :sub-items
        (("Overview"
          (edit-file "eg:Probability;Distributions;overview.lsp")
          "Overview and entry point into example files on statistical 
distributions in Quail")
         ("-"
          ()
          "")
         ("Introduction"
          (edit-file "eg:Probability;Distributions;intro.lsp")
          "Introduction to distributions in Quail.")
         ("Built in distributions."
          (edit-file "eg:Probability;Distributions;stock.lsp")
          "The built-in distributions in Quail."
          :sub-items
          (("Overview."
            (edit-file "eg:Probability;Distributions;stock.lsp")
            "The built-in distributions in Quail."
            )
           ("-"
            ()
            "")
           ("Continuous distributions."
            (edit-file "eg:Probability;Distributions;stock-cts.lsp")
            "The built-in continuous distributions in Quail."
            )
           ("Beta"
            (edit-file "eg:Probability;Distributions;beta.lsp")
            "The beta distribution in Quail.")
           ("Cauchy"
            (edit-file "eg:Probability;Distributions;cauchy.lsp")
            "The Cauchy distribution in Quail.")
           ("Chi-squared"
            (edit-file "eg:Probability;Distributions;chi-squared.lsp")
            "The Chi-squared distribution in Quail.")
           ("Exponential"
            (edit-file "eg:Probability;Distributions;exponential.lsp")
            "The exponential distribution in Quail.")
           ("F"
            (edit-file "eg:Probability;Distributions;F-dist.lsp")
            "The F distribution in Quail.")
           ("Gamma"
            (edit-file "eg:Probability;Distributions;gamma.lsp")
            "The gamma distribution in Quail.")
           ("Gaussian (Normal)"
            (edit-file "eg:Probability;Distributions;gaussian.lsp")
            "The Gaussian distribution in Quail.")
           ("K"
            (edit-file "eg:Probability;Distributions;K-dist.lsp")
            "The K distribution in Quail.")
           ("Pareto"
            (edit-file "eg:Probability;Distributions;pareto.lsp")
            "The Pareto distribution in Quail.")
           ("Student's t"
            (edit-file "eg:Probability;Distributions;student.lsp")
            "The student distribution in Quail.")
           ("Uniform"
            (edit-file "eg:Probability;Distributions;uniform.lsp")
            "The uniform distribution in Quail.")
           ("Weibull"
            (edit-file "eg:Probability;Distributions;weibull.lsp")
            "The weibull distribution in Quail.")
           ("-"
            ()
            "")
           ("Discrete distributions."
            (edit-file "eg:Probability;Distributions;stock-disc.lsp")
            "The built-in discrete distributions in Quail.")
           ("Bernoulli"
            (edit-file "eg:Probability;Distributions;bernoulli.lsp")
            "The Bernoulli distribution in Quail.")
           ("Binomial"
            (edit-file "eg:Probability;Distributions;binomial.lsp")
            "The Binomial distribution in Quail.")
           ("Geometric"
            (edit-file "eg:Probability;Distributions;geometric.lsp")
            "The geometric distribution in Quail.")
           ("Hypergeometric"
            (edit-file "eg:Probability;Distributions;hypergeometric.lsp")

            "The Hypergeometric distribution in Quail.")
           ("Negative binomial"
            (edit-file 
"eg:Probability;Distributions;negative-binomial.lsp")
            "The negative binomial distribution in Quail.")
           ("Poisson"
            (edit-file "eg:Probability;Distributions;poisson.lsp")
            "The Poisson distribution in Quail.")
           ("Uniform (discrete)"
            (edit-file 
"eg:Probability;Distributions;discrete-uniform.lsp")
            "The discrete uniform distribution in Quail.")
           ))
         ("Empirical distributions"
          (edit-file "eg:Probability;Distributions;empirical.lsp")
          "Interpreting data as empirical distributions in Quail.")
         ("Finite mixtures"
          (edit-file "eg:Probability;Distributions;finite-mixture.lsp")
          "Finite mixture distributions in Quail.")
         ("Adding new distributions"
          (edit-file "eg:Probability;Distributions;extending.lsp")
          "How to add new distributions to Quail.")
         )
        )
       )
      )
     ("Statistics" ()
      "Statistical modelling and graphics"
      :sub-items
      (("Summary statistics" 
        (edit-file "eg:Statistics;summary-statistics.lsp")
        "Summary statistics."
        )
       ("Response Models" (edit-file "eg:Statistics;Models;overview.lsp")

        "Overview and entry point into example files on statistical 
models in Quail"
        :sub-items
        (("Overview" (edit-file "eg:Statistics;Models;overview.lsp")
          "Overview of response models in Quail.")
         ("-"  ()  "")
         ("binary/logit" (edit-file 
"eg:Statistics;Models;eg-glm-kyphosis.lsp")
          "Logistic regression models.")
         ("poisson/log" (edit-file 
"eg:Statistics;Models;eg-glm-ship-data.lsp")
          "Poisson regression (or log-linear) models.")
         ("gamma/reciprocal" (edit-file 
"eg:Statistics;Models;eg-glm-car.lsp")
          "Gamma regression models.")
         )
        )
       ("Analyses" () ;;(edit-file "eg:Statistics;Analyses;overview.lsp")

        "Overview and entry point into example files on statistical 
analyses in Quail"
        :sub-items
        (("Speed of light meta analysis"
          (edit-file "eg:Statistics;Analyses;meta-analysis.lsp")
          "A graphical meta analysis of the speed of light studies.")
         )
        )
       
       )
      )
     ("Views"
      (edit-file "eg:Views;overview.lsp")
      "Examples illustrating some Views functionality."
      :sub-items
      (("Overview"
        (edit-file "eg:Views;overview.lsp")
        "An overview of the Views philosophy and system.")
       ("-" () "")
       
       ("Plots"
        (edit-file "eg:Views;Plots;general.lsp")
        "An overview of the stock statistical graphics."
        :sub-items
        (("Plots"
          (edit-file "eg:Views;Plots;general.lsp")
          "Introduction to plots in general.")
         ("-" () "")
         ("Scatterplots"
          (edit-file "eg:Views;Plots;scatterplot.lsp")
          "Introduction to scatterplots.")
         ("Surface plots"
          (edit-file "eg:Views;Plots;surface.lsp")
          "Plotting surfaces.")
         ("Grid plots"
          (edit-file "eg:Views;Plots;grid-plot.lsp")
          "Introduction to laying views out in a grid plot.")
         )
        )
        ("Simple-Views"
        ();;(edit-file "eg:Views;Simple-Views;introduction.lsp")
        "An overview of the simple views."
        :sub-items
        (("Bars"
          (edit-file "eg:Views;Simple-Views;bar.lsp")
          "Rectangular bars as in bar plots, histograms, etc.")
         ("Pies"
          (edit-file "eg:Views;Simple-Views;pie.lsp")
          "Pies as in pie charts")
         ("Labels"
          (edit-file "eg:Views;Simple-Views;label.lsp")
          "Text labels.")
         )
        )
        ("Basics"
        (edit-file "eg:Views;Basics;introduction.lsp")
        "An overview of the stock statistical graphics."
        :sub-items
        (("Introduction"
          (edit-file "eg:Views;Basics;introduction.lsp")
          "Introduction to basic design of views.")
         ("-" () "")
         ("Classes"
          (edit-file "eg:Views;Basics;classes.lsp")
          "Classes in the Views system.")
         ("Drawing styles"
          (edit-file "eg:Views;Basics;drawing-styles.lsp")
          "Introduction to drawing-styles of views.")
         ("Mouse interaction"
          (edit-file "eg:Views;Basics;mouse.lsp")
          "How the pointing device can be used to interact with views.")
         ("Moving and Copying"
          (edit-file "eg:Views;Basics;move-copy.lsp")
          "How views may be moved and copied.")
         ("Viewports and Windows"
          (edit-file "eg:Views;Basics;vp-vw.lsp")
          "Discussion of viewports and view-windows.")
         ("Selection of views"
          (edit-file "eg:Views;Basics;selection.lsp")
          "Discussion of selecting views with the mouse.")
         ("Dealing with data"
          (edit-file "eg:Views;Basics;data.lsp")
          "Discussion of the plot-data interface.")
         )
        )
       
       
       
      
       ("Graphical Layout"
        (inform-user "Sorry no overview written yet.")
        ;;(edit-file "eg:Views;overview.lsp")
        "Laying out graphics."
        :sub-items
        (("Grid plot"
          (edit-file "eg:Views;Plots;grid-plot.lsp")
          "Arranging plots in a rectangular grid.")
         
         )
        )
       ("Text"
          (edit-file "eg:Views;Simple-Views;label.lsp")
          "Labels as views of text.")

       ("Advanced"
        (inform-user "Various advanced displays.")
        ;;(edit-file "eg:Views;Stat-graphics;overview.lsp")
        "Various advanced displays."
        :sub-items
        (("Interaction plots"
          (edit-file "eg:Views;Advanced;interaction-plots.lsp")
          "Interacton plots for factorial data.")
         ("A trellis example"
          (edit-file "eg:Views;Advanced;trellis.lsp")
          "Using the views toolkit to build linked trellis displays.")
         ("Suicide data"
          (edit-file "eg:Views;Advanced;suicide.lsp")
          "Linking trellis displays and correspondence analysis plots.")
         ("Categorical data"
          (edit-file "eg:Views;Advanced;categorical.lsp")
          "Barcharts and tables for categorical data.")
         ("Mosaic displays"
          (edit-file "eg:Views;Advanced;mosaic.lsp")
          "Mosaic displays for categorical data.")
         )
        )

       ("Applications"
        (inform-user "Sorry no overview written yet.")
        ;;(edit-file "eg:Views;Stat-graphics;overview.lsp")
        "An overview of the stock statistical graphics."
        :sub-items
        (("Box-Cox plots"
          (edit-file "eg:Views;Applications;boxcox.lsp")
          "Building Box Cox plots that illustrate views functionality.")
         ("EU symbol"
          (edit-file "eg:Views;Applications;euro.lsp")
          "An example using basic methods to draw the European Union 
symbol.")
         ("Smoker analysis"
          (edit-file "eg:Views;Applications;smoke.lsp")
          "An example using basic methods to analyze some data on 
smokers.")
         )
        )
        
       )
      )
     #|
     ("Interface to foreign code" (edit-file "eg:Quaff;overview.lsp")
      "Quail's foreign function interface."
      :sub-items
      (("Overview of Quaff" (edit-file "eg:Quaff;overview.lsp")
        "Quail's foreign function interface.")
       ("-" () "")
       ("Fortran example" (edit-file "eg:Quaff;fortran-example.lsp")
        "Fortran example")
       ("C example" (edit-file "eg:Quaff;c-example.lsp")
        "C example")
       ("Auto-generated access to fortran"
        (edit-file "eg:Quaff;auto-cl-to-f.lsp")
        "Routine for generating lisp code to attach fortran routines.")
       )
      )
     |#
     )
    )
   ("Datasets" 
    (inform-user "Datasets can be found in the directory q:Data;")
    "Datasets can be found in the directory q:Data;"
    :sub-items
    (("A.A. Michelson's 1879 speed of light"
      (edit-file "q:Data;michelson-1879.lsp")
      "A.A. Michelson's 1879 speed of light experiment.")
     ("Apple data"
      (edit-file "q:Data;apple.lsp")
      "Apple data.")
     ("Arms race"
      (edit-file "q:Data;arms.lsp")
      "Arms race.")
     ("Brain and Body average weights"
      (edit-file "q:Data;bbwgt.lsp")
      "Brain and Body average weights")
     ("Cigarette Chemicals"
      (edit-file "q:Data;cigs.lsp")
      "Cigarette Chemicals")
     ("Coal mine data"
      (edit-file "q:Data;coal.lsp")
      "Coal mine data.")
     ("Historical measures of speed of light"
      (edit-file "q:Data;light-speeds.lsp")
      "Historical measures of speed of light.")
     ("Nile river annual flow"
      (edit-file "q:Data;nile-river.lsp")
      "Nile river annual flow.")
     ("Reaction times"
      (edit-file "q:Data;reaction-times.lsp")
      "Reaction time data")
     ("Smoker data"
      (edit-file "q:Data;smoker.lsp")
      "Smoker data")
     ("Solar data"
      (edit-file "q:Data;solar.lsp")
      "Solar data")
     ("Squids eaten by sharks"
      (edit-file "q:Data;squid.lsp")
      "Squids eaten by sharks.")
     ("U.S. annual production"
      (edit-file "q:Data;US-production.lsp")
      "U.S. annual production")
     
     ))
   ("Environment"
    (inform-user "Sub-items on this menu allow global Quail environment ~

                  parameters to be set.")
    "Access to global Quail environment parameters."
    :sub-items
    (("Run Quail toplevel loop?" (quail)
      "Run Quail's toplevel loop in the listener.")
     ("-" () "")
     ("Mouse behaviour" (edit-file 
"eg:Window-Basics;mouse-behaviour.lsp")
      "Information on the role of the mouse keys.")
     ("Help window"
      (inform-user "You must select a sub-item on this menu!")
      "Set background and pen color of the help window."
      :sub-items
      (("Help in windows"
        (setf *help-in-windows* (not *help-in-windows*))
        "Toggles whether help information is to appear in windows."
        :sub-items
        (("Yes ... in windows"
          (setf *help-in-windows* T)
          "Help information is to appear in windows.")
         ("No ... in the listener"
          (setf *help-in-windows* NIL)
          "Help information is to appear in the listener.")
         )
        )
       ("Background color"
        (set-help-background-color
              (wb:prompt-user-for-color))
        "Set the default background colour for all help windows."
        :sub-items
        (("Black"
          (set-help-background-color
              wb:*black-color*)
          "Set the default background colour to be black.")
         ("Gray"
          (set-help-background-color
              wb:*gray-color*)
          "Set the default background colour to be gray.")
         ("White"
          (set-help-background-color
              wb:*white-color*)
          "Set the default background colour to be white.")
         ("-"
          ()
          "")
         ("User defined"
          (set-help-background-color
              (wb:prompt-user-for-color))
          "Set the default background colour to one defined by the 
user.")
         ("-"
          ()
          "")
         ("Black"
          (set-help-background-color
              wb:*black-color*)
          "Set the default background colour to be black.")
         ("Blue"
          (set-help-background-color
              wb:*blue-color*)
          "Set the default background colour to be blue.")
         ("Blue (light)"
          (set-help-background-color
              wb:*light-blue-color*)
          "Set the default background colour to be light blue.")
         ("Brown"
          (set-help-background-color
              wb:*brown-color*)
          "Set the default background colour to be brown.")
         ("Brown (light)"
          (set-help-background-color
              wb:*tan-color*)
          "Set the default background colour to be tan.")
         ("Gray"
          (set-help-background-color
              wb:*gray-color*)
          "Set the default background colour to be gray.")
         ("Gray (dark)"
          (set-help-background-color
              wb:*dark-gray-color*)
          "Set the default background colour to be dark gray.")
         ("Gray (light)"
          (set-help-background-color
              wb:*light-gray-color*)
          "Set the default background colour to be light gray.")
         ("Green"
          (set-help-background-color
              wb:*green-color*)
          "Set the default background colour to be green.")
         ("Green (dark)"
          (set-help-background-color
              wb:*dark-green-color*)
          "Set the default background colour to be dark green.")
         ("Orange"
          (set-help-background-color
              wb:*orange-color*)
          "Set the default background colour to be orange.")
         ("Pink"
          (set-help-background-color
              wb:*pink-color*)
          "Set the default background colour to be pink.")
         ("Purple"
          (set-help-background-color
              wb:*purple-color*)
          "Set the default background colour to be purple.")
         ("Red"
          (set-help-background-color
              wb:*red-color*)
          "Set the default background colour to be red.")
         ("White"
          (set-help-background-color
              wb:*white-color*)
          "Set the default background colour to be white.")
         ("Yellow"
          (set-help-background-color
              wb:*yellow-color*)
          "Set the default background colour to be yellow.")
             )
        )
       ("Pen color"
        (set-help-pen-color (wb:prompt-user-for-color))
        "Set the default pen colour for the Help window."
        :sub-items
        (("Black"
          (set-help-pen-color wb:*black-color*)
          "Set the default pen colour to be black.")
         ("Gray"
          (set-help-pen-color wb:*gray-color*)
          "Set the default pen colour to be gray.")
         ("White"
          (set-help-pen-color wb:*white-color*)
          "Set the default pen colour to be white.")
         ("-"
          ()
          "")
         ("User defined"
          (set-help-pen-color (wb:prompt-user-for-color))
          "Set the default pen colour to one defined by the user.")
         ("-"
          ()
          "")
         ("Black"
          (set-help-pen-color wb:*black-color*)
          "Set the default pen colour to be black.")
         ("Blue"
          (set-help-pen-color wb:*blue-color*)
          "Set the default pen colour to be blue.")
         ("Blue (light)"
          (set-help-pen-color wb:*light-blue-color*)
          "Set the default pen colour to be light blue.")
         ("Brown"
          (set-help-pen-color wb:*brown-color*)
          "Set the default pen colour to be brown.")
         ("Brown (light)"
          (set-help-pen-color wb:*tan-color*)
          "Set the default pen colour to be tan.")
         ("Gray"
          (set-help-pen-color wb:*gray-color*)
          "Set the default pen colour to be gray.")
         ("Gray (dark)"
          (set-help-pen-color wb:*dark-gray-color*)
          "Set the default pen colour to be dark gray.")
         ("Gray (light)"
          (set-help-pen-color wb:*light-gray-color*)
          "Set the default pen colour to be light gray.")
         ("Green"
          (set-help-pen-color wb:*green-color*)
          "Set the default pen colour to be green.")
         ("Green (dark)"
          (set-help-pen-color wb:*dark-green-color*)
          "Set the default pen colour to be dark green.")
         ("Orange"
          (set-help-pen-color wb:*orange-color*)
          "Set the default pen colour to be orange.")
         ("Pink"
          (set-help-pen-color wb:*pink-color*)
          "Set the default pen colour to be pink.")
         ("Purple"
          (set-help-pen-color wb:*purple-color*)
          "Set the default pen colour to be purple.")
         ("Red"
          (set-help-pen-color wb:*red-color*)
          "Set the default pen colour to be red.")
         ("White"
          (set-help-pen-color wb:*white-color*)
          "Set the default pen colour to be white.")
         ("Yellow"
          (set-help-pen-color wb:*yellow-color*)
          "Set the default pen colour to be yellow.")
             )
        )
       )
      )
     ("Information window"
      (inform-user "You must select a sub-item on this menu!")
      "Set background and pen color of the information window."
      :sub-items
      (("Background color"
        (set-info-background-color
              (wb:prompt-user-for-color))
        "Set the default background colour for the information window."
        :sub-items
        (("Black"
          (set-info-background-color
              wb:*black-color*)
          "Set the default background colour to be black.")
         ("Gray"
          (set-info-background-color
              wb:*gray-color*)
          "Set the default background colour to be gray.")
         ("White"
          (set-info-background-color
              wb:*white-color*)
          "Set the default background colour to be white.")
         ("-"
          ()
          "")
         ("User defined"
          (set-info-background-color
              (wb:prompt-user-for-color))
          "Set the default background colour to one defined by the 
user.")
         ("-"
          ()
          "")
         ("Black"
          (set-info-background-color
              wb:*black-color*)
          "Set the default background colour to be black.")
         ("Blue"
          (set-info-background-color
              wb:*blue-color*)
          "Set the default background colour to be blue.")
         ("Blue (light)"
          (set-info-background-color
              wb:*light-blue-color*)
          "Set the default background colour to be light blue.")
         ("Brown"
          (set-info-background-color
              wb:*brown-color*)
          "Set the default background colour to be brown.")
         ("Brown (light)"
          (set-info-background-color
              wb:*tan-color*)
          "Set the default background colour to be tan.")
         ("Gray"
          (set-info-background-color
              wb:*gray-color*)
          "Set the default background colour to be gray.")
         ("Gray (dark)"
          (set-info-background-color
              wb:*dark-gray-color*)
          "Set the default background colour to be dark gray.")
         ("Gray (light)"
          (set-info-background-color
              wb:*light-gray-color*)
          "Set the default background colour to be light gray.")
         ("Green"
          (set-info-background-color
              wb:*green-color*)
          "Set the default background colour to be green.")
         ("Green (dark)"
          (set-info-background-color
              wb:*dark-green-color*)
          "Set the default background colour to be dark green.")
         ("Orange"
          (set-info-background-color
              wb:*orange-color*)
          "Set the default background colour to be orange.")
         ("Pink"
          (set-info-background-color
              wb:*pink-color*)
          "Set the default background colour to be pink.")
         ("Purple"
          (set-info-background-color
              wb:*purple-color*)
          "Set the default background colour to be purple.")
         ("Red"
          (set-info-background-color
              wb:*red-color*)
          "Set the default background colour to be red.")
         ("White"
          (set-info-background-color
              wb:*white-color*)
          "Set the default background colour to be white.")
         ("Yellow"
          (set-info-background-color
              wb:*yellow-color*)
          "Set the default background colour to be yellow.")
             )
        )
       ("Pen color"
        (set-info-pen-color (wb:prompt-user-for-color))
        "Set the default pen colour for the information window."
        :sub-items
        (("Black"
          (set-info-pen-color wb:*black-color*)
          "Set the default pen colour to be black.")
         ("Gray"
          (set-info-pen-color wb:*gray-color*)
          "Set the default pen colour to be gray.")
         ("White"
          (set-info-pen-color wb:*white-color*)
          "Set the default pen colour to be white.")
         ("-"
          ()
          "")
         ("User defined"
          (set-info-pen-color (wb:prompt-user-for-color))
          "Set the default pen colour to one defined by the user.")
         ("-"
          ()
          "")
         ("Black"
          (set-info-pen-color wb:*black-color*)
          "Set the default pen colour to be black.")
         ("Blue"
          (set-info-pen-color wb:*blue-color*)
          "Set the default pen colour to be blue.")
         ("Blue (light)"
          (set-info-pen-color wb:*light-blue-color*)
          "Set the default pen colour to be light blue.")
         ("Brown"
          (set-info-pen-color wb:*brown-color*)
          "Set the default pen colour to be brown.")
         ("Brown (light)"
          (set-info-pen-color wb:*tan-color*)
          "Set the default pen colour to be tan.")
         ("Gray"
          (set-info-pen-color wb:*gray-color*)
          "Set the default pen colour to be gray.")
         ("Gray (dark)"
          (set-info-pen-color wb:*dark-gray-color*)
          "Set the default pen colour to be dark gray.")
         ("Gray (light)"
          (set-info-pen-color wb:*light-gray-color*)
          "Set the default pen colour to be light gray.")
         ("Green"
          (set-info-pen-color wb:*green-color*)
          "Set the default pen colour to be green.")
         ("Green (dark)"
          (set-info-pen-color wb:*dark-green-color*)
          "Set the default pen colour to be dark green.")
         ("Orange"
          (set-info-pen-color wb:*orange-color*)
          "Set the default pen colour to be orange.")
         ("Pink"
          (set-info-pen-color wb:*pink-color*)
          "Set the default pen colour to be pink.")
         ("Purple"
          (set-info-pen-color wb:*purple-color*)
          "Set the default pen colour to be purple.")
         ("Red"
          (set-info-pen-color wb:*red-color*)
          "Set the default pen colour to be red.")
         ("White"
          (set-info-pen-color wb:*white-color*)
          "Set the default pen colour to be white.")
         ("Yellow"
          (set-info-pen-color wb:*yellow-color*)
          "Set the default pen colour to be yellow.")
             )
        )
       )
      )
     ("Canvas parameters"
      (inform-user "You must select a sub-item on this menu!")
      "Set some global parameters for canvases."
      :sub-items
      (("Default background color"
        (setf wb:*default-canvas-background-color*
              (wb:prompt-user-for-color))
        "Set the default background colour for all canvases."
        :sub-items
        (("Black"
          (setf wb:*default-canvas-background-color*
              wb:*black-color*)
          "Set the default background colour to be black.")
         ("Gray"
          (setf wb:*default-canvas-background-color*
              wb:*gray-color*)
          "Set the default background colour to be gray.")
         ("White"
          (setf wb:*default-canvas-background-color*
              wb:*white-color*)
          "Set the default background colour to be white.")
         ("-"
          ()
          "")
         ("User defined"
          (setf wb:*default-canvas-background-color*
              (wb:prompt-user-for-color))
          "Set the default background colour to one defined by the 
user.")
         ("-"
          ()
          "")
         ("Black"
          (setf wb:*default-canvas-background-color*
              wb:*black-color*)
          "Set the default background colour to be black.")
         ("Blue"
          (setf wb:*default-canvas-background-color*
              wb:*blue-color*)
          "Set the default background colour to be blue.")
         ("Blue (light)"
          (setf wb:*default-canvas-background-color*
              wb:*light-blue-color*)
          "Set the default background colour to be light blue.")
         ("Brown"
          (setf wb:*default-canvas-background-color*
              wb:*brown-color*)
          "Set the default background colour to be brown.")
         ("Brown (light)"
          (setf wb:*default-canvas-background-color*
              wb:*tan-color*)
          "Set the default background colour to be tan.")
         ("Gray"
          (setf wb:*default-canvas-background-color*
              wb:*gray-color*)
          "Set the default background colour to be gray.")
         ("Gray (dark)"
          (setf wb:*default-canvas-background-color*
              wb:*dark-gray-color*)
          "Set the default background colour to be dark gray.")
         ("Gray (light)"
          (setf wb:*default-canvas-background-color*
              wb:*light-gray-color*)
          "Set the default background colour to be light gray.")
         ("Green"
          (setf wb:*default-canvas-background-color*
              wb:*green-color*)
          "Set the default background colour to be green.")
         ("Green (dark)"
          (setf wb:*default-canvas-background-color*
              wb:*dark-green-color*)
          "Set the default background colour to be dark green.")
         ("Orange"
          (setf wb:*default-canvas-background-color*
              wb:*orange-color*)
          "Set the default background colour to be orange.")
         ("Pink"
          (setf wb:*default-canvas-background-color*
              wb:*pink-color*)
          "Set the default background colour to be pink.")
         ("Purple"
          (setf wb:*default-canvas-background-color*
              wb:*purple-color*)
          "Set the default background colour to be purple.")
         ("Red"
          (setf wb:*default-canvas-background-color*
              wb:*red-color*)
          "Set the default background colour to be red.")
         ("White"
          (setf wb:*default-canvas-background-color*
              wb:*white-color*)
          "Set the default background colour to be white.")
         ("Yellow"
          (setf wb:*default-canvas-background-color*
              wb:*yellow-color*)
          "Set the default background colour to be yellow.")
             )
        )
       ("Default pen color"
        (setf wb:*default-canvas-pen-color*
              (wb:prompt-user-for-color))
        "Set the default pen colour for all canvases."
        :sub-items
        (("Black"
          (setf wb:*default-canvas-pen-color*
              wb:*black-color*)
          "Set the default pen colour to be black.")
         ("Gray"
          (setf wb:*default-canvas-pen-color*
              wb:*gray-color*)
          "Set the default pen colour to be gray.")
         ("White"
          (setf wb:*default-canvas-pen-color*
              wb:*white-color*)
          "Set the default pen colour to be white.")
         ("-"
          ()
          "")
         ("User defined"
          (setf wb:*default-canvas-pen-color*
              (wb:prompt-user-for-color))
          "Set the default pen colour to one defined by the user.")
         ("-"
          ()
          "")
         ("Black"
          (setf wb:*default-canvas-pen-color*
              wb:*black-color*)
          "Set the default pen colour to be black.")
         ("Blue"
          (setf wb:*default-canvas-pen-color*
              wb:*blue-color*)
          "Set the default pen colour to be blue.")
         ("Blue (light)"
          (setf wb:*default-canvas-pen-color*
              wb:*light-blue-color*)
          "Set the default pen colour to be light blue.")
         ("Brown"
          (setf wb:*default-canvas-pen-color*
              wb:*brown-color*)
          "Set the default pen colour to be brown.")
         ("Brown (light)"
          (setf wb:*default-canvas-pen-color*
              wb:*tan-color*)
          "Set the default pen colour to be tan.")
         ("Gray"
          (setf wb:*default-canvas-pen-color*
              wb:*gray-color*)
          "Set the default pen colour to be gray.")
         ("Gray (dark)"
          (setf wb:*default-canvas-pen-color*
              wb:*dark-gray-color*)
          "Set the default pen colour to be dark gray.")
         ("Gray (light)"
          (setf wb:*default-canvas-pen-color*
              wb:*light-gray-color*)
          "Set the default pen colour to be light gray.")
         ("Green"
          (setf wb:*default-canvas-pen-color*
              wb:*green-color*)
          "Set the default pen colour to be green.")
         ("Green (dark)"
          (setf wb:*default-canvas-pen-color*
              wb:*dark-green-color*)
          "Set the default pen colour to be dark green.")
         ("Orange"
          (setf wb:*default-canvas-pen-color*
              wb:*orange-color*)
          "Set the default pen colour to be orange.")
         ("Pink"
          (setf wb:*default-canvas-pen-color*
              wb:*pink-color*)
          "Set the default pen colour to be pink.")
         ("Purple"
          (setf wb:*default-canvas-pen-color*
              wb:*purple-color*)
          "Set the default pen colour to be purple.")
         ("Red"
          (setf wb:*default-canvas-pen-color*
              wb:*red-color*)
          "Set the default pen colour to be red.")
         ("White"
          (setf wb:*default-canvas-pen-color*
              wb:*white-color*)
          "Set the default pen colour to be white.")
         ("Yellow"
          (setf wb:*default-canvas-pen-color*
              wb:*yellow-color*)
          "Set the default pen colour to be yellow.")
             )
        )
       ("Device type" (wb::set-device-type
                       (wb::prompt-user
                        :prompt-string
                        "Enter one of :color, :gray-scale, or 
:black&white."
                        :type 'symbol
                        :read-type :eval))
        "Set the type of the current device."
        :sub-items
        (("Color" (wb::set-device-type :color)
          "Set the type of the current device to :color.")
         ("Gray scale" (wb::set-device-type :gray-scale)
          "Set the type of the current device to :gray-scale.")
         ("Black and White" (wb::set-device-type :black&white)
          "Set the type of the current device to :black&white.")
         ("Other" (wb::set-device-type
                   (wb::prompt-user
                    :prompt-string
                    (format NIL
                            "Enter one of ~s."
                            wb::*device-types*)
                    :type 'symbol
                    :read-type :eval))
          "Set the type of the current device to :black&white.")))
       ("Default canvas position"
        (quail-print "You must select a sub-item on this menu!")
        "Set the default canvas position."
        :sub-items
        (("Bottom left corner"
          (wb::set-up-default-canvas-region  10 10 400 300)
          "All new canvases will appear for the first time in the bottom 
left corner ~
           of the display.")
         ("Top left corner"
          (wb::set-up-default-canvas-region
           10
           (- (wb::screen-height) 300)
           400 300)
          "All new canvases will appear for the first time in the top 
left corner ~
           of the display.")
         ("Top right corner"
          (wb::set-up-default-canvas-region
           (- (wb::screen-width) 400)
           (- (wb::screen-height) 300)
           400 300)
          "All new canvases will appear for the first time in the top 
right corner ~
           of the display.")
         ("Bottom right corner"
          (wb::set-up-default-canvas-region
           (- (wb::screen-width) 400)
           10
           400 300)
          "All new canvases will appear for the first time in the bottom 
right corner ~
           of the display.")
         ("Center of display"
          (wb::set-up-default-canvas-region
           (- (round (/ (wb::screen-width) 2)) 200)
           (- (round (/ (wb::screen-height) 2)) 150)
           400 300)
          "All new canvases will appear for the first time in the bottom 
right corner ~
           of the display.")
         ("Choose at creation"
          (setf wb::*default-canvas-region* NIL)
          "All new canvases will appear for the first time in the bottom 
right corner ~
           of the display.")
         ))
       ))
     ("-" () "")
     ("Default colors for Views"
      (inform-user "You must select a sub-item on this menu!")
      "Set background and pen color of the help window."
      :sub-items
      (
       ("Highlighting"
        (setf *default-highlight-color* (wb:prompt-user-for-color))
        "Set the default highlight colour for all views."
        :sub-items
        (("Black"
          (setf *default-highlight-color* wb:*black-color*)
          "Set the default highlight colour to be black.")
         ("Gray"
          (setf *default-highlight-color* wb:*gray-color*)
          "Set the default highlight colour to be gray.")
         ("White"
          (setf *default-highlight-color* wb:*white-color*)
          "Set the default highlight colour to be white.")
         ("-"
          ()
          "")
         ("User defined"
          (setf *default-highlight-color* (wb:prompt-user-for-color))
          "Set the default highlight colour to one defined by the user.")

         ("-"
          ()
          "")
         ("Black"
          (setf *default-highlight-color* wb:*black-color*)
          "Set the default highlight colour to be black.")
         ("Blue"
          (setf *default-highlight-color* wb:*blue-color*)
          "Set the default highlight colour to be blue.")
         ("Blue (light)"
          (setf *default-highlight-color* wb:*light-blue-color*)
          "Set the default highlight colour to be light blue.")
         ("Brown"
          (setf *default-highlight-color* wb:*brown-color*)
          "Set the default highlight colour to be brown.")
         ("Brown (light)"
          (setf *default-highlight-color* wb:*tan-color*)
          "Set the default highlight colour to be tan.")
         ("Gray"
          (setf *default-highlight-color* wb:*gray-color*)
          "Set the default highlight colour to be gray.")
         ("Gray (dark)"
          (setf *default-highlight-color* wb:*dark-gray-color*)
          "Set the default highlight colour to be dark gray.")
         ("Gray (light)"
          (setf *default-highlight-color* wb:*light-gray-color*)
          "Set the default highlight colour to be light gray.")
         ("Green"
          (setf *default-highlight-color* wb:*green-color*)
          "Set the default highlight colour to be green.")
         ("Green (dark)"
          (setf *default-highlight-color* wb:*dark-green-color*)
          "Set the default highlight colour to be dark green.")
         ("Orange"
          (setf *default-highlight-color* wb:*orange-color*)
          "Set the default highlight colour to be orange.")
         ("Pink"
          (setf *default-highlight-color* wb:*pink-color*)
          "Set the default highlight colour to be pink.")
         ("Purple"
          (setf *default-highlight-color* wb:*purple-color*)
          "Set the default highlight colour to be purple.")
         ("Red"
          (setf *default-highlight-color* wb:*red-color*)
          "Set the default highlight colour to be red.")
         ("White"
          (setf *default-highlight-color* wb:*white-color*)
          "Set the default highlight colour to be white.")
         ("Yellow"
          (setf *default-highlight-color* wb:*yellow-color*)
          "Set the default highlight colour to be yellow.")
             )
        )
       ("Point symbols"
        (setf *default-point-color* (wb:prompt-user-for-color))
        "Set the default point colour for all views."
        :sub-items
        (("Black"
            (setf *default-point-color* wb:*black-color*)
            "Set the default point colour to be black.")
           ("Gray"
            (setf *default-point-color* wb:*gray-color*)
            "Set the default point colour to be gray.")
           ("White"
            (setf *default-point-color* wb:*white-color*)
            "Set the default point colour to be white.")
           ("-"
            ()
            "")
           ("User defined"
            (setf *default-point-color* (wb:prompt-user-for-color))
            "Set the default point colour to one defined by the user.")
           ("-"
            ()
            "")
           ("Black"
            (setf *default-point-color* wb:*black-color*)
            "Set the default point colour to be black.")
           ("Blue"
            (setf *default-point-color* wb:*blue-color*)
            "Set the default point colour to be blue.")
           ("Blue (light)"
            (setf *default-point-color* wb:*light-blue-color*)
            "Set the default point colour to be light blue.")
           ("Brown"
            (setf *default-point-color* wb:*brown-color*)
            "Set the default point colour to be brown.")
           ("Brown (light)"
            (setf *default-point-color* wb:*tan-color*)
            "Set the default point colour to be tan.")
           ("Gray"
            (setf *default-point-color* wb:*gray-color*)
            "Set the default point colour to be gray.")
           ("Gray (dark)"
            (setf *default-point-color* wb:*dark-gray-color*)
            "Set the default point colour to be dark gray.")
           ("Gray (light)"
            (setf *default-point-color* wb:*light-gray-color*)
            "Set the default point colour to be light gray.")
           ("Green"
            (setf *default-point-color* wb:*green-color*)
            "Set the default point colour to be green.")
           ("Green (dark)"
            (setf *default-point-color* wb:*dark-green-color*)
            "Set the default point colour to be dark green.")
           ("Orange"
            (setf *default-point-color* wb:*orange-color*)
            "Set the default point colour to be orange.")
           ("Pink"
            (setf *default-point-color* wb:*pink-color*)
            "Set the default point colour to be pink.")
           ("Purple"
            (setf *default-point-color* wb:*purple-color*)
            "Set the default point colour to be purple.")
           ("Red"
            (setf *default-point-color* wb:*red-color*)
            "Set the default point colour to be red.")
           ("White"
            (setf *default-point-color* wb:*white-color*)
            "Set the default point colour to be white.")
           ("Yellow"
            (setf *default-point-color* wb:*yellow-color*)
            "Set the default point colour to be yellow.")
           )
       )
       ("Curves"
          (setf *default-curve-color* (wb:prompt-user-for-color))
          "Set the default curve colour for all views."
          :sub-items
          (("Black"
            (setf *default-curve-color* wb:*black-color*)
            "Set the default curve colour to be black.")
           ("Gray"
            (setf *default-curve-color* wb:*gray-color*)
            "Set the default curve colour to be gray.")
           ("White"
            (setf *default-curve-color* wb:*white-color*)
            "Set the default curve colour to be white.")
           ("-"
            ()
            "")
           ("User defined"
            (setf *default-curve-color* (wb:prompt-user-for-color))
            "Set the default curve colour to one defined by the user.")
           ("-"
            ()
            "")
           ("Black"
            (setf *default-curve-color* wb:*black-color*)
            "Set the default curve colour to be black.")
           ("Blue"
            (setf *default-curve-color* wb:*blue-color*)
            "Set the default curve colour to be blue.")
           ("Blue (light)"
            (setf *default-curve-color* wb:*light-blue-color*)
            "Set the default curve colour to be light blue.")
           ("Brown"
            (setf *default-curve-color* wb:*brown-color*)
            "Set the default curve colour to be brown.")
           ("Brown (light)"
            (setf *default-curve-color* wb:*tan-color*)
            "Set the default curve colour to be tan.")
           ("Gray"
            (setf *default-curve-color* wb:*gray-color*)
            "Set the default curve colour to be gray.")
           ("Gray (dark)"
            (setf *default-curve-color* wb:*dark-gray-color*)
            "Set the default curve colour to be dark gray.")
           ("Gray (light)"
            (setf *default-curve-color* wb:*light-gray-color*)
            "Set the default curve colour to be light gray.")
           ("Green"
            (setf *default-curve-color* wb:*green-color*)
            "Set the default curve colour to be green.")
           ("Green (dark)"
            (setf *default-curve-color* wb:*dark-green-color*)
            "Set the default curve colour to be dark green.")
           ("Orange"
            (setf *default-curve-color* wb:*orange-color*)
            "Set the default curve colour to be orange.")
           ("Pink"
            (setf *default-curve-color* wb:*pink-color*)
            "Set the default curve colour to be pink.")
           ("Purple"
            (setf *default-curve-color* wb:*purple-color*)
            "Set the default curve colour to be purple.")
           ("Red"
            (setf *default-curve-color* wb:*red-color*)
            "Set the default curve colour to be red.")
           ("White"
            (setf *default-curve-color* wb:*white-color*)
            "Set the default curve colour to be white.")
           ("Yellow"
            (setf *default-curve-color* wb:*yellow-color*)
            "Set the default curve colour to be yellow.")
           )
          )
       ("Labels"
          (setf *default-label-color* (wb:prompt-user-for-color))
          "Set the default label colour for all views."
          :sub-items
          (("Black"
            (setf *default-label-color* wb:*black-color*)
            "Set the default label colour to be black.")
           ("Gray"
            (setf *default-label-color* wb:*gray-color*)
            "Set the default label colour to be gray.")
           ("White"
            (setf *default-label-color* wb:*white-color*)
            "Set the default label colour to be white.")
           ("-"
            ()
            "")
           ("User defined"
            (setf *default-label-color* (wb:prompt-user-for-color))
            "Set the default label colour to one defined by the user.")
           ("-"
            ()
            "")
           ("Black"
            (setf *default-label-color* wb:*black-color*)
            "Set the default label colour to be black.")
           ("Blue"
            (setf *default-label-color* wb:*blue-color*)
            "Set the default label colour to be blue.")
           ("Blue (light)"
            (setf *default-label-color* wb:*light-blue-color*)
            "Set the default label colour to be light blue.")
           ("Brown"
            (setf *default-label-color* wb:*brown-color*)
            "Set the default label colour to be brown.")
           ("Brown (light)"
            (setf *default-label-color* wb:*tan-color*)
            "Set the default label colour to be tan.")
           ("Gray"
            (setf *default-label-color* wb:*gray-color*)
            "Set the default label colour to be gray.")
           ("Gray (dark)"
            (setf *default-label-color* wb:*dark-gray-color*)
            "Set the default label colour to be dark gray.")
           ("Gray (light)"
            (setf *default-label-color* wb:*light-gray-color*)
            "Set the default label colour to be light gray.")
           ("Green"
            (setf *default-label-color* wb:*green-color*)
            "Set the default label colour to be green.")
           ("Green (dark)"
            (setf *default-label-color* wb:*dark-green-color*)
            "Set the default label colour to be dark green.")
           ("Orange"
            (setf *default-label-color* wb:*orange-color*)
            "Set the default label colour to be orange.")
           ("Pink"
            (setf *default-label-color* wb:*pink-color*)
            "Set the default label colour to be pink.")
           ("Purple"
            (setf *default-label-color* wb:*purple-color*)
            "Set the default label colour to be purple.")
           ("Red"
            (setf *default-label-color* wb:*red-color*)
            "Set the default label colour to be red.")
           ("White"
            (setf *default-label-color* wb:*white-color*)
            "Set the default label colour to be white.")
           ("Yellow"
            (setf *default-label-color* wb:*yellow-color*)
            "Set the default label colour to be yellow.")
           )
          )
      )
     
     )
    )
   )
 )
 )
