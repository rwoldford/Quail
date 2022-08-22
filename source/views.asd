;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;;                               views.asd
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;;
;;; Copyright (c) 1991 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     C. Hurley
;;;     R.W. Oldford 1991.
;;;
;;;
;;;--------------------------------------------------------------------------------

PLACEHOLDER ONLY

(defsystem views
  :source-pathname (identity (append-directories
                              (path-source)
                              (path-views)))
  :binary-pathname (identity (append-directories
                              (path-binary)
                              (bin (path-views-binary))))
  :components
  ((:file "views-package")
   
   
   (:module views-macros
            :source-pathname (path-views-macros)
            :binary-pathname (bin (path-views-macros))
            :components
            ((:file "region")
             (:file "draw-macros")
             (:file "linkbounds-macros")))
   
   (:module utilities
            :source-pathname (path-views-utilities)
            :binary-pathname (bin (path-views-utilities))
            :components
            (
             ;;(:file "affinetrans")
             (:file "fasttrans")
             (:file "title-menu")
             (:file "class-info")
             (:file "function-info")
             (:file "stats")
             )
            :depends-on ("views-macros"))
   
   (:module views-data
            :source-pathname (path-views-data)
            :binary-pathname (bin (path-views-data))
            :components
            ((:file "data")
             (:file "simple-case-object")
             (:file "dataset")
             (:file "mway-dataset") 
             (:file "data-lists")
             (:file "data-ref-arrays")
             (:file "dataset-class")
             (:file "mlevel-dataset")
             (:file "data-subsets")
             (:file "prompt-data")
             ))
   
   (:module views-general
            :source-pathname (path-views-general)
            :binary-pathname (bin (path-views-general))
            :components
            ((:file "special-vars")
             (:file "draw-styles")
             (:file "draw-style-mixins")
             (:file "button-mixin" )
             (:file "linkbounds")
             (:file "view-generics")
             (:file "view-def" )
             (:file "view-ops" )
             (:file "layer" )
             (:file "view-window")
             (:file "abstract-views" )
             (:file "make-views" )
             (:file "display"))
            :depends-on ("views-macros"))
   
   
   (:module views-mixins
            :source-pathname (path-views-mixins)
            :binary-pathname (bin (path-views-mixins))
            :components
            ( (:file "brush" )
             (:file "angled-brush" )
              (:file "lines-mixin")
             (:file "mixin" ) 
             (:file "link-table" )
             (:file "link" )
              (:file "tic-mixin")
             (:file "bordered-view-mixin"))
            :depends-on ("views-macros"))
   
   (:module controls
            :source-pathname (path-views-controls)
            :binary-pathname (bin (path-views-controls))
            :components
            ((:file "button-control")
             (:file "control-button")
	     (:file "elliptical-button")
	     (:file "rounded-button")
	     (:file "signpost-button")
	     (:file "prompt-widget")
             (:file "slider")
             (:file "double-slider")
             (:file "range-slider"))
            :depends-on ("views-macros"))
   
   (:module simple-views
            :source-pathname (path-views-simple-views)
            :binary-pathname (bin (path-views-simple-views))
            :components
            ((:file "label")
             (:file "point-symbol")
             (:file "bar"  )
             (:file "pie"  )
             (:file "line-segment")
             (:file "function-view")
             (:file "line" )
             (:file "text-view" )
             (:file "numerical-label")
             (:file "key-input")
             (:file "editable-text")
             )
            :depends-on ("views-macros"))
   
   (:module dview-def
            :source-pathname (path-views-dview-def)
            :binary-pathname (bin (path-views-dview-def))
            :components
            ((:file "d-view-mixins"  )
             (:file "d-view-defs"  )
             (:file "d-view" )
             (:file "d-view-menus" )
             (:file "change-var" )
             (:file "change-cases" )
             (:file "text-link" ))
            :depends-on ("views-macros"))
   
   (:module d-views
            :source-pathname (path-views-d-views)
            :binary-pathname (bin (path-views-d-views))
            :components
            ((:file "one-per-case")
             (:file "point-cloud")
             (:file "fitted-line")
             (:file "simple-lines")
             (:file "lines" )
             (:file "histogram")
             (:file "boxplot")
             (:file "axis")
             (:file "grid-view")
             (:file "smooth")
             ;;(:file "barchart")
             ;;(:file "fringe-view")
             (:file "interval-view")
             (:file "line-segments-per-case")
             (:file "moving-cloud-mixin" )
             (:file "rotation")
             (:file "rotating-lines")
             )
            :depends-on ("views-macros"))
   
   (:module layout
            :source-pathname (path-views-layout)
            :binary-pathname (bin (path-views-layout))
            :components
            ((:file "view-layers")
             (:file "view-layout")
             (:file "grid-layout")
             (:file "batch-layout")
             (:file "1d-layout")
             (:file "pairs-layout")
             (:file "xy-layout")
             (:file "case-layout")
             (:file "barchart")
             (:file "table")
             )
            :depends-on ("views-macros"))
   
   (:module plots
            :source-pathname (path-views-plots)
            :binary-pathname (bin (path-views-plots))
            :components
            ((:file "plot-mixins")
             (:file "plot" )
             (:file "plot-d" )
             (:file "grid-plot")
             (:file "rotating-plot")
             (:file "bar-plot")
             (:file "overlay-plots" )
             (:file "make-plots" ))
            :depends-on ("views-macros"))
   
   (:module scroll
            :source-pathname (path-views-scroll)
            :binary-pathname (bin (path-views-scroll))
            :components
            ((:file "arrow")
             (:file "scroll-bar")
             (:file "scrollable-view-mixin" )
             (:file "display-list")
             (:file "scrolling-display"))
            :depends-on ("views-macros"))
   
   (:module prompt-plot
            :source-pathname (path-views-prompt-plot)
            :binary-pathname (bin (path-views-prompt-plot))
            :components
            ((:file "prompt-selections")
             (:file "single-plot")
             (:file "group-plot")
             ;; (:file "prompt-projection-trace" )
             (:file "prompt-plot")
             (:file "prompt-plot-menu")))
   (:module clone
            :source-pathname (path-views-clone)
            :binary-pathname (bin (path-views-clone))
            :components
            ((:file "clone")
             (:file "copy")
             (:file "combine-args")))
   (:module display
            :source-pathname (path-views-display)
            :binary-pathname (bin (path-views-display))
            :components
            ((:file "display-methods")
             (:file "display-data")
             (:file "signposts-methods")
             ))
   (:module other
            :source-pathname (path-views-other)
            :binary-pathname (bin (path-views-other))
            :components
            ((:file "connected-points")
             ))
   ))