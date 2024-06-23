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

(asdf:defsystem "views"
    :default-component-class cl-source-file.lsp
  :components
  ((:file "views/views-package")
   (:module "views/views-macros"
            :components
            ((:file "region")
             (:file "draw-macros")
             (:file "linkbounds-macros")))
   
   (:module "views/utilities"
            :components
            (
             ;;(:file "affinetrans")
             (:file "fasttrans")
             (:file "title-menu")
             (:file "class-info")
             (:file "function-info")
             (:file "stats")
             )
            :depends-on ("views/views-macros"))
   
   (:module "views/views-data"
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

   (:module "views/views-general"
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
            :depends-on ("views/views-macros"))
 #|  
   
   (:module "views/views-mixins"
            :components
            ( (:file "brush" )
             (:file "angled-brush" )
              (:file "lines-mixin")
             (:file "mixin" ) 
             (:file "link-table" )
             (:file "link" )
              (:file "tic-mixin")
             (:file "bordered-view-mixin"))
            :depends-on ("views/views-macros"))
   
   (:module "views/controls"
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
            :depends-on ("views/views-macros"))
   
   (:module "views/simple-views"
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
            :depends-on ("views/views-macros"))
   
   (:module "views/dview-def"
            :components
            ((:file "d-view-mixins"  )
             (:file "d-view-defs"  )
             (:file "d-view" )
             (:file "d-view-menus" )
             (:file "change-var" )
             (:file "change-cases" )
             (:file "text-link" ))
            :depends-on ("views/views-macros"))
   
   (:module "views/d-views"
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
            :depends-on ("views/views-macros"))
   
   (:module "views/layout"
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
            :depends-on ("views/views-macros"))
   
   (:module "views/plots"
            :components
            ((:file "plot-mixins")
             (:file "plot" )
             (:file "plot-d" )
             (:file "grid-plot")
             (:file "rotating-plot")
             (:file "bar-plot")
             (:file "overlay-plots" )
             (:file "make-plots" ))
            :depends-on ("views/views-macros"))
   
   (:module "views/scroll"
            :components
            ((:file "arrow")
             (:file "scroll-bar")
             (:file "scrollable-view-mixin" )
             (:file "display-list")
             (:file "scrolling-display"))
            :depends-on ("views/views-macros"))
   
   (:module "views/prompt-plot"
            :components
            ((:file "prompt-selections")
             (:file "single-plot")
             (:file "group-plot")
             ;; (:file "prompt-projection-trace" )
             (:file "prompt-plot")
             (:file "prompt-plot-menu")))
   (:module "views/clone"
            :components
            ((:file "clone")
             (:file "copy")
             (:file "combine-args")))
   (:module "views/display"
            :components
            ((:file "display-methods")
             (:file "display-data")
             (:file "signposts-methods")
             ))
   (:module "views/other"
            :components
            ((:file "connected-points")
             ))
|#             
   )
)
