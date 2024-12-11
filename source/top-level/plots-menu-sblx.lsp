;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;
;;;
;;;                          quail-plot-menu-sblx.lsp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;
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
;;; quail-plot-menu-sblx.lsp  makes only the Plots menu through the p-command-table
;;; quail-canvas-menu-sblx.lsp  makes on the Canvas menu through the c-command-table
;;; quail-plot-menu-sblx.lsp  makes the table for Quail and Plots menus
;;; quail-plots-canvas-menus-sbls.lsp  makes the table for Quail, Plots, and Canvas
;;; change-menu-bar-sblx.lsp  contains code for replacing a command-table
;;; Here is mcclim-like code to make the (mcclim)command-tables for the Quail menus

(in-package :wb);(in-package :quail)
(eval-when (:compile-toplevel :load-toplevel :execute) (export '(p-command-table)))


;;; Time to make the Plots menu
;;; from ~/views/prompt-plot/prompt-plot/prompt-plot-menu.lsp

   ;;; START P/D SB Data Sub-menu
;;; the commands
(define-qmbar-command com-cases-PDSB ()
	(prompt-case-display))

(define-qmbar-command com-variates-PDSB ()
	(prompt-variable-display))

(define-qmbar-command com-bygroup-PDSB ()
	(prompt-case-display-by))

;;; make the command table
(make-command-table 'pdsb-command-table :errorp nil
	:menu '(("Cases" :command com-cases-PDSB)
		("Variates" :command com-variates-PDSB)
		("By Group" :command com-bygroup-PDSB)))   
   ;;; END P/D SB sub-menu

   ;;; START P/SV Single variate Sub-menu
;;; the commands
(define-qmbar-command com-hist-PSVSB ()
	(vw::prompt-plot :dim 1 :plot-fn #'histogram))

(define-qmbar-command com-barcht-PSVSB ()
	(vw::prompt-plot :dim 1 :plot-fn #'bar-plot))

(define-qmbar-command com-boxplot-PSVSB ()
	(vw::prompt-plot :dim 1 :plot-fn #'boxplot))

(define-qmbar-command com-dotplot-PSVSB ()
	(vw::prompt-plot :dim 1 :plot-fn #'dot-plot))

(define-qmbar-command com-plot-PSVSB ()
	(vw::prompt-plot :dim 1))

(define-qmbar-command com-histby-PSVSB ()
	(vw::prompt-plot-by :dim 1 :plot-fn 'histogram-view))

(define-qmbar-command com-barby-PSVSB ()
	(vw::prompt-plot-by :dim 1 :plot-fn 'bar-chart))

(define-qmbar-command com-boxby-PSVSB ()
	(vw::prompt-plot-by :dim 1 :plot-fn 'boxplot-view))

(define-qmbar-command com-dotby-PSVSB ()
	(vw::prompt-plot-by :dim 1 :plot-fn '1d-point-cloud))

(define-qmbar-command com-plotby-PSVSB ()
	(vw::prompt-plot-by :dim 1))

;;; make the command table
(make-command-table 'psvsb-command-table :errorp nil
	:menu '(("Histogram" :command com-hist-PSVSB)
		("Barchart" :command com-barcht-PSVSB)
		("Boxplot":command com-boxplot-PSVSB)
		("Dot plot" :command com-dotplot-PSVSB)
		("Plot" :command com-plot-PSVSB)
		("" :command com-divider)
		("Histogram by" :command com-histby-PSVSB)
		("Barchart by" :command com-barby-PSVSB)
		("Boxplot by" :command com-boxby-PSVSB)
		("Dot plot by" :command com-dotby-PSVSB)
		("Plot by" :command com-plotby-PSVSB)))										
   ;;; END P/SV SB sub-menu

   ;;; START P/TV SB Two variates Sub-menu
;;; the commands
(define-qmbar-command com-scatter-PTVSB ()
	(vw::prompt-plot :dim 2 :plot-fn #'scatterplot))

(define-qmbar-command com-linespl-PTVSB ()
	(vw::prompt-plot :dim 2 :plot-fn #'lines-plot))

(define-qmbar-command com-linseg-PTVSB ()
	(vw::prompt-plot :dim 2 :plot-fn #'line-segment-2d-plot))

(define-qmbar-command com-plot-PTVSB ()
	(vw::prompt-plot :dim 2))

(define-qmbar-command com-scatterby-PTVSB ()
	(vw::prompt-plot-by :dim 2 :plot-fn #'2d-point-cloud))

(define-qmbar-command com-linesby-PTVSB ()
	(vw::prompt-plot-by :dim 2 :plot-fn #'lines))

(define-qmbar-command com-plotby-PTVSB ()
	(vw::prompt-plot-by :dim 2))
;;; make the command table
(make-command-table 'ptvsb-command-table :errorp nil
	:menu '(("Scatterplot" :command com-scatter-PTVSB)
		("Lines plot" :command com-linesby-PTVSB)
		("Line Segments" :command com-linseg-PTVSB)
		("Plot" :command com-plot-PTVSB)
		("" :command com-divider)
		("Scatterplot by" :command com-scatterby-PTVSB)
		("Lines plot by" :command com-linesby-PTVSB)
		("Plot by" :command com-plotby-PTVSB)))							
   ;;; END P/TVSB submenu

   ;;; START P/MV SB Multiple variates Sub-menu
;;; the commands
(define-qmbar-command com-3dscatter-PMVSB ()
	(vw::prompt-plot :dim 3 :plot-fn "Rotating Plot"))

(define-qmbar-command com-3dlineseg-PMVSB ()
	(vw::prompt-plot :dim 3 :plot-fn "Rotating Lines"))

(define-qmbar-command com-scatmat-PMVSB ()
	(vw::prompt-plot :dim 3 :plot-fn "Scatterplot Matrix"))

(define-qmbar-command com-andtrace-PMVSB ()
	(vw::prompt-plot :dim 3 :plot-fn "Andrews' Trace"))

(define-qmbar-command com-tuktrace-PMVSB ()
	(vw::prompt-plot :dim 3 :plot-fn "Tukey's Trace"))

(define-qmbar-command com-plotmat-PMVSB ()
	(vw::prompt-plot :dim 3 :plot-fn "Plot Matrix"))

(define-qmbar-command com-sideside-PMVSB ()
	(vw::prompt-plot :dim 3 :plot-fn "Side by side"))

(define-qmbar-command com-crosspl-PMVSB ()
	(vw::prompt-plot :dim '(nil nil)))

(define-qmbar-command com-scamatby-PMVSB ()
	(vw::prompt-plot-by :dim 3 :plot-fn 'pairs-layout))

(define-qmbar-command com-3dscatsby-PMVSB ()
	(vw::prompt-plot-by :dim 3 :plot-fn 'rotating-plot))

;;; make the command table
(make-command-table 'pmvsb-command-table :errorp nil
    :menu '(("3D Scatterplot" :command com-3dscatter-PMVSB)
    	("3D Line Segments" :command com-3dlineseg-PMVSB)
    	("Scatterplot matrix" :command com-scatmat-PMVSB)
    	("Andrews' Trace" :command com-andtrace-PMVSB)
    	("Tukey's Trace" :command com-tuktrace-PMVSB)
    	("" :command com-divider)
    	("Plot matrix" :command com-plotmat-PMVSB)
    	("Side by side" :command com-sideside-PMVSB)
    	("Cross plot" :command com-crosspl-PMVSB)
    	("" :command com-divider)
    	("Scatterplot matrix by" :command com-scamatby-PMVSB)
    	("3D Scatterplots by" :command com-3dscatsby-PMVSB)))
   ;;; END P/MVSB sub-menu

   ;;; START P/F Function sub-menu
;;; the commands
(define-qmbar-command com-svf-PFSB ()
	(function-plot))

(define-qmbar-command com-tvf-PFSB ()
	(surface-plot))
;;; make the command table
(make-command-table 'pfsb-command-table :errorp nil
	:menu '(("Single variable function" :command com-scatmat-PMVSB)
		("Two variable function" :command com-tvf-PFSB)))		
   ;;; END P/FSB sub-menu

   ;;; START P/L SB Linking sub-menu
;;; the commands
(define-qmbar-command com-lnkvws-PLSB ()
	(link-selected-views))

(define-qmbar-command com-unlnkvws-PLSB ()
	(unlink-top-views))

(define-qmbar-command com-newlnktab-PLSB ()
	(plot-menu-items 1))

(define-qmbar-command com-dellnktabs-PLSB ()
	(delete-link-tables))
;;; make the command table
(make-command-table 'plsb-command-table :errorp nil
	:menu '(("Link views" :command com-lnkvws-PLSB)
		("Unlink views" :command com-unlnkvws-PLSB)
		("New link table" :command com-newlnktab-PLSB)
		("Delete all link tables" :command com-dellnktabs-PLSB)))				
   ;;; END P/LSB sub-menu

   ;;; START P/VS SB View  sets sub-menu command table
;;; the commands
(define-qmbar-command com-name-PVSSB ()
	 (VIEW-SET-MENU-ITEMS 0))

(define-qmbar-command com-show-PVSSB ()
	(VIEW-SET-MENU-ITEMS 1))

(define-qmbar-command com-union-PVSSB ()
	(VIEW-SET-MENU-ITEMS 2))

(define-qmbar-command com-intersect-PVSSB ()
	(VIEW-SET-MENU-ITEMS 3))

(define-qmbar-command com-diff-PVSSB ()
	 (VIEW-SET-MENU-ITEMS 4))
;;; make the command table
(make-command-table 'pvssb-command-table :errorp nil
	:menu '(("Name" :command com-name-PVSSB)
		("Show" :command com-show-PVSSB)
		("Union" :command com-union-PVSSB)
		("Intersection" :command com-intersect-PVSSB)
		("Difference" :command com-diff-PVSSB)))					
   ;;; END P/VSSB command table

   ;;; START P Plots command table
;;; a some commands which appear at various places in the table
(define-qmbar-command com-gridlay-P ()
	 (plot-menu-items 0))

(define-qmbar-command com-viewlay-P ()
	(view-layout :nsubviews 0 :draw? T))

(define-qmbar-command com-table-P ()
	(vw::prompt-plot-by :dim 0))   
;;; make the command table
(make-command-table 'p-command-table :errorp nil
	:menu '(("DataInfo" :menu pdsb-command-table)
		("Single variate" :menu psvsb-command-table)
		("Two variates" :menu ptvsb-command-table)
		("Multiple variates" :menu pmvsb-command-table)
		("Table" :command com-table-P)
		("Function" :menu pfsb-command-table)
		("Grid layout" :command com-gridlay-P)
		("View layout" :command com-viewlay-P)
		("" :command com-divider)
		("Linking" :menu plsb-command-table)
		("View sets" :menu pvssb-command-table)))
   ;;; END P command table
