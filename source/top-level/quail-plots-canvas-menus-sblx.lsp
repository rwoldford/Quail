;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;
;;;
;;;                          quail-plots-canvas-menus-sblx.lsp                               
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
;;; plots-menu-sblx.lsp  makes only the Plots menu through the p-command-table
;;; canvas-menu-sblx.lsp  makes on the Canvas menu through the c-command-table
;;; quail-plots-menu-sblx.lsp  makes the table for Quail and Plots menus
;;; quail-plots-canvas-menus-sblx.lsp  makes the table for Quail, Plots, and Canvas
;;; change-menu-bar-sblx.lsp  contains code for replacing a command-table
;;; Here is mcclim-like code to make the (mcclim)command-tables for the Quail menus

(in-package :wb);(in-package :quail)
(eval-when (:compile-toplevel :load-toplevel :execute) (export '(qpc-command-table)))

;;; make a command table for q,p,c
(make-command-table 'qpc-command-table ;quailmenubar-command-table 
                    :errorp nil
                    :menu '(("Quail" :menu wb::q-command-table)
                                       ("Plots" :menu wb::p-command-table)
                                       ("Canvas" :menu wb::c-command-table)))