;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                     scrolling-windows-pc.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;  This file is part of the Window-Basics package intended to provide a uniform
;;;  window environment for Common Lisp programming.
;;;  
;;;  For copyright information and history, see window-basics-copyright.lisp
;;;  Authors:
;;;     R.W. Oldford 1989-1992
;;;     G.W. Bennett 1996
;;;----------------------------------------------------------------------------------
;;;  A new class of windows which contain scroll-bars and a scrollable
;;;  area.
;;; ----------------------------------------------------------------------------------
;;;  Adapted from the scrolling-windows.lisp 1989 examples file distributed by apple
;;;(require :scrollers)     ;; got it in wb-system-mcl.lisp
;;;  Default changed to t for track-thumb-p
;;;  my-scroller changed to the-scroller
;;;  set-view-size changed to redisplay the entire window               ... rwo 92
(in-package :wb)
(eval-when (:compile-toplevel :load-toplevel :execute) (export '()))
(defclass scrolling-window ()
  ())
