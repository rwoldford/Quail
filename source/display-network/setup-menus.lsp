;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                              setup-menus.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1992 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1992.
;;;
;;;
;;;----------------------------------------------------------------------------

(in-package :quail)


(add-function-in-menu #'in-view "in view" '|Objects in this view| 'self)
(add-function-in-menu #'in-view! "in view!" '|Objects in this view| 'self)
(add-function-in-menu #'widen-view "widen view" '|Widen this view| 'self)
(add-function-in-menu #'add-forward-analysis-link "add forward analysis link"
                      'Link-operators 'self 'linkee)
(add-function-in-menu #'analysis-link "analysis link"
                      'Link-operators 'self 'linkee)
(add-function-in-menu #'analysis-unlink "analysis unlink"
                      'Link-operators 'self 'linkee)
(add-function-in-menu #'remove-back-analysis-link "remove back analysis-link"
                      'Link-operators 'self 'linkee)
(add-function-in-menu #'remove-forward-analysis-link "remove forward analysis-link"
                      'Link-operators 'self 'linkee)
(add-function-in-menu #'and "and" 'binary-operation 'arg-left 'arg-right)
(add-function-in-menu #'or "or" 'binary-operation 'arg-left 'arg-right)
(add-function-in-menu #'xor "xor" 'binary-operation 'arg-left 'arg-right)
(add-function-in-menu #'* "*" 'binary-operation 'arg-left 'arg-right)
(add-function-in-menu #'+ "+" 'binary-operation 'arg-left 'arg-right)
(add-function-in-menu #'- "-" 'binary-operation 'arg-left 'arg-right)
(add-function-in-menu #'/ "/" 'binary-operation 'arg-left 'arg-right)
(add-function-in-menu #'< "<" 'binary-comparison 'arg-left 'arg-right)
(add-function-in-menu #'= "=" 'binary-comparison 'arg-left 'arg-right)
(add-function-in-menu #'> ">" 'binary-comparison 'arg-left 'arg-right)
(add-function-in-menu #'mod "mod" 'binary-operation 'arg-left 'arg-right)
(add-function-in-menu #'rem "rem" 'binary-operation 'arg-left 'arg-right)
(add-function-in-menu #'^ "^" 'binary-operation 'arg-left 'arg-right)
(add-function-in-menu #'!= "!=" 'binary-comparison 'arg-left 'arg-right)
(add-function-in-menu #'! "!" 'reexpression 'self)
(add-function-in-menu #'abs "abs" 'reexpression 'self)
(add-function-in-menu #'acos "acos" 'reexpression 'self)
(add-function-in-menu #'exp "exp" 'reexpression 'self)
(add-function-in-menu #'expt "expt" 'reexpression 'self 'exponent)
(add-function-in-menu #'floor "floor" 'reexpression 'self)
;;(add-function-in-menu #'gamma "gamma" 'reexpression 'self)
;;(add-function-in-menu #'lgamma "lgamma" 'reexpression 'self)
(add-function-in-menu #'log "log" 'reexpression 'self)
(add-function-in-menu #'log10 "log 10" 'reexpression 'self)
(add-function-in-menu #'sin "sin" 'reexpression 'self)
(add-function-in-menu #'sqrt "sqrt" 'reexpression 'self)
(add-function-in-menu #'trunc "trunc" 'reexpression 'self)
(add-function-in-menu #'unary-minus "unary minus" 'reexpression 'self)
(add-function-in-menu #'asin "asin" 'reexpression 'self)
(add-function-in-menu #'atan "atan" 'reexpression 'self)
(add-function-in-menu #'ceiling "ceiling" 'reexpression 'self)
(add-function-in-menu #'cos "cos" 'reexpression 'self)
(add-function-in-menu #'add-analysis-node "add analysis node" '|Widen this view|
                      'self '|new analysis node|)
