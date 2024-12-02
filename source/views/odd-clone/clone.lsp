;;; This is ~/RESERVE/mintq/source/views/odd-clone/clone.lsp
;;; Holding just 2 things from the real file clone.lsp
;;; to see whether I can get passed the subview-keys-of problem
;;; Started 12OCT2024

(in-package :views)

(defgeneric subview-keys-of (view)
  (:documentation "Returns a list of keywords. ~
                   When constructing a view, if a keyword in this list~
                   appears more than once in the view construction arglist~
                   the values are appended."))

(defmethod subview-keys-of ((self view)))