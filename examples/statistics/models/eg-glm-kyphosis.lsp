(in-package :q-user)

;; ***********  BINOMIAL FAMILY with LOGIT LINK  ***********

;; A brief GLM analysis of kyphosis data following 
;;   Hastie & Tibshirani, Generalized Additive Models, section 10.2

(set-quail-data-directory "q:Examples;Statistics;Models;")

;; to undo:  (set-quail-data-directory nil :reset t)

(setf kyp-dat (array (scan "kyphosis.lsp") :dimensions '(t 4)))

;; Currently, we don't have a rich set of representations for GLM data  ...
;; For now, binomial data is given as two columns: 
;; one column of counts of successes for these covariates, one column of failures.
;; The data here are the common bernoulli case ie. one individual per value of covariates.

(setf binomial-response (cglue (ref kyp-dat t 3) (- 1 (ref kyp-dat t 3))))

(setf kyp (data-frame (list binomial-response
                            (ref kyp-dat t 0)
                            (ref kyp-dat t 1)
                            (ref kyp-dat t 2))  ;; the data
                      (list "kyphosis"
                            "age"
                            "start"
                            "number")  ;; the corresponding names
                      ))

;; The following calls to glm illustrate variations on specifying link and family.
;;
;; eg. for binomial family, can use:
;;       'binomial, :binomial, 'binomial-family, :binomial-family, "binomial", or "binomial-family"
;;
;;     The above specifications are package-independent.
;;     In packages which use :quail package, such as :quail-user, there are also symbols bound to
;;     the correct instance, eg binomial-family 

(setf gl0 (glm "kyphosis ~ 1"              ;; null model ie. intercept only 
               kyp
               :link :logit
               :family :binomial-family))

(defun single-cut (x cut-point)
  (map-element #'(lambda (xx) (if (> xx cut-point) 1 0))
               :row
               x))


(defun single-poly (x degree)
  (if (> (ncols x) 1)
    (error "Can only handle (non-orthogonal) polynomials in one variable, currently.")
    (apply #'cglue (loop for d from 1 to degree
                         collect (** x d)))))

(setf gl1 (glm "kyphosis ~ age + {(** age 2)} + {(** age 3)} +
                           start + {(** start 2)} + {(** start 3)} +
                           number + {(** number 2)} + {(** number 3)}"
               kyp
               :link 'logit
               :family "binomial"))

;; or equivalently ...

(setf gl1b (glm "kyphosis ~ {(single-poly age 3)} + {(single-poly start 3)} + {(single-poly number 3)}"
                kyp
                :link 'logit
                :family "binomial"))

(setf gl2 (glm "kyphosis ~ {(single-poly age 2)} + {(* (- start 12) (single-cut start 12))}" 
               kyp
               :link 'logit-link
               :family binomial-family
