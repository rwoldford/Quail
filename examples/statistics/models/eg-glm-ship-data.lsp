(in-package q-user)

;; ***********  POISSON FAMILY with LOG LINK  ***********

;; THIS FILE IS PROVIDED AS SOMETHING OF A CAVEAT ...

;; See McCullagh and Nelder, Generalized Linear Models, 2nd ed.
;; This dataset and an analysis is given in section 6.3.2.

;;  We have a tricky situation here with empty cells, some of which
;;  are structural, and some of which are accidental.

;;  The Quail GLM code picks up the right degrees of freedom for the
;;  main effects model under Approach #1, but not for the interaction
;;  case or for Approach #2.
;;
;;  I'm not sure yet how this problem in is dealt with in general.

(set-quail-data-directory (probe-file "q:Examples;Statistics;Models;"))

;; to undo:  (set-quail-data-directory nil :reset t)

(load (quail-file-pathname "full-factorial-design.lsp"))

;; ship damage data
;; column 0 is aggregate months of service
;; column 1 is number of damage incidents

(setf ship-data (array (scan "ship_data")
                       :dimensions '(t 2)))

(setf ship-type '(a b c d e))
(setf year-of-construction '("60-64" "65-69" "70-74" "75-79"))
(setf period-of-operation '("60-74" "75-79"))

(setf scov (full-factorial-design ship-type
                                  year-of-construction
                                  period-of-operation))

;;-----------------------------------------------------------------------

;; Approach #1 ... remove rows containing empty cells

;; we do a little bit of manipulation here to remove
;; the structural and accidental empty cells.

(setf structural-empties '(6 14 22 30 38))
(setf accidental-empties '(33))
(setf empties (append structural-empties accidental-empties))

(setf ship-cov (ref scov (list* :c empties) t))
(setf ship-dat (ref ship-data (list* :c empties) t))

(setf ships (data-frame (list (array (ref ship-cov t 0)
                                     :class 'factor-array
                                     :levels ship-type)
                              (array (ref ship-cov t 1)
                                     :class 'factor-array
                                     :levels year-of-construction)
                              (array (ref ship-cov t 2)
                                     :class 'factor-array
                                     :levels period-of-operation)
                              (ref ship-dat t 0)
                              (ref ship-dat t 1))
                        (list "type"
                              "year"
                              "period"
                              "months"
                              "damage")))

(setf s1 (glm "damage ~ type + year + period"
              ships
              :family :poisson
              :link :log
              :offset (log (eref ships "months"))
              :tolerance 1.0d-3))

;;  Here we get the degrees of freedom wrong 
;;  as described in McCullagh & Nelder, p 208-9.

(setf s2 (glm "damage ~ type * year + period"
              ships
              :family :poisson
              :link :log
              :offset (log (eref ships "months"))
              :tolerance 1.0d-3))

;;-----------------------------------------------------------------------

;; Approach #2 ...  set weight to be 0 for empty-cell observations.

;; Here we have too many degrees of freedom, since
;; many of the empty cells are structural.

;; Estimation works, though.

(setf ships2 (data-frame (list (array (ref scov t 0)
                                      :class 'factor-array
                                      :levels ship-type)
                               (array (ref scov t 1)
                                      :class 'factor-array
                                      :levels year-of-construction)
                               (array (ref scov t 2)
                                      :class 'factor-array
                                      :levels period-of-operation)
                               (ref ship-data t 0)
                               (ref ship-data t 1))
                         (list "type"
                               "year"
                               "period"
                               "months"
                               "damage")))

(setf weight
      (let ((log-months (log (eref ships2 "months")))
            (weight (array 1 :dimensions (dimensions-of log-months))))
        (loop for i upto (1- (first (dimensions-of log-months)))
              when (eq (eref log-months i) -infinity)
              do (setf (eref weight i) 0))
        weight))

(setf s3 (glm "damage ~ type + year + period"
              ships2
              :family :poisson
              :link :log
              :offset (log (eref ships "months"))
              :weight weight
              :tolerance 1e-3))

(setf s4 (glm "damage ~ type * year + period"
              ships2
              :family :poisson
              :link :log
              :offset (log (eref ships "months"))
              :weight weight
              :tolerance 1e-3))

