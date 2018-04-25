;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               grapher.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;
;;;
;;;  Authors:
;;;     G. Desvignes  1988-89
;;;     J.R. MacPhail 1995
;;;     R.W. Oldford  1988-1992
;;;
;;;
;;;----------------------------------------------------------------------------------

;;; This is what remains of the old "grapher.lisp" after factoring out
;;; "grapher-node.lisp", "grapher-links.lisp", "grapher-layout-lisp" and
;;; "grapher-draw.lisp".


(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(graph-node-lst
          find-graph-node
          get-node-from-id
          graph-region
          reverse)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Structure definition :
;;;


(defstruct graph
  node-lst 
  side-flg 
  directed-flg)


;; Change this to a defmethod, with graph and position arguments. -- jrm

(defun find-graph-node (canvas position)
  "Return the node hit, NIL if position not in a node."
  
  (let* ((graph (wb:display-of canvas))
         (node-lst (if graph
                     (graph-node-lst graph))))
    (if node-lst
      (node-lst-as-menu node-lst position)
      NIL)))


;; This is only used by find-graph-node.  Clean it up to use gn-top and friends,
;; and then put it into find-graph-node.  -- jrm

(defun node-lst-as-menu (node-lst position)
  "Find the node that is closest to position and returns it ~
   the node N of Node-lst is returned if position is inside the region it ~
   occupies in the window otherwise NIL is returned."
  
  (let ((x (wb:position-x position))
        (y (wb:position-y position))
        t1 t2)
    (dolist (n node-lst nil)
      (when (and (< (- (setq t1 (wb:position-y (graph-node-position  n)))
                       (setq t2 (half (graph-node-height n))))
                    y)
                 (< y (+ t1 t2))
                 (< (- (setq t1 (wb:position-x (graph-node-position
                                             n)))
                       (setq t2 (half (graph-node-width n))))
                    x)
                 (< x (+ t1 t2)))
        (return n)))))


;; The somewhat bizarre comment seems rather irrelevant.  The only point is
;; that the id argument may be a list. -- jrm

(defun get-node-from-id (id node-lst &optional no-err-flg)
  "Returns node associated to ID."

       (or (dolist (node node-lst nil)
               (when (eq id (graph-node-id node))
                     (return node)))
           (and id (listp id)
                                               ; the nodes created when we
                                               ; break cycles have the same
                                               ; name than the original node
                                               ; in a list, so both node name
                                               ; should return the same graph
                                               ; node
                (get-node-from-id (first id)
                       node-lst))
           (if no-err-flg
               nil
               (quail-error "No graphnode for ID : ~S" id))))



;; The max-min family can all be made cleaner by using mapcar.
;; For speed, the node lists could be sorted to find the x, y minima fastest. -- jrm

(defun max-right (node-lst)
  "Returns the largest right position of nodes in Node-lst."

       (let ((largest (gn-right (car node-lst))))
            (dolist (node node-lst)
                (when (> (gn-right node)
                         largest)
                    (setq largest (gn-right node))))
            largest))

(defun max-top (node-lst)
  "Returns the largest Top position of nodes in Node-Lst."

       (let ((largest (gn-top (car node-lst))))
            (dolist (node node-lst)
                (when (> (gn-top node)
                         largest)
                    (setq largest (gn-top node))))
            largest))

(defun min-bottom (node-lst)
  "Returns the smallest bottom position of nodes in Node-Lst."

       (let ((smallest (gn-bottom (car node-lst))))
            (dolist (node node-lst)
                (when (< (gn-bottom node)
                         smallest)
                    (setq smallest (gn-bottom node))))
            smallest))

(defun min-left (node-lst)
  "Returns the smallest left position of nodes in Node-lst."

       (let ((smallest (gn-left (car node-lst))))
            (dolist (node node-lst)
                (when (< (gn-left node)
                         smallest)
                    (setq smallest (gn-left node))))
            smallest))


;; Notice that this function adjusts the nodes to get min-left = min-bottom = 0.
;; Suspicion: everything probably gets recomputed every time we add a node. -- jrm

(defun offset-to-origin (node-lst)
  "After layout, the computed positions can be negative, which can be a problem ~
   on some environments. To avoid this, we systematically offset the graph so ~
   that each position is positive."

  (let ((inf-x (min-left node-lst))
        (inf-y (min-bottom node-lst))
        pos)
     (unless (and (eq 0 inf-x)
                  (eq 0 inf-y))
         (dolist (node node-lst)
            (setq pos (graph-node-position node))
            (setf (wb:position-x pos) (- (wb:position-x pos) inf-x))
            (setf (wb:position-y pos) (- (wb:position-y pos) inf-y))))))


(defun adjust-graph-positions (graph x-adjust y-adjust)
  "Destructively ~
   adds x-adjust to all x-positions and y-adjust to all y-positions in graph. ~
   Returns graph."
  (cond
   ((zerop x-adjust)
    (if (zerop y-adjust)
      graph
      (dolist (n (graph-node-lst graph))
        (setf (wb:position-y (graph-node-position n))
              (+ y-adjust (wb:position-y (graph-node-position n)))))))
   ((zerop y-adjust)
    (dolist (n (graph-node-lst graph))
        (setf (wb:position-x (graph-node-position n))
              (+ x-adjust (wb:position-x (graph-node-position n))))))
   (T
    (dolist (n (graph-node-lst graph))
        (setf (wb:position-x (graph-node-position n))
              (+ x-adjust (wb:position-x (graph-node-position n))))
        (setf (wb:position-y (graph-node-position n))
              (+ y-adjust (wb:position-y (graph-node-position n))))))))


(defun eq-or-member (item atom-or-list)
  "Returns T if item EQ Atom-or-list or if item is a member of Atom-or-list."

       (or (eq item atom-or-list)
           (and (listp atom-or-list) (member item atom-or-list :test #'eq))))


(defun init-nodes-for-layout (ns format root-ids font)
       (declare (special the-node-lst))
       (dolist (gn ns)
           (setf (graph-node-position gn)
                 
                 ;; T indicate prior visitation. Roots are already visited
                 (not (not (member (graph-node-id gn)
                                  root-ids :test #'eq))))
           (or (graph-node-font gn)
               (setf (graph-node-font gn)
                     font)))
       (dolist (r root-ids)
           (if (eq-or-member 'lattice format)
               (lattice-break-cycles (get-node-from-id r the-node-lst)
                      nil)
               (forest-break-cycles (get-node-from-id r the-node-lst))))
       (dolist (gn the-node-lst)
           (setf (graph-node-position gn)
                 nil)
           (set-label-size gn)))

(defun interpret-mark-format (format)
  "Sets special variables for new-instance-of-graph-node and mark-graph-node."

       (declare (special box-both-flg box-leaves-flg border-for-marking 
                       label-shade-for-marking))
       (prog (pl)
             (and (eq-or-member 'copies-only format)
                  (setq box-both-flg nil))
             (and (eq-or-member 'not-leaves format)
                  (setq box-leaves-flg nil))
             (cond ((not (listp format))
                    (return))
                   ((eq (car format)
                        'mark)
                    (setq pl (cdr format)))
                   (t (return)))
             (if (member 'border pl :test #'eq)
                 (setq border-for-marking (getf pl 'border))
                 (setq border-for-marking 'dont))
             (if (member 'label-shade pl :test #'eq)
                 (setq label-shade-for-marking (getf pl 'label-shade))
                 (setq label-shade-for-marking 'dont))))


;;; Reflection routines, really used by grapher-layout.lisp -- jrm

(defun reflect-graph-diagonally (graph)
  (setf (graph-side-flg graph) (not (graph-side-flg graph)))
  (let (yn)
    (dolist (n (graph-node-lst graph))
      (setq n (graph-node-position n))
      (setq yn (wb:position-y n))
      (setf (wb:position-y n) (wb:position-x n))
      (setf (wb:position-x n) yn))))

(defun reflect-graph-horizontally (graph)

       (let ((w (+ (max-right (graph-node-lst graph))
                   (min-left (graph-node-lst graph)))))
            (dolist (n (graph-node-lst graph))
                (setq n (graph-node-position n))
                (setf (wb:position-x n)
                      (- w (wb:position-x n))))))

(defun reflect-graph-vertically (graph)

       (let ((h (+ (max-top (graph-node-lst graph))
                   (min-bottom (graph-node-lst graph)))))
            (dolist (n (graph-node-lst graph))
                (setq n (graph-node-position n))
                (setf (wb:position-y n)
                      (- h (wb:position-y n))))))

(defun switch-node-height-width (node-lst)
  
       (let (wn)
            (dolist (n node-lst)
                (setq wn (graph-node-width n))
                (setf (graph-node-width n)
                      (graph-node-height n))
                (setf (graph-node-height n)
                      wn))))


;;; Region stuff.

(defun graph-region (graph)
  "Return the minimum region containing the graph."
  
  (if graph
    (prog (left-offset bottom-offset (node-lst (graph-node-lst graph)))
      (return
       (cond (node-lst
              (dolist (n node-lst)
                (measure-graph-node n))
              (wb:make-region
               (setq left-offset (min-left node-lst))
               (setq bottom-offset (min-bottom 
                                    node-lst))
               (+ 1 (- (max-right node-lst)
                       left-offset))
               (+ 1 (- (max-top node-lst)
                       bottom-offset))))
             (t (wb:make-region 0 0 0 0)))))
    (wb:make-region 0 0 0 0)))



(defun intersect-regionp-lbwh (left bottom width height reg)
  "Like intersect regions, but without requiring the consing."

       (not (or (> (wb:region-bottom reg)
                   (+ bottom height))
                (< (+ (wb:region-left reg)
                      (wb:region-width reg))
                   left)
                (> (wb:region-left reg)
                   (+ left width))
                (< (+ (wb:region-bottom reg)
                      (wb:region-height reg))
                   bottom))))
