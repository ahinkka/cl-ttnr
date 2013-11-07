;;;;
;;;; Minimal neighbor list graph representation with alists
;;;;
;;;; Numbers should be used as nodes.
;;;;

(defpackage #:minimal-graph
  (:use #:common-lisp)
  (:nicknames #:mg)
  (:export #:make-graph #:add-node #:edges #:add-edge #:nodes #:order-edges-by-weight
	   #:order-edges-by-preferred-nodes #:order-graph-by-preferred-nodes))

(in-package :minimal-graph)


(defun make-graph ()
  nil)


(defun add-node (graph node)
  (acons node '() graph))


(defun edges (graph node)
  (cdr (assoc node graph :test #'eq)))


(defun nodes (graph)
  (mapcar #'car graph))


(defun add-edge (graph from to weight)
  (let*
      ((from-node (assoc from graph))
       (to-node (assoc to graph)))

    (assert from-node)
    (assert to-node)

    (setf (cdr from-node)
	  (acons to weight (cdr from-node)))

    (setf (cdr to-node)
	  (acons from weight (cdr to-node)))
    graph))


(defun order-edges-by-weight (graph)
  (dolist (pair graph)
    (setf (cdr pair)
	  (sort (cdr pair) #'> :key #'cdr)))
  graph)


(defun order-edges-by-preferred-nodes (graph preferred-nodes)
  (dolist (pair graph)
    (setf (cdr pair)
	  (sort (cdr pair)
		#'(lambda (x y)
		    (let
			((x-preferred (member x preferred-nodes :test #'eq))
			 (y-preferred (member y preferred-nodes :test #'eq)))
		      (cond
			((and x-preferred (not y-preferred)) t)
			((and (not x-preferred) y-preferred) nil)
			(t t))))
		:key #'car)))
  graph)


(defun order-graph-by-preferred-nodes (graph preferred-nodes)
  (sort graph
	#'(lambda (x y)
	    (let
		((x-preferred (member x preferred-nodes :test #'eq))
		 (y-preferred (member y preferred-nodes :test #'eq)))
	      (cond
		((and x-preferred (not y-preferred)) t)
		((and (not x-preferred) y-preferred) nil)
		(t t))))
	:key #'car))
