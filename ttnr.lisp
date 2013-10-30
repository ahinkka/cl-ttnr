;;;;
;;;; ttnr.lisp
;;;;
;;;; Author: Atte Hinkka <atte.hinkka@iki.fi>
;;;;
;;
;; Usage example:
;;  (let*
;;   ((graph (bmg:read-graph #P"/path/to/graph.bmg"))
;;    (start-node (gethash "Protein_UniProt:P04083" (bmg:node-by-name graph)))
;;    (goal-node (gethash "Protein_UniProt:P07355" (bmg:node-by-name graph))))
;; (setf graph (ttnr:remove-dangling-nodes graph (list start-node goal-node)))
;; (time (ttnr:run-iterations #'ttnr:randomized-bfs 25000 start-node goal-node)))
;;

(defpackage #:ttnr
  (:use #:common-lisp)
  (:export #:run-iterations
	   #:remove-dangling-nodes
	   #:remove-serial-nodes
	   #:randomized-bfs))

(in-package :ttnr)


(defun collect-nodes-by-degree (graph degree)
  (loop for node being the hash-values in (bmg:node-by-id graph)
     when
       (= (length (bmg:edges node)) degree)
     collect node))


(defun remove-dangling-nodes (graph preservables)
  (let* ((visited (make-hash-table :test #'eq))
	 (queue (collect-nodes-by-degree graph 1)))
    (loop while queue do
	 (let
	     ((current-node (pop queue)))
	   (setf (gethash (bmg:id current-node) visited) t)
	   (dolist (edge (bmg:edges current-node))
	     (let ((other-node (bmg:other-node edge current-node)))
	       (if (not (gethash (bmg:id other-node) visited))
		   (if (not queue)
		       (setf queue (cons other-node queue))
		       (nconc queue (list other-node))))))
	   (if (and
		(not (member current-node preservables))
		(< (length (bmg:edges current-node)) 2))
	       (bmg:remove-node graph current-node))))))


(defun find-path-pair (graph exclusions)
  "Finds a pair of nodes that can be combined together, i.e. both are of degree 2."
  (let ((candidates (remove-if
		     #'(lambda (x) (member x exclusions :test #'bmg:node-equal))
		     (collect-nodes-by-degree graph 2))))
    (dolist (node candidates)
      (dolist (edge (bmg:edges node))
	(let ((neighbor (bmg:other-node edge node)))
	  (when (member neighbor candidates :test #'bmg:node-equal)
	    (return-from find-path-pair (list node neighbor))))))))


(defun collect-neighbors (nodes)
  "Collects all the neighboring nodes of given list of nodes."
  (set-difference
   (alexandria:flatten
    (loop for node in nodes
       collect (loop for edge in (bmg:edges node)
		 collect (bmg:other-node edge node))))
   nodes :test #'bmg:node-equal))


(defun collect-edges (nodes)
  (alexandria:flatten
   (loop for node in nodes
      collect (loop for edge in (bmg:edges node)
		 collect edge))))


(defun combine-nodes (graph node another-node)
  "Combines a pair of nodes into a new node."
  (let*
      ((new-node (bmg:add-node graph (string (gensym))))
       (neighbors (collect-neighbors (list node another-node)))
       (all-edges (collect-edges (list node another-node)))
       (goodness (apply #'min (mapcar #'bmg:goodness all-edges))))
    (bmg:remove-node graph node)
    (bmg:remove-node graph another-node)

    (dolist (neighbor neighbors)
      (let
	  ((edge (make-instance 'bmg:edge
				:from new-node
				:to neighbor
				:goodness goodness
				:id (incf (bmg::max-edge-id graph)))))
	(bmg:add-edge (bmg::from edge) edge)
	(bmg:add-edge (bmg::to edge) edge)))
    new-node))


(defun remove-serial-nodes (graph exclusions)
  (let
      ((removed 0))
    (loop do
	 (let
	     ((pair (ttnr::find-path-pair graph exclusions)))
	   (unless pair (return))
	   (incf removed)
	   (ttnr::combine-nodes graph (first pair) (second pair))))
    removed))


(defun randomized-bfs (start-node goal-node
		       &optional (visited (make-hash-table :test #'eq))
		       (edges-not-exist (make-hash-table :test #'eq)))
  (declare (optimize (speed 3) (safety 0) (space 0) (debug 0) (compilation-speed 0)))
  (let ((queue (list start-node)))
    (loop while queue do
	 (let* ((current-node (pop queue))
		(current-node-id (bmg:id current-node)))
	   (when (bmg:node-equal current-node goal-node) (return-from randomized-bfs t))
	   (setf (gethash current-node-id visited) t)
	   (dolist (edge (bmg:edges current-node))
	     (let* ((other-node (bmg:other-node edge current-node))
		    (other-node-id (bmg:id other-node))
		    (edge-id (bmg:id edge)))
	       (declare (type fixnum other-node-id edge-id))
	       (when (and (not (gethash other-node-id visited))
			  (not (gethash edge-id edges-not-exist)))
		 (let ((r (random 1.0))
		       (g (bmg:goodness edge)))
		   (declare (type single-float r g))
		   (when (> g r)
		     (if (not queue)
			 (setf queue (cons other-node queue))
			 (nconc queue (list other-node)))
		     (setf (gethash (bmg:id edge) edges-not-exist) t))))))))))


(defun run-iterations (implementation iteration-count from to)
  (let ((succesful-iterations 0))
    (dotimes (iterations-so-far iteration-count)
      ;; (if (= (mod iterations-so-far 100000) 0)
      ;; 	  (format *error-output* "~a~%" iterations-so-far))
      (if (funcall implementation from to)
	    (incf succesful-iterations)))
    (values (format nil "~f" (/ succesful-iterations iteration-count))
	    succesful-iterations iteration-count)))
