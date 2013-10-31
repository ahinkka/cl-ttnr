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
	   #:remove-parallel-edges
	   #:randomized-bfs))

(in-package :ttnr)


(defun collect-nodes-by-degree (graph degree)
  (loop for node being the hash-values in (bmg:node-by-id graph)
     when
       (= (length (bmg:edges node)) degree)
     collect node))


(defun remove-dangling-nodes (graph preservables)
  (let* ((visited (make-hash-table :test #'eq))
	 (queue (collect-nodes-by-degree graph 1))
	 (removed 0))
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
	       (progn
		 (bmg:remove-node graph current-node)
		 (incf removed)))))
    (format *error-output* "Removed ~A dangling nodes~%" removed)))


(defun collect-neighbors (nodes)
  "Collects all the neighboring nodes of given list of nodes."
  (set-difference
   (alexandria:flatten
    (loop for node in nodes
       collect (loop for edge in (bmg:edges node)
		 collect (bmg:other-node edge node))))
   nodes :test #'bmg:node-equal))


(defun collect-edges (nodes)
  (remove-duplicates
   (alexandria:flatten
    (loop for node in nodes
       collect (loop for edge in (bmg:edges node)
		  collect edge)))
   :key #'bmg:id))


(defun remove-serial-node (graph node)
  (let*
      ((edges (collect-edges (list node)))
       (neighbors (mapcar #'(lambda (edge) (bmg:other-node edge node)) edges))
       (weights (mapcar #'bmg:goodness edges))
       (new-edge-weight (apply #'* weights))
       (new-edge (bmg:make-edge graph (first neighbors) (second neighbors) new-edge-weight)))
    (dolist (edge edges)
      (bmg:remove-edge edge))
    (bmg:remove-node graph node)
    
    (bmg:add-edge (first neighbors) new-edge)
    (bmg:add-edge (second neighbors) new-edge)))


(defun remove-serial-nodes (graph exclusions)
  (let
      ((removed 0))
    (loop do
	 (let
	     ((candidates (remove-if
			   #'(lambda (x) (member x exclusions :test #'bmg:node-equal))
			   (collect-nodes-by-degree graph 2))))
	   (unless candidates
	     (progn
	       (format *error-output* "Removed ~A serial nodes~%" removed)
	       (return-from remove-serial-nodes removed)))
	   (incf removed)
	   (remove-serial-node graph (first candidates))))))


(defun remove-parallel-edges-helper (graph)
  (let*
      ((removed 0)
       (candidates
	(mapcar #'(lambda (edge) (cons (list (bmg::from edge) (bmg::to edge)) edge))
		(collect-edges (alexandria:hash-table-values (bmg:node-by-id graph)))))
       (unique-edges (remove-duplicates (mapcar #'car candidates) :test #'equal))
       (counts
	(mapcar #'(lambda (edge)
		    (cons edge (count edge candidates :key #'car :test #'equal)))
		unique-edges))
       (real-candidates (remove-if #'(lambda (x) (< (cdr x) 2)) counts)))
    (dolist (candidate real-candidates)
      (let
	  (first second)
	(setf first (assoc (car candidate) candidates :test #'equal))
	(setf candidates (delete first candidates))
	(setf second (assoc (car candidate) candidates :test #'equal))
	(setf candidates (delete second candidates))

	(let
	    ((e1 (cdr first))
	     (e2 (cdr second)))

	  (setf (bmg:goodness e1)
	      (/ 1 (+ (/ 1 (max (bmg:goodness e1)) (/ 1 (bmg:goodness e2))))))
	  (bmg:remove-edge e2)
	  (incf removed))))
    removed))


(defun remove-parallel-edges (graph)
  (let
      ((removed 0))
    (loop do
	 (let
	     ((removed-this-round (remove-parallel-edges-helper graph)))
	   (incf removed removed-this-round)

	   (when (= 0 removed-this-round)
	     (progn
	       (format *error-output* "Removed ~A parallel edges~%" removed)
	       (return-from remove-parallel-edges removed)))))))


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
	       (unless (or
			(gethash other-node-id visited)
			(gethash edge-id edges-not-exist))
		 (let ((r (random 1.0))
		       (g (bmg:goodness edge)))
		   (declare (type single-float r g))
		   (if (> g r)
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
