(in-package :ttnr)


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
			   #'(lambda (x) (member x exclusions :test #'eq))
			   (collect-nodes-by-degree graph 2))))
	   (unless candidates
	     (progn
	       (format *error-output* "Removed ~A serial nodes~%" removed)
	       (return-from remove-serial-nodes removed)))
	   (incf removed)
	   (remove-serial-node graph (first candidates))))))


(defun endpoint-edge-pairs (graph)
  "Returns a list of ((node . node) . edge) pairs."
  (flet ((sort-nodes (nodes)
	   (sort nodes #'< :key #'(lambda (node) (bmg:id node)))))
    (mapcar #'(lambda (edge)
		(cons
		 (sort-nodes (list (bmg::from edge) (bmg::to edge)))
		 edge))
	    (collect-edges (alexandria:hash-table-values (bmg:node-by-name graph))))))


(defun nodepairs-with-duplicate-edges (graph)
  (flet ((not-one (number)
	   (declare (type fixnum number))
	   (not (eq number 1))))
    (let*
	((pairs (endpoint-edge-pairs graph))
	 (unique-pairs (remove-duplicates pairs :key #'car :test #'equal))
	 (count-edge-pairs
	  (mapcar #'(lambda (pair)
		      (cons
		       (count pair pairs :key #'car :test #'equal)
		       pair))
		  (mapcar #'car unique-pairs))))
      (mapcar #'cdr (remove-if-not #'not-one count-edge-pairs :key #'car)))))


(defun remove-parallel-edges (graph)
  (let
      ((removed 0)
       (duplicate-pairs (nodepairs-with-duplicate-edges graph)))

    (dolist (pair duplicate-pairs)
      (let*
	  ((from (first pair))
	   (to (second pair))
	   (edges-between (remove-if-not
			   #'(lambda (edge)
			       (eq (bmg:other-node edge from) to))
			   (bmg:edges from)))
	   (new-weight (combine-parallel-edge-weights (mapcar #'bmg:goodness edges-between)))
	   (first-edge (car edges-between))
	   (rest-edges (cdr edges-between)))

	(setf (bmg:goodness first-edge) new-weight)

	(dolist (edge rest-edges)
	  (bmg:remove-edge edge)
	  (incf removed))))
    (format *error-output* "Removed ~A parallel edges~%" removed)
    removed))