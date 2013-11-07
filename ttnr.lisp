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
;;    (best-paths nil))
;;    (ttnr:remove-dangling-nodes graph (list start-node goal-node))
;;    (ttnr:remove-serial-nodes graph (list start-node goal-node))
;;    (ttnr:remove-parallel-edges graph)
;;    (ttnr:remove-serial-nodes graph (list start-node goal-node))
;;    (ttnr:remove-parallel-edges graph)
;;    (ttnr:remove-dangling-nodes graph (list start-node goal-node))
;;    (setf best-paths (bmg-bestpath:best-distinct-paths start-node goal-node))
;;    (time (ttnr:run-iterations #'ttnr:bfs-with-hashes 100000 start-node goal-node best-paths)))
;; Or
;;  (let*
;;      ((graph (bmg:read-graph #P"/path/to/data.bmg"))
;;       (start-node (gethash "Protein_UniProt:P04083" (bmg:node-by-name graph)))
;;       (goal-node (gethash "Protein_UniProt:P07355" (bmg:node-by-name graph)))
;;       (mg nil)
;;       (best-path-nodes nil))
;;    (ttnr:remove-dangling-nodes graph (list start-node goal-node))
;;    (ttnr:remove-serial-nodes graph (list start-node goal-node))
;;    (ttnr:remove-parallel-edges graph)
;;    (ttnr:remove-serial-nodes graph (list start-node goal-node))
;;    (ttnr:remove-parallel-edges graph)
;;    (ttnr:remove-dangling-nodes graph (list start-node goal-node))
;;    (setf best-path-nodes (bmg-bestpath:collect-nodes (bmg-bestpath:best-distinct-paths start-node goal-node)))
;;    (setf mg (ttnr:bmgraph-to-minimal-graph graph))
;;    (setf mg (mg:order-edges-by-weight mg))
;;    (setf mg (mg:order-edges-by-preferred-nodes mg
;;  					      (mapcar #'bmg:id best-path-nodes)))
;;    (setf mg (mg:order-graph-by-preferred-nodes mg (mapcar #'bmg:id best-path-nodes)))
;;    (time (ttnr:run-iterations #'ttnr:bfs-with-minimal-graph 100000 (bmg:id start-node) (bmg:id goal-node) mg)))


(defpackage #:ttnr
  (:use #:common-lisp)
  (:export #:run-iterations
	   #:remove-dangling-nodes
	   #:remove-serial-nodes
	   #:remove-parallel-edges
	   #:bmgraph-to-minimal-graph
	   #:bfs-with-hashes
	   #:bfs-with-minimal-graph))

(in-package :ttnr)


(defun bfs-with-hashes (start-node goal-node)
  (declare (optimize (speed 3) (safety 0) (space 0) (debug 0) (compilation-speed 0)))
  (let ((queue (list start-node))
	(visited (make-hash-table :test #'eq))
	(edges-not-exist (make-hash-table :test #'eq)))

    (loop while queue do
	 (let ((current-node (pop queue)))
	   (when (eq current-node goal-node) (return-from bfs-with-hashes t))
	   (setf (gethash current-node visited) t)
	   (dolist (edge (bmg:edges current-node))
	     (let ((other-node (bmg:other-node edge current-node)))
	       (when (null (gethash other-node visited))
		 (when (null (gethash edge edges-not-exist))
		   (let ((r (random 1.0))
			 (g (bmg:goodness edge)))
		     (declare (type single-float r g))
		     (if (> g r)
			 (if (null queue)
			     (setf queue (cons other-node nil))
			     (nconc queue (list other-node)))
			 (setf (gethash edge edges-not-exist) t)))))))))))


(defun bfs-with-minimal-graph (start-node goal-node graph)
  (declare (optimize (speed 3) (safety 0) (space 0) (debug 0) (compilation-speed 0)))
  (let ((queue (list start-node))
	(visited (make-hash-table :test #'eq))
	(edges-not-exist (make-hash-table :test #'eq)))

    (loop while queue do
	 (let ((current-node (pop queue)))
	   (when (eq current-node goal-node) (return-from bfs-with-minimal-graph t))
	   (setf (gethash current-node visited) t)
	   (dolist (edge (mg:edges graph current-node))
	     (let ((other-node (car edge)))
	       (when (null (gethash other-node visited))
		 (when (null (gethash edge edges-not-exist))
		   (let ((r (random 1.0))
			 (g (cdr edge)))
		     (declare (type single-float r g))
		     (if (> g r)
			 (if (null queue)
			     (setf queue (cons other-node nil))
			     (nconc queue (list other-node)))
			 (setf (gethash edge edges-not-exist) t)))))))))))


(defun bmgraph-to-minimal-graph (in)
  (let ((graph (mg:make-graph))
	(included (make-hash-table :test #'eq)))
    (dolist (node (alexandria:hash-table-values (bmg:node-by-id in)))
      (setf graph (mg:add-node graph (bmg:id node))))
    
    (dolist (node (alexandria:hash-table-values (bmg:node-by-id in)))
      (dolist (edge (bmg:edges node))
	(when (not (gethash edge included))
	  (setf graph (mg:add-edge graph
				   (bmg:id (bmg::from edge)) (bmg:id (bmg::to edge))
				   (bmg:goodness edge)))
	  (setf (gethash edge included) t))))

    graph))


(defun run-iterations (implementation iteration-count from to &rest arguments)
  (let ((successful-iterations 0)
	(arglist (nconc (list from to) arguments)))
    (declare (type fixnum successful-iterations))

    (dotimes (iterations-so-far iteration-count)
      ;; (if (= (mod iterations-so-far 100000) 0)
      ;; 	  (format *error-output* "~a~%" iterations-so-far))
      (if (apply implementation arglist)
	  (incf successful-iterations)))
    (values (format nil "~f" (/ successful-iterations iteration-count))
	    successful-iterations iteration-count)))
