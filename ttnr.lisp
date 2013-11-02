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
