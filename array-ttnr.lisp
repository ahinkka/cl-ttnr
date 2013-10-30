;;;;
;;;; ttnr.lisp
;;;;
;;;; Author: Atte Hinkka <atte.hinkka@iki.fi>
;;;;
;;
;; Usage example:
;; (let*
;;     ((graph (bmg:read-graph #P"/path/to/data.bmg"))
;;      (start-node (gethash "Protein_UniProt:P04083" (bmg:node-by-name graph)))
;;      (goal-node (gethash "Protein_UniProt:P07355" (bmg:node-by-name graph))))
;;   (time (array-ttnr:run-iterations
;; 	 #'array-ttnr:bfs-lazy-with-bit-vectors
;; 	 graph 5000 start-node goal-node)))
;;
;; (let*
;;     ((graph (bmg:read-graph #P"/path/to/data.bmg"))
;;      (start-node (gethash "Protein_UniProt:P04083" (bmg:node-by-name graph)))
;;      (goal-node (gethash "Protein_UniProt:P07355" (bmg:node-by-name graph))))
;;   (time (array-ttnr:run-iterations
;; 	 #'array-ttnr:bfs-with-precomputed-gamma
;; 	 graph 5000 start-node goal-node)))
;;

(defpackage #:array-ttnr
  (:use #:common-lisp #:iterate)
  (:export #:run-iterations
	   #:bfs-lazy-with-bit-vectors
	   #:bfs-with-precomputed-gamma
	   #:create-gamma))

(in-package :array-ttnr)


(defun create-weight-matrix (graph)
  "Returns a 2-dimensional float matrix."
  (let* ((max-node-id (slot-value graph 'bmg::max-node-id))
	 (array-size (+ 1 max-node-id))
	 (array (make-array (list array-size array-size) :element-type 'single-float)))
    (iterate
      (for i from 0 to max-node-id)
      (let ((node (gethash i (bmg:node-by-id graph))))
	(if node
	    (iterate (for edge in (bmg:edges node))
		     (setf (aref array i (bmg:id (bmg:other-node edge node))) (bmg:goodness edge))))))
    array))


(defun create-gamma (graph)
  "Returns a 2-dimensional binary matrix."
  (let* ((max-node-id (slot-value graph 'bmg::max-node-id))
	 (array-size (+ 1 max-node-id))
	 (array (make-array (list array-size array-size) :element-type 'bit)))
    (iterate
      (for i from 0 to max-node-id)
      (let ((node (gethash i (bmg:node-by-id graph))))
	(if node
	    (iterate (for edge in (bmg:edges node))
		     (let ((r (random 1.0))
			   (g (bmg:goodness edge)))
		       (declare (type single-float r g))
		       (if (> g r)
			   (setf (aref array i (bmg:id (bmg:other-node edge node))) 1)
			   (setf (aref array i (bmg:id (bmg:other-node edge node))) 0)))))))
    array))


(defun bfs-with-precomputed-gamma (graph start-node goal-node &optional
				   (gamma (create-gamma graph))
				   (visited (make-array (list (slot-value graph 'bmg::max-node-id)) :element-type 'bit)))
  (declare (type (simple-array bit) visited))
  (declare (type (array bit 2) gamma))
  (declare (optimize (speed 3) (safety 0) (space 0) (debug 0) (compilation-speed 0)))
  (let ((queue (list (bmg:id start-node)))
	(goal-node (bmg:id goal-node))
	(max-node-id (slot-value graph 'bmg::max-node-id)))

    (loop while queue do
	 (let ((current-node (pop queue)))
	   (if (eq current-node goal-node)
	       (return-from bfs-with-precomputed-gamma t))
	   (setf (aref visited current-node) 1)

 	   (iterate (for i from 0 to max-node-id)
		    (when (eq 1 (aref gamma current-node i))
		      (when (eq 0 (aref visited i))
			(if (not queue)
			    (setf queue (cons i queue))
			    (nconc queue (list i))))))))))


(defun bfs-lazy-with-bit-vectors (graph start-node goal-node)
  (declare (optimize (speed 3) (safety 0) (space 0) (debug 0) (compilation-speed 0)))
  (let* ((weights (create-weight-matrix graph))
	 (queue (list (bmg:id start-node)))
	 (goal-node-id (bmg:id goal-node))
	 (max-node-id (slot-value graph 'bmg::max-node-id))
	 (array-size (+ 1 max-node-id))
	 (visited (make-array (list array-size) :element-type 'bit))
	 (calculated (make-array (list array-size array-size) :element-type 'bit))
	 (connected (make-array (list array-size array-size) :element-type 'bit)))
    (declare (type (simple-array single-float 2) weights))
    (declare (type fixnum goal-node-id array-size max-node-id))
    (declare (type (simple-array bit) visited))
    (declare (type (array bit 2) calculated connected))

    (loop while queue do
	 (let ((current-node (pop queue)))
	   (when (eq current-node goal-node-id)
	     (return-from bfs-lazy-with-bit-vectors t))
	   (setf (aref visited current-node) 1)

 	   (dotimes (i max-node-id)
	     (let
		 ((edge-existence-calculated (eq 1 (aref calculated current-node i)))
		  (edge-weight (aref weights current-node i)))
	       (declare (type single-float edge-weight))

	       (when (and
		      (eq nil edge-existence-calculated)
		      (> edge-weight 0.0))

		 (setf (aref connected current-node i)
		       (let ((random-float (random 1.0)))
			 (declare (type single-float random-float))
			 (if (> edge-weight random-float) 1 0)))
		 (setf (aref calculated current-node i) 1))

	       (when (eq 1 (aref connected current-node i))
		 (unless (eq 1 (aref visited i))
		   (if (not queue)
		       (setf queue (cons i queue))
		       (nconc queue (list i)))))))))))


(defun run-iterations (implementation graph iteration-count from to)
  (let ((successful-iterations 0))
    (dotimes (iterations-so-far iteration-count)
      (when (funcall implementation graph from to)
	(incf successful-iterations)))
    (values (format nil "~f" (/ successful-iterations iteration-count))
	    successful-iterations iteration-count)))
