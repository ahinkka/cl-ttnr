;;;;
;;;; ttnr.lisp
;;;;
;;;; Author: Atte Hinkka <atte.hinkka@iki.fi>
;;;;
;;
;; Usage example:
;;  (let*
;;      ((graph (bmg:read-graph #P"/path/to/data.bmg"))
;;       (start-node (gethash "Protein_UniProt:P04083" (bmg:node-by-name graph)))
;;       (goal-node (gethash "Protein_UniProt:P07355" (bmg:node-by-name graph))))
;;    (time (run-iterations graph 5000 start-node goal-node)))
;;
;;  (let*
;;      ((graph (bmg:read-graph #P"/path/to/data.bmg"))
;;       (start-node (gethash "Protein_UniProt:P04083" (bmg:node-by-name graph)))
;;       (goal-node (gethash "Protein_UniProt:P07355" (bmg:node-by-name graph))))
;;    (time (run-lazy-iterations graph 10000 start-node goal-node)))

(defpackage #:array-ttnr
  (:use #:common-lisp #:iterate)
  (:export #:run-iterations
	   #:randomized-bfs))

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


(defun bfs-with-gamma (start-node goal-node gamma &optional
		       (visited (make-array (list (array-dimension gamma 0)) :element-type 'bit)))
  (declare (optimize (speed 3) (safety 0) (space 0) (debug 0) (compilation-speed 0)))
  (let ((queue (list (bmg:id start-node)))
	(goal-node (bmg:id goal-node))
	(max-node-id (- (array-dimension gamma 0) 1)))
    (loop while queue do
	 (let ((current-node (pop queue)))
	   (if (eq current-node goal-node)
	       (return-from bfs-with-gamma t))
	   (setf (aref visited current-node) 1)

 	   (iterate (for i from 0 to max-node-id)
		    (when (eq 1 (aref gamma current-node i))
		      (if (eq 0 (aref visited i))
			  (if (not queue)
			      (setf queue (cons i queue))
			      (nconc queue (list i))))))))))


(defun lazy-random-bfs (graph start-node goal-node)
  (let* ((weights (create-weight-matrix graph))
	 (queue (list (bmg:id start-node)))
	 (goal-node (bmg:id goal-node))
	 (max-node-id (slot-value graph 'bmg::max-node-id))
	 (array-size (+ 1 max-node-id))
	 (visited (make-array (list array-size) :element-type 'bit))
	 (calculated (make-array (list array-size array-size) :element-type 'bit))
	 (connected (make-array (list array-size array-size) :element-type 'bit)))

    (loop while queue do
	 (let ((current-node (pop queue)))
	   (if (= current-node goal-node)
	       (return-from lazy-random-bfs t))
	   (setf (aref visited current-node) 1)

 	   (iterate (for i from 0 to max-node-id)
		    (progn
		      (when (and
			     (eq 0 (aref calculated current-node i))
			     (> 0.0 (aref weights current-node i)))
			(setf (aref connected current-node i)
			      (let ((r (random 1.0))
				    (g (aref weights current-node i)))
				(declare (type single-float r g))
				(if (> g r)
				    1
				    0))))
		      (if (eq 1 (aref connected current-node i))
			  (if (not queue)
			      (setf queue (cons i queue))
			      (nconc queue (list i))))))))))


(defun run-lazy-iterations (graph iteration-count from to)
  (let ((succesful-iterations 0))
    (dotimes (iterations-so-far iteration-count)
      ;; (if (= (mod iterations-so-far 100000) 0)
      ;; 	  (format *error-output* "~a~%" iterations-so-far))
      (if (lazy-random-bfs graph from to)
	  (incf succesful-iterations)))
    (values (format nil "~f" (/ succesful-iterations iteration-count)) succesful-iterations iteration-count)))


(defun run-iterations (graph iteration-count from to)
  (let ((succesful-iterations 0))
    (dotimes (iterations-so-far iteration-count)
      ;; (if (= (mod iterations-so-far 100000) 0)
      ;; 	  (format *error-output* "~a~%" iterations-so-far))
      (if (bfs-with-gamma from to (create-gamma graph))
	  (incf succesful-iterations)))
    (values (format nil "~f" (/ succesful-iterations iteration-count)) succesful-iterations iteration-count)))
