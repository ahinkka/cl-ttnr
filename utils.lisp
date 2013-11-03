(in-package :ttnr)


(defun collect-nodes-by-degree (graph degree)
  (loop for node being the hash-values in (bmg:node-by-id graph)
     when
       (= (length (bmg:edges node)) degree)
     collect node))


(defun collect-neighbors (nodes)
  "Collects all the neighboring nodes of given list of nodes."
  (set-difference
   (alexandria:flatten
    (loop for node in nodes
       collect (loop for edge in (bmg:edges node)
		 collect (bmg:other-node edge node))))
   nodes :test #'eq))


(defun collect-edges (nodes)
  (remove-duplicates
   (alexandria:flatten
    (loop for node in nodes
       collect (loop for edge in (bmg:edges node)
		  collect edge)))
   :key #'bmg:id))


(defun combine-parallel-edge-weights (weights)
  (- 1 (apply #'* (mapcar #'(lambda (weight) (- 1 weight)) weights))))
