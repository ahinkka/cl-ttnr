(defpackage #:bmgraph-bestpath
  (:use #:common-lisp)
  (:nicknames #:bmg-bestpath)
  (:export #:best-distinct-paths #:edges #:collect-nodes))

(in-package :bmgraph-bestpath)


(defclass path ()
  ((edges :accessor edges :initform '())))

(defclass path-edge (bmg:edge) ())

(defmethod clone ((object path))
  (let ((new-instance (make-instance 'path)))
    (setf (edges new-instance) (copy-list (edges object)))
    new-instance))

(defmethod goodness ((object path))
  (let ((goodness 0.0))
    (declare (type single-float goodness))
    (dolist (edge (edges object))
      (incf goodness (* -1 (log (bmg:goodness edge)))))
    goodness))

(defmethod make-path-edge ((object bmg:edge))
  (with-slots (bmg:id bmg::from bmg::to bmg::goodness) object
    (make-instance 'path-edge :id bmg:id :from bmg::from :to bmg::to :goodness bmg::goodness)))

(defmethod swap-direction ((object path-edge))
  (let ((to (slot-value object 'bmg::to)))
    (setf (slot-value object 'bmg::to) (slot-value object 'bmg::from))
    (setf (slot-value object 'bmg::from) to)))

(defmethod add-edge ((object path) path-edge)
  (with-slots (length edges) object
      (if edges
          (nconc edges (list path-edge))
          (setf edges (cons path-edge edges)))))


(defun reorder-edges (graph preferred-nodes)
  (loop for node being the hash-values of (bmg:node-by-id graph) do
       (setf (bmg:edges node)
	     (sort (bmg:edges node)
		   #'(lambda (x y)
		       (let
			   ((x-preferred (member x preferred-edges :test #'eq))
			    (y-preferred (member y preferred-edges :test #'eq)))
			 (cond
			   ((and x-preferred (not y-preferred)) t)
			   ((and (not x-preferred) y-preferred) nil)
			   (t t))))))))


(defun collect-nodes (paths)
  (let
      ((nodes nil))
    (dolist (path paths)
      (dolist (edge (edges path))
	(setf nodes
	      (adjoin (slot-value edge 'bmg::from)
		      (adjoin (slot-value edge 'bmg::to) nodes :test #'eq)
		      :test #'eq))))
    nodes))


(defun best-path (start-node end-node &optional banned-edges)
  (let ((best-path-to-node (make-hash-table :test #'equal))
        (best-paths (pileup:make-heap #'< :key #'goodness)))
    (pileup:heap-insert (make-instance 'path) best-paths)
    (loop do
         (let ((current-path (pileup:heap-pop best-paths))
                (current-node nil))

           (if (= (length (edges current-path)) 0)
               (setf current-node start-node)
               (setf current-node (bmg::to (car (last (edges current-path))))))

           (if (eq current-node end-node)
               (return-from best-path current-path))
           (dolist (edge (bmg:edges current-node))
             (if (not (member edge banned-edges :test #'eq))
                 (let* ((other-node (bmg:other-node edge current-node))
                        (other-node-id (bmg:id other-node))
                        (current-best-path-to-other (gethash other-node-id best-path-to-node))
                        (path-to-other-node-goodness (* (goodness current-path) (bmg:goodness edge))))
                   (if (or (not current-best-path-to-other)
                           (< path-to-other-node-goodness (goodness current-best-path-to-other)))
	               (let ((new-best-path-to-other (clone current-path))
                             (path-edge (make-path-edge edge)))
                         (if (not (eq (bmg::to path-edge) other-node))
                             (swap-direction path-edge))
                         (add-edge new-best-path-to-other path-edge)
                         (setf (gethash other-node-id best-path-to-node) new-best-path-to-other)
                         (pileup:heap-insert new-best-path-to-other best-paths)))))))
         (if (pileup:heap-empty-p best-paths)
             (error "This should never happen.")))))


(defun best-distinct-paths (start-node end-node &optional (number 3))
  (let ((banned-edges nil)
	(paths nil))
    (dotimes (i number)
      (let ((best-path (if (not banned-edges)
                           (best-path start-node end-node)
                           (best-path start-node end-node banned-edges))))
	(push best-path paths)
	(dolist (edge (edges best-path))
          (push edge banned-edges))))
    (nreverse paths)))