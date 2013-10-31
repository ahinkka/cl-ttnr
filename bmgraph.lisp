(defpackage #:bmgraph
  (:use #:common-lisp)
  (:nicknames #:bmg)
  (:export #:read-graph
	   #:node-by-name
	   #:node-by-id

	   ; Classes
	   #:edge

	   ; Accessors
	   #:name
	   #:id
	   #:goodness
	   #:edges

	   ; Other functions
	   #:add-edge
	   #:make-node
	   #:make-edge
	   #:remove-node
	   #:remove-edge
	   #:other-node
	   #:node-equal))

(in-package :bmgraph)


;; Class definitions
(defclass graph ()
  ((max-node-id  :initform 0                              :accessor max-node-id)
   (max-edge-id  :initform 0                              :accessor max-edge-id)
   (node-by-id   :initform (make-hash-table :test 'eq)    :accessor node-by-id)
   (node-by-name :initform (make-hash-table :test 'equal) :accessor node-by-name)))

(defclass edge ()
  ((id       :initarg :id       :accessor id)
   (from     :initarg :from     :accessor from)
   (to       :initarg :to       :accessor to)
   (goodness :initarg :goodness :accessor goodness)))

(defclass node ()
  ((id    :initarg :id    :accessor id)
   (name  :initarg :name  :accessor name)
   (edges :initarg :edges :accessor edges :initform '())))


;; Common graph manipulation
(defmethod add-edge ((object node) edge)
  (setf (edges object) (cons edge (edges object))))

(defmethod make-node ((object graph) name)
  (make-instance 'node
		 :name name
		 :id (incf (max-node-id object))))

(defmethod make-edge ((object graph) from to goodness)
  (make-instance 'edge
		 :from from
		 :to to
		 :goodness goodness
		 :id (incf (max-edge-id object))))

(defmethod remove-node ((object graph) node)
  (remhash (name node) (node-by-name object))
  (remhash (id node) (node-by-id object))
  (dolist (edge (edges node))
    (remove-edge edge)))

(defmethod remove-edge ((object edge))
  (let ((from (from object))
	(to (to object)))
    (setf (edges from) (remove-if #'(lambda (edge) (eq (id object) (id edge))) (edges from)))
    (setf (edges to) (remove-if #'(lambda (edge) (eq (id object) (id edge))) (edges to)))))


;; Utility functions
(defmethod other-node ((object edge) node)
  (if (eq (id node) (id (to object)))
      (from object)
      (to object)))

(defmethod node-equal ((object node) node)
  (= (id object) (id node)))

(defmethod print-object ((object edge) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~a (~a -[~d]-> ~a)" (id object) (from object) (goodness object) (to object))))

(defmethod print-object ((object node) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (name id) object
      (format stream "~a" id))))

(defun pick-goodness (list)
  (dolist (item list result)
    (when (string=
	   (nth 0 (cl-ppcre:split "=" item))
	   "goodness")
      (setf result (read-from-string
		    (nth 1 (cl-ppcre:split "=" item)))))))


;; Beef
(defun read-graph (bmg-path)
  "Reads a BMGraph format graph into memory."
  (let* ((graph (make-instance 'graph))
	 (node-id (max-node-id graph))
	 (edge-id (max-edge-id graph))
	 (node-by-name (node-by-name graph))
	 (node-by-id (node-by-id graph)))
    (with-open-stream (stream (open bmg-path))
      (loop for line = (read-line stream nil)
	 while line do
	   (when (not (char= (elt line 0) #\#)) ; skip comments
	     (let ((members (cl-ppcre:split " " line)))
	       (cond ((= (length members) 1) ; node introduction
		      (if (not (gethash (car members) node-by-name))
			  (setf (gethash (car members) node-by-name)
				(make-instance 'node
					       :name (car members)
					       :id (incf node-id)))))
		     ((> (length members) 2)
		      (unless (gethash (nth 0 members) node-by-name)
			(setf (gethash (nth 0 members) node-by-name)
			      (make-instance 'node
					     :name (nth 0 members)
					     :id (incf node-id))))
		      (unless (gethash (nth 1 members) node-by-name)
			(setf (gethash (nth 1 members) node-by-name)
			      (make-instance 'node
					     :name (nth 1 members)
					     :id (incf node-id))))
		      (let ((object (make-instance 'edge
						   :from (gethash (nth 0 members) node-by-name)
						   :to (gethash (nth 1 members) node-by-name)
						   :goodness (pick-goodness members)
						   :id (incf edge-id))))
			(add-edge (from object) object)
			(add-edge (to object) object)))))))
      
      (loop for value being the hash-values of node-by-name do
	   (setf (gethash (id value) node-by-id) value))
      graph)
    (with-slots (max-node-id max-edge-id) graph
      (setf max-node-id node-id)
      (setf max-edge-id edge-id))
    graph))
