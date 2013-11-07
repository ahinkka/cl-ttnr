(require 'asdf)
(push (truename ".") asdf:*central-registry*)

(handler-case
    (asdf:oos 'asdf:load-op 'ttnr :verbose nil)
  (asdf/find-component:missing-dependency (e)
    (ql:quickload 'ttnr)))


(defvar *iterations* 100000)
(defvar *from* "Protein_UniProt:P04083")
(defvar *to* "Protein_UniProt:P07355")

(defun ttnr-once ()
  (let*
      ((start (get-internal-run-time))
       (graph (bmg:read-graph #P"data.bmg"))
       (from (gethash *from* (bmg:node-by-name graph)))
       (to (gethash *to* (bmg:node-by-name graph)))
       (best-paths nil)
       (graph-read (get-internal-run-time))
       (graph-simplified nil)
       (paths-calculated nil)
       (ttnr-estimated nil)

       (mg nil)
       (best-path-nodes))

    (ttnr:remove-dangling-nodes graph (list from to))
    (ttnr:remove-serial-nodes graph (list from to))
    (ttnr:remove-parallel-edges graph)
    (ttnr:remove-serial-nodes graph (list from to))
    (ttnr:remove-parallel-edges graph)
    (ttnr:remove-dangling-nodes graph (list from to))

    (setf best-path-nodes (bmg-bestpath:collect-nodes (bmg-bestpath:best-distinct-paths from to)))
    (setf mg (ttnr:bmgraph-to-minimal-graph graph))
    (setf mg (mg:order-edges-by-weight mg))
    (setf mg (mg:order-edges-by-preferred-nodes
	      mg (mapcar #'bmg:id best-path-nodes)))
    (setf mg (mg:order-graph-by-preferred-nodes
	      mg (mapcar #'bmg:id best-path-nodes)))
    ;; (time (ttnr:run-iterations #'ttnr:bfs-with-minimal-graph 100000 (bmg:id start-node) (bmg:id goal-node) mg)))

    (setf graph-simplified (get-internal-run-time))
    (format *error-output* "Nodes left: ~a~%" (hash-table-count (bmg:node-by-name graph)))

    (setf best-paths (bmg-bestpath:best-distinct-paths from to))
    (setf paths-calculated (get-internal-run-time))

    ;; (format t "~a~%"
    ;; 	    (ttnr:run-iterations #'ttnr:bfs-with-hashes *iterations* from to best-paths))
    (format t "~a~%"
	    (ttnr:run-iterations #'ttnr:bfs-with-minimal-graph *iterations*
				 (bmg:id from) (bmg:id to) mg))
    (setf ttnr-estimated (get-internal-run-time))

    (values
     (- graph-read start)
     (- graph-simplified start)
     (- paths-calculated graph-simplified)
     (- ttnr-estimated paths-calculated)
     (- ttnr-estimated start))))


(defvar *times* nil)
(dotimes (i 10)
  (multiple-value-bind (read simplified calculated monte-carlo-time) (ttnr-once)
    (setf *times*
	  (cons (list read simplified calculated monte-carlo-time)
		*times*))))

(dolist (time *times*)
  (format t "~a~%" time))


(flet ((last-value (list) (car (last list))))
  (format t "Mean (Monte-Carlo): ~$~%Median (Monte-Carlo): ~$~%Standard deviation (Monte-Carlo): ~$~%"
	  (alexandria:mean (mapcar #'last-value *times*))
	  (alexandria:median (mapcar #'last-value *times*))
	  (alexandria:standard-deviation (mapcar #'last-value *times*))))
