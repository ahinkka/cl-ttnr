(asdf:defsystem #:ttnr
  :depends-on (#:alexandria #:bmgraph #:bmgraph-bestpath #:minimal-graph)
  :components ((:file "ttnr")
	       (:file "utils")
	       (:file "series-parallel-optimizations")))
