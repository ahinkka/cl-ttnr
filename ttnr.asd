(asdf:defsystem #:ttnr
  :depends-on (#:alexandria #:trees #:bmgraph #:bmgraph-bestpath #:minimal-graph)
  :components ((:file "ttnr")
	       (:file "utils")
	       (:file "series-parallel-optimizations")))
