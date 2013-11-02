(asdf:defsystem #:ttnr
  :depends-on (#:alexandria #:bmgraph)
  :components ((:file "ttnr")
	       (:file "utils")
	       (:file "series-parallel-optimizations")))
