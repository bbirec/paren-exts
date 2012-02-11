
(asdf:defsystem #:paren-exts
  :serial t
  :components ((:file "paren-node")
	       (:file "paren-fb")
	       (:file "paren-jquery"))
  :depends-on (:parenscript))
	       
		      