
(asdf:defsystem #:paren-exts
  :serial t
  :depends-on (:parenscript)
  :components ((:file "paren-node")
	       (:file "paren-fb")
	       (:file "paren-jquery")
	       (:file "paren-util")))
	       
		      
