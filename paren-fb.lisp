
(defpackage :paren-fb
  (:use :cl :parenscript)

  (:export
   #:with-init-fb
   #:load-sdk))

(in-package :paren-fb)

;;;; Facebook SDK 

(defmacro+ps with-init-fb (app-id channel-url &rest body)
  `(progn 
     (setf (chain window fb-async-init) 
	   (lambda () 
	     (chain -F-B (init (create app-id ,app-id
				       channel-url ,channel-url
				       status t
				       cookie t
				       xfbml t)))
	     ,@body))))

(defmacro+ps load-sdk ()
  `((lambda (d) 
      (let ((id "facebook-jssdk"))
	(unless (chain d (get-element-by-id id))
	  (let ((js (chain d (create-element "script"))))
	    (setf (@ js id) id)
	    (setf (@ js async) t)
	    (setf (@ js src) "//connect.facebook.net/en_US/all.js")
	    (chain d (get-elements-by-tag-name "head") 0 (append-child js))))))
    document))
