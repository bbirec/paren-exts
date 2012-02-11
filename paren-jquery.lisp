
(defpackage :paren-jquery
  (:use :cl :parenscript)

  (:export
   #:$ajax
   #:reload-page
   #:with-elements
   #:$parse-json
   #:minusp))

(in-package :paren-jquery)


(defpsmacro $ajax (param done-func)
  `(chain $ (ajax ,param) (done ,done-func)))

(defpsmacro reload-page ()
  `(chain history (go 0)))

(defpsmacro with-elements (selector-form &rest body)
  "Selector form is (var selector)"
  `(let 
       ,(loop for form in selector-form collect 
	    (destructuring-bind (v s) form 
	      `(,v (chain ($ ,s)))))
     ,@body))
       
(defpsmacro $parse-json (&rest rest)
  `(chain $ (parse-J-S-O-N ,@rest)))


(defpsmacro minusp (number)
  `(< ,number 0))
