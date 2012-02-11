
(defpackage :paren-node
  (:use :cl :parenscript)
  (:export 
   ;; Utils 
   #:create-dict
   #:concat
   #:parse-json
   #:stringify-json

   
   ;; Node.js
   #:log-msg
   
   ;; Socket.io
   #:call
   #:on-event
   #:on-connection
   #:emit

   ;;Redis
   #:with-redis
   #:let-redis-1
   #:let-redis**
   #:print-redis
   #:do-redis
   #:list-redis*))

(in-package :paren-node)

;; Utils

(defmacro+ps create-dict (keys values)
  (let ((dict-sym (gensym)))
    `(let ((,dict-sym (create)))
       (loop for k in ,keys 
	  for v in ,values do
	  (setf (getprop ,dict-sym k) v))
       ,dict-sym)))

(defmacro+ps concat (&rest rest)
  `(concatenate 'string ,@rest))

(defmacro+ps parse-json (str)
  `(chain -J-S-O-N (parse ,str)))

(defmacro+ps stringify-json (obj)
  `(chain -J-S-O-N (stringify ,obj)))


;; Node.js

(defmacro+ps log-msg (&rest msg)
  `(chain console (log (concatenate 'string ,@msg))))


;; Socket.io

(defmacro+ps call (obj func &rest rest)
  `(chain ,obj (,func ,@rest)))

(defmacro+ps on-event (obj event &rest rest)
  `(call ,obj on ,event ,@rest))

(defmacro+ps on-connection (func)
  `(on-event (@ io sockets) "connection" ,func))

(defmacro+ps emit (socket name obj)
  `(call ,socket emit ,name ,obj))


;; Redis
(defmacro+ps with-redis (&rest body)
  `(progn
     ,@(loop for s in body collect
		(cons 'call (cons '*client* s)))))

(defun exp-p (name)
  (let ((l (length name)))
    (if (< l 4)
	nil
	(equal "-EXP" (string-upcase (subseq name (- l 4)))))))


(defmacro+ps let-redis-1 (binding &body body)
  (cons 'with-redis
	(list (append 
	       ;; Expand macro first if the command is ending with -exp
	       (let ((s (cadr binding)))
		 (if (exp-p (symbol-name (car s)))
		     (macroexpand s)
		     s))

	       `(#'(lambda (err ,(car binding))
		     #+nil
		     (if err (log-msg "Error:" err) (log-msg "OK:" ,(car binding)))
		     ,@body))))))

(defmacro+ps let-redis** (bindings &body body)
  ;; Send all of command first and execute body when all of results are received.
  (let ((function-syms 
	 (loop for i from 0 to (length bindings) collect (gensym)))
	(sym-final (gensym))
	(sym-count (gensym)))
	  
    `(let ((,sym-count 0)
	   ,@(loop for b in bindings collect `(,(car b) nil)))
       (labels ((,sym-final () ,@body)
		,@(loop for b in bindings 
		     for f in function-syms collect 
		     `(,f (reply)
			  (setf ,sym-count (1+ ,sym-count))
			  (setf ,(car b) reply)
			  (when (eql ,sym-count ,(length bindings)) (,sym-final)))))
	 ,@(loop for b in bindings 
	      for f in function-syms collect
	      `(let-redis-1 ,b (,f ,(car b))))))))


(defmacro+ps print-redis (s)
  `(let-redis-1 (output ,s) (log-msg output)))

(defmacro+ps do-redis ((var list) &body body)
  `(dolist (,var ,list) (with-redis ,@body)))



(defmacro+ps list-redis* ((var list) (v exp) &body body)
  (let ((sym-var (gensym))
	(sym-count (gensym))
	(sym-length (gensym))
	(sym-results (gensym))
	(sym-final (gensym))
	(sym-handler (gensym))
	(sym-idx (gensym)))
  `(let ((,sym-count 0)
	 (,sym-length (length ,list))
	 (,sym-results (make-array)))
     (labels ((,sym-final () (let ((,v ,sym-results)) ,@body))
	      (,sym-handler (idx result)
		;; Debug code
		#+nil
		(log-msg "Handler: " idx " => " result)
		(setf ,sym-count (1+ ,sym-count))
		(setf (aref ,sym-results idx) result)
		(when (eql ,sym-count ,sym-length) (,sym-final))))
       (loop for i in ,list
	  for idx from 0 do
	    (let ((,var i)
		  (,sym-idx idx)) ; Index should be saved
	      (let-redis-1 (,sym-var ,exp) (,sym-handler ,sym-idx ,sym-var))))))))
