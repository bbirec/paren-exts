(defpackage :paren-util
  (:use :cl :parenscript)
  (:export
   #:gen-js))

(in-package :paren-util)

(defun absolute-path (system filename)
  (merge-pathnames filename (asdf:system-source-directory system)))

(defun gen-js (system input-files &optional (filename "core.js"))
  "Compile the input files and write the output string. The base directory is
   decieded by the given system."
  (with-open-file (s (absolute-path system filename) :direction :output
		     :if-exists :supersede)
    (let ((out (loop for f in input-files collect
		       (ps-compile-file (absolute-path system f)))))
      (write-string (apply #'concatenate 'string out) s))
    
    (format t "File written in ~A~%" filename)))



