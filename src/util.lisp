;;;
;;; Atlantis is a framework for creating multi-user dungeon worlds.
;;; This is the Common Lisp implementation.
;;;
;;; This file provides commonly used utility functions and macros.
;;;
;;; Licensed under the terms of the GPLv3
;;; author: Daniel Vedder
;;; date: 09/05/2015
;;;


(defmacro input (var &optional (prompt ">>>"))
	"Take input from terminal and store it in var"
	`(progn
		 (format t "~&~A " ,prompt)
		 (setf ,var (read))))

(defmacro while (condition &body body)
	"An implementation of a while loop as found in other languages"
	`(do ()
		 ((not ,condition))
		 ,@body))


;TODO
(defun count-instances (search-term search-vector)
	"Count the number of instances of search-term in search-vector"
	(let (n (find-if #'(lambda (a) (equalp a search-term)) search-vector))
		(if (null n) 0
			())))
