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


(defun count-instances (search-term search-list)
	"Count the number of instances of search-term in search-list"
	(do ((lst (cdr (member search-term search-list))
			 (cdr (member search-term lst)))
			(counter 0 (1+ counter)))
		((null lst) counter)))

; Probably quite inefficient, maybe remove this function later
(defun to-list (vector)
	"Turn the vector into a list"
	(do* ((i 0 (1+ i))
			 (e (aref vector i) (aref vector i))
			 (lst (list e) (cons e lst)))
		((= i (1- (length vector))) (reverse lst))))
