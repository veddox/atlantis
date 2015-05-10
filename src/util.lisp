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


; potentially inefficient if called often
(defmacro set-list (value &rest var-list)
	"Set each symbol in var-list to value"
	(do* ((expr (list 'setf)) (vl var-list (cdr vl)) (var (car vl) (car vl)))
		((null vl) expr)
		(setf (cdr (last expr)) (list var))
		(setf (cdr (last expr)) (list value))))
		
(defmacro input (&rest vars)
	"Take input from terminal and store each element in a passed variable"
	; Add a prompt parameter again?
	`(progn
		 (format t "~&>>> ")
		 (set-list (read) ,@vars)))

(defmacro simple-input (var &optional (prompt ">>>"))
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

; Use this to develop the input macro further
(defun commandline ()
	"This function takes in a command together with its argument
from the commandline"
	(format t "~&>>> ")
	(setf command (read))
	(setf argument (read))
	(format t "~&Command = ~A~%Argument = ~A" command argument))
