;;;
;;; Atlantis is a framework for creating multi-user dungeon worlds.
;;; This is the Common Lisp implementation.
;;;
;;; This file provides commonly used utility functions and macros.
;;;
;;; Licensed under the terms of the MIT license.
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
		 (set-list (read) ,@vars)
		 (magic (first (list ,@vars)))))

(defmacro input-string (var)
	"Read a string input line"
	`(progn
		 (format t "~&>>> ")
		 (setf ,var (read-line))
		 (magic (read-from-string ,var))))

(defmacro simple-input (var &optional (prompt ">>>"))
	"Take input from terminal and store it in var"
	`(progn
		 (format t "~&~A " ,prompt)
		 (setf ,var (read))))

(defmacro magic (var)
	"Execute typed-in Lisp code"
	(let ((expr (gensym)))
		`(when (equalp ,var 'magic)
			 (progn (simple-input ,expr "[spell]>")
				 (eval ,expr)))))

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

(defun load-file (file-name)
	"Load a file into a list of strings (representing the lines)"
	(with-open-file (f file-name)
		(do* ((line (read-line f nil nil)
				  (read-line f nil nil))
				 (file-lines (list line) (append file-lines (list line))))
			((null line) file-lines))))


;; Intended for interactive sessions
;; Load automatically at any clisp start?
(let ((file-name 'util.lisp))
	(defun l (&optional new-file-name)
		(when new-file-name (setf file-name new-file-name))
		(load file-name)))
