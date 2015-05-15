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


;;; MACROS

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
		 (,@body)))

(defmacro != (object1 object2 &key (test 'eql))
	"A not-equals macro to save some typing"
	`(not (,test ,object1 ,object2)))

(defmacro cassoc (entry table &key (test #'eql))
	"Returns (car (cdr (assoc entry table)))"
	`(car (cdr (assoc ,entry ,table :test ,test))))

(defmacro safe-aref (vector index)
	"Return (aref vector index), but return NIL if out of range"
	`(if (> ,index (1- (length ,vector)))
		 NIL (aref ,vector ,index)))

(defmacro dovector ((element vector &optional (return-variable NIL)) &body body)
	"A macro analogous to dolist"
	(let ((index (gensym)))
		`(do* ((,index 0 (1+ ,index))
				  (,element (safe-aref ,vector ,index)
					  (safe-aref ,vector ,index)))
			 ((= ,index (length ,vector)) ,return-variable)
			 ,@body)))


;;; FUNCTIONS

; Some of these functions are probably quite inefficient (lots of consing)

(defun cut-string (s i)
	"Cut string s in two at index i and return the two substrings in a list"
	(do* ((c 0 (1+ c)) (letter (aref s c) (aref s c))
			(letter-list-1 NIL) (letter-list-2 NIL))
		((= c (1- (length s)))
			(list (to-string (append letter-list-1))
				(to-string (append letter-list-2 (list letter)))))
		(if (< c i) (setf letter-list-1 (append letter-list-1 (list letter)))
			(setf letter-list-2 (append letter-list-2 (list letter))))))

(defun to-string (char-list)
	"Convert a character list to a string"
	(let ((s (make-string (length char-list) :initial-element #\SPACE)))
		(dotimes (i (length char-list) s)
			(setf (aref s i) (nth i char-list)))))

(defun find-char (c s)
	"Find character c in string s and return the index (or NIL if non-existent)"
	(dotimes (letter (length s) NIL)
		(when (eql (char s letter) c) (return letter))))

(defun count-instances (search-term search-list)
	"Count the number of instances of search-term in search-list"
	(do ((lst (cdr (member search-term search-list))
			 (cdr (member search-term lst)))
			(counter 0 (1+ counter)))
		((null lst) counter)))

(defun count-vector-instances (search-term search-vector)
	"Count the number of instances of search-term in search-vector"
	(do ((count 0) (item-nr 0 (1+ item-nr))
			(item (aref search-vector item-nr) (aref search-vector item-nr)))
		((= item-nr (1- (length search-vector))) count)
		;TODO
	))

(defun to-list (vector)
	"Turn the vector into a list"
	(do* ((i 0 (1+ i))
			 (e (aref vector i) (aref vector i))
			 (lst (list e) (cons e lst)))
		((= i (1- (length vector))) (reverse lst))))

(defun load-file (file-name)
	"Load a file into a list of strings (representing the lines)"
	;; adds two NIL to the end?
	(with-open-file (f file-name)
		(do* ((line (read-line f nil nil)
				  (read-line f nil nil))
				 (file-lines (list line) (append file-lines (list line))))
			((null line) file-lines))))

