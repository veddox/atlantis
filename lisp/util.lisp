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

(defmacro let-gensyms (syms &body body)
	"Gratefully copied from Paul Graham's 'On Lisp'..."
	;; I had to rename it from with-gensyms due to a naming conflict
	`(let ,(mapcar #'(lambda (s)
						 `(,s (gensym)))
			   syms)
		 ,@body))

(defmacro simple-input (var &optional (prompt ">>>"))
	"Take input from terminal and store it in var"
	`(progn
		 (format t "~&~A " ,prompt)
		 (setf ,var (read))))

(defmacro magic (var)
	"Execute typed-in Lisp code"
	`(when (eq ,var 'magic)
		 (repl)))

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
		 (magic (first (list ,@vars)))
		 (first (list ,@vars))))

(defmacro input-string (&optional (var (gensym)))
	"Read a string input line"
	`(progn
		 (format t "~&>>> ")
		 (setf ,var (read-line))
		 (magic (read-from-string ,var))
		 ,var))

(defmacro while (condition &body body)
	"An implementation of a while loop as found in other languages"
	`(do ()
		 ((not ,condition) NIL)
		 ,@body))

(defmacro != (object1 object2 &key (test 'eql))
	"A not-equals macro to save some typing"
	`(not (,test ,object1 ,object2)))

(defmacro cassoc (entry table &key (test #'eql))
	"Returns (car (cdr (assoc entry table)))"
	`(car (cdr (assoc ,entry ,table :test ,test))))

(defmacro safe-nth (index lst)
	"Return (nth index lst), or NIL if index is out of range"
	`(if (> ,index (1- (length ,lst)))
		 NIL (nth ,index ,lst)))

(defmacro safe-aref (vector index)
	"Return (aref vector index), but return NIL if out of range"
	`(if (> ,index (1- (length ,vector)))
		 NIL (aref ,vector ,index)))

(defmacro dovector ((element vector &optional (return-variable NIL)) &body body)
	"A macro analogous to dolist"
	(let-gensyms (index)
		`(do* ((,index 0 (1+ ,index))
				  (,element (safe-aref ,vector ,index)
					  (safe-aref ,vector ,index)))
			 ((= ,index (length ,vector)) ,return-variable)
			 ,@body)))

;;; FUNCTIONS

; Some of these functions are probably quite inefficient (lots of consing)


(defun call-function (function-name &rest args)
	"Save myself some quoting when calling a function from a generated symbol"
	;; Perhaps not very clean, but it works
	(eval `(,function-name ,@args)))

(defun keys (assoc-list)
	"Return a list of the keys in an association list"
	(if (null assoc-list) NIL
		(cons (car (car assoc-list)) (keys (cdr assoc-list)))))

(defun string-from-list (lst &optional (separator " - "))
	"Put all elements of lst into a single string, separated by the separator"
	(let ((str (to-string (first lst))))
		(dolist (item (cdr lst) str)
			(setf str (concatenate 'string str separator (to-string item))))))

(defun cut-string (s i)
	"Cut string s in two at index i and return the two substrings in a list"
	(do* ((c 0 (1+ c)) (letter (aref s c) (aref s c))
			(letter-list-1 NIL) (letter-list-2 NIL))
		((= c (1- (length s)))
			(list (list-to-string (append letter-list-1))
				(list-to-string (append letter-list-2 (list letter)))))
		(if (< c i) (setf letter-list-1 (append letter-list-1 (list letter)))
			(setf letter-list-2 (append letter-list-2 (list letter))))))

(defun list-to-string (char-list)
	"Convert a character list to a string"
	(let ((s (make-string (length char-list) :initial-element #\SPACE)))
		(dotimes (i (length char-list) s)
			(setf (aref s i) (nth i char-list)))))

(defun to-string (x)
	"Whatever x is, convert it into a string"
    (if (or (stringp x) (symbolp x)) (string x)
		(format NIL "~S" x)))

;; The next two functions might be simplified into one using the elt function
(defun count-instances (search-term search-list &key (test #'eql))
	"Count the number of instances of search-term in search-list"
	(let ((count 0))
		(dolist (item search-list count)
			(when (funcall test search-term item) (incf count)))))

(defun count-vector-instances (search-term search-vector &key (test #'eql))
	"Count the number of instances of search-term in search-vector"
	(let ((count 0))
		(dovector (item search-vector count)
			(when (funcall test search-term item) (incf count)))))

(defun to-list (vector)
	"Turn the vector into a list"
	(do* ((i 0 (1+ i))
			 (e (aref vector i) (aref vector i))
			 (lst (list e) (cons e lst)))
		((= i (1- (length vector))) (reverse lst))))

(defun load-text-file (file-name)
	"Load a text file into a list of strings (representing the lines)"
	;; adds two NIL to the end?
	(with-open-file (f file-name)
		(do* ((line (read-line f nil nil)
				  (read-line f nil nil))
				 (file-lines (list line) (append file-lines (list line))))
			((null line) file-lines))))

(defun build-symbol (&rest components)
	"Concatenate the passed components into a single symbol"
	;; A very useful function illustrating the power of Lisp :-)
	(let ((comps components))
		(dotimes (i (length comps))
			(when (symbolp (nth i comps))
				(setf (nth i comps) (symbol-name (nth i comps)))))
		(eval `(read-from-string (concatenate 'string ,@comps)))))

(defun repl ()
	"Launch a read-eval-print loop"
	(let ((expr (simple-input expr "lisp >")))
		;; FIXME 'done' exits Lisp
		(while (!= expr 'done)
			(if (eq expr 'help)
				(progn
					(format t "~&You are in a read-eval-print loop.")
					(format t "~&To escape, type done; to quit, type (quit)."))
			(format t "~&~S" (eval expr)))
			(simple-input expr "lisp >"))))

