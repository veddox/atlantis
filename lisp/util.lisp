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

(defmacro debugging (str &rest format-args)
	"If *debugging* is true, print str"
	`(when *debugging* (format t ,str ,@format-args)))

;; TODO DEPRECATED - Needs to be replaced in the current code
(defmacro simple-input (var &optional (prompt ">>>"))
	"Take input from terminal and store it in var"
	`(progn
		 (format t "~&~A " ,prompt)
		 (setf ,var (read))))

;; FIXME Remove from the code!
;; XXX Very useful for debugging, but represents a major security hole
;; when used in a network setting
(defmacro magic (var)
	"Execute typed-in Lisp code"
	`(when (eq ,var 'magic)
		 (repl)))

;; XXX potentially inefficient if called often
(defmacro set-list (value &rest var-list)
	"Set each symbol in var-list to value"
	(do* ((expr (list 'setf)) (vl var-list (cdr vl)) (var (car vl) (car vl)))
		((null vl) expr)
		(setf (cdr (last expr)) (list var))
		(setf (cdr (last expr)) (list value))))
		
(defmacro input (&rest vars)
	"Take input from terminal and store each element in a passed variable"
	;; XXX Add a prompt parameter again?
	`(progn
		 (format t "~&>>> ")
		 (set-list (read) ,@vars)
		 (magic (first (list ,@vars))) ;;TODO Remove this again
		 (first (list ,@vars))))

(defmacro input-string (&optional (var (gensym)))
	"Read a string input line"
	`(progn
		 (format t "~&>>> ")
		 (setf ,var (read-line))
		 (magic (read-from-string ,var)) ;;TODO Remove this again
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

;; Some of these functions are probably quite inefficient (lots of consing)

(defun remove-first-if (fn lst)
	"Remove the first element in a list that satisfies the given predicate"
    (cond ((null lst) NIL)
		((funcall fn (car lst))	(cdr lst))
		(T (cons (car lst) (remove-first-if fn (cdr lst))))))

(defun average (&rest numbers)
	"Compute the average of the given numbers"
	(/ (reduce #'+ numbers) (length numbers)))

(defun keys (assoc-list)
	"Return a list of the keys in an association list"
	(if (null assoc-list) NIL
		(cons (car (car assoc-list)) (keys (cdr assoc-list)))))

;; TODO change &optional to &key (and figure out why the heck that doesn't
;; work - clisp bug?), add null-filler keyword
;; TODO Add a maximum line length after which a newline is inserted
(defun string-from-list (lst &optional (separator " - "))
	"Put all elements of lst into a single string, separated by the separator"
	(cond ((null lst) "")
		((= (length lst) 1) (to-string (car lst)))
		(T (concatenate 'string (to-string (first lst)) (to-string separator)
			(string-from-list (cdr lst) separator)))))

(defun split-string (str separator)
	"Split the string up into a list of strings along the separator character"
	(cond ((equalp str (to-string separator)) NIL)
		((zerop (count-instances separator str)) (list str))
		(T (let ((split-elt (cut-string str (position separator str))))
			   (cons (first split-elt)
				   (split-string (second (cut-string (second split-elt) 1))
					   separator))))))

(defun cut-string (s i)
	"Cut string s in two at index i and return the two substrings in a list"
	(if (or (minusp i) (> i (length s))) s
		(let ((s1 (make-string i)) (s2 (make-string (- (length s) i))))
			(dotimes (c (length s) (list s1 s2))
				(if (> i c)
					(setf (aref s1 c) (aref s c))
					(setf (aref s2 (- c i)) (aref s c)))))))

(defun list-to-string (char-list)
	"Convert a character list to a string"
	(let ((s (make-string (length char-list) :initial-element #\SPACE)))
		(dotimes (i (length char-list) s)
			(setf (aref s i) (nth i char-list)))))

(defun trim-whitespace (s)
	"Trim off spaces and tabs before and after string s"
	(string-trim '(#\space #\tab) s))

(defun to-string (x)
	"Whatever x is, convert it into a string"
	(cond ((stringp x) x)
		((or (symbolp x) (characterp x)) (string x))
		(t (format NIL "~S" x))))

(defun extract-elements (str)
	"Extract all Lisp elements (strings, symbols, numbers, etc.) from str"
	(when (null str) (return-from extract-elements))
	(multiple-value-bind (next-element i) (read-from-string str nil)
		(if (null next-element) NIL
			(cons next-element
				(extract-elements (second (cut-string str i)))))))

(defun count-instances (search-term search-sequence &key (test #'eql))
	"Count the number of instances of search-term in search-sequence"
	(let ((count 0))
		(dotimes (i (length search-sequence) count)
			(when (funcall test search-term (elt search-sequence i))
				(incf count)))))

(defun set-p (lst)
	"Is lst a set (i.e. no elements occur more than once)?"
	(cond ((null lst) T)
		((member (car lst) (cdr lst)) NIL)
		(T (set-p (cdr lst)))))

(defun to-list (vector &optional (next-elt 0))
	"Turn the vector into a list"
	(if (= next-elt (1- (length vector))) NIL
		(cons (aref vector next-elt) (to-list vector (1+ next-elt)))))

(defun cut-list (l i)
	"Cut list l in two at index i and return the two sublists in a list"
	(if (or (< i 1) (> i (length l))) l
		(do* ((lst2 l (cdr lst2))
				 (lst1 (list (car lst2)) (append lst1 (list (car lst2)))))
			((= i (length lst1)) (list lst1 (cdr lst2))))))

(defun random-elt (seq)
	"Return a random element of this sequence"
	(elt seq (random (length seq))))

(defun load-text-file (file-name)
	"Load a text file into a list of strings (representing the lines)"
	(with-open-file (f file-name)
		(do* ((line (read-line f nil nil)
				  (read-line f nil nil))
				 (file-lines (list line) (append file-lines (list line))))
			((null line) file-lines))))

(defun print-text-file (file-name)
	"Print out the contents of this text file"
	(dolist (line (load-text-file file-name))
		(unless (null line) (format t "~%~A" line))))

(defun write-to-file (text filename &optional (append NIL))
	"Write text (a string or list of strings) to the specified file"
	(let ((text-list (if (listp text) text (list text)))
			 (f (if append
					(open filename :direction :output :if-exists :append
						:if-does-not-exist :create)
					(open filename :direction :output))))
		(dolist (line text-list)
			(format f "~&~A~&" line))
		(close f)))

(defun build-symbol (&rest components)
	"Concatenate the passed components into a single symbol"
	(read-from-string (string-from-list components "")))

(defun make-list-function (container-type &optional (add-s t))
	"Return a function to return a list of the names of all objects of the
specified type in the container struct"
	#'(lambda (object-type container)
		  (let* ((get-objects (symbol-function
								  (build-symbol container-type "-"
									  object-type (if add-s "s" ""))))
					(get-object-name (symbol-function
										 (build-symbol object-type "-name")))
					(objects (funcall get-objects container)) (name-list NIL))
			  (dolist (o objects name-list)
				  (when (stringp o) (return objects))
				  (setf name-list
					  (cons (funcall get-object-name o) name-list))))))

(defun choose-number-option (option-list)
	"The user chooses one out of a list of options, the index is returned"
	(dotimes (i (length option-list))
		(format t "~&~S) ~A" (1+ i) (nth i option-list)))
	(simple-input choice)
	(while (or (not (numberp choice)) (< choice 1)
			   (> choice (length option-list)))
		(format t "~&Invalid choice! Please choose again:")
		(simple-input choice))
	(1- choice))

(defun choose-option (option-list)
	"Like choose-number-option, but return the value of the choice"
	;; Basically just a utility wrapper
	(nth (choose-number-option option-list) option-list))

(defun lisp-ed ()
	"A very, very basic line editor for inputing several lines of text."
	(flet ((text-input ()
			   (format t "~&$ ")
			   (do* ((line (read-line) (read-line))
						(text line (string-from-list
									   (list text line) #\newline)))
				   ((equalp line ".") (first (cut-string text
												 (- (length text) 2))))
				   (format t "$ "))))
		(setf help-msg "
Input your text below. When you are done, finish with a line that contains only
a single fullstop. If you make a mistake, you can still edit your text later.")
		(format t help-msg)
		(setf text (text-input))
		(while (not (y-or-n-p "~&Save and exit?"))
			(when (y-or-n-p "~&Cancel and exit?")
				(return-from lisp-ed))
			(format t "~&Please reenter your text:")
			(setf text (text-input)))
		text))

(defun clear-screen ()
	"Clear the screen in an OS-dependent manner"
	;; NOTE: only works with CLISP! (ext:shell function used)
	;; TODO Make sure we're on clisp, otherwise do nothing
	(cond ((member ':unix *features*) (ext:shell "clear"))
		((member ':win32 *features*) (ext:shell "cls"))
		(t (debugging "~&clear-screen is not supported on this operating system!"))))

(defun repl ()
	"Launch a read-eval-print loop"
	(let ((expr (simple-input expr "lisp >")))
		(while (!= expr 'done)
			(if (eq expr 'help)
				(progn
					(format t "~&You are in a read-eval-print loop.")
					(format t "~&To escape, type done; to quit, type (quit)."))
			(format t "~&~S" (eval expr)))
			(simple-input expr "lisp >"))))

;; XXX Interesting phenomenon of repl (security bug?):
;; Enter two Lisp expressions that have not had a value assigned to them in the
;; current session (e.g. 'foo ls'). The first will cause the interpreter to
;; exit with an error. The second, however, is still printed to stdout (which is
;; now a shell), followed by a newline. If the symbol represents a valid shell
;; command, it is therefore executed. ('ls' in the example above.)

