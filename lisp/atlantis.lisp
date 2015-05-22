;;;
;;; Atlantis is a framework for creating multi-user dungeon worlds.
;;; This is the Common Lisp implementation.
;;;
;;; Licensed under the terms of the MIT license.
;;; author: Daniel Vedder
;;; date: 09/05/2015
;;;

(defconstant ATLANTIS-VERSION '(0 1 0))

(load 'util.lisp)
(load 'interpreter.lisp)
(load 'client.lisp)


(defun start-server ()
	"Start a new game on a server"
	(format t "~&What world file do you want to load?")
	(input-string world-file) 
    (format t "~&What port should the game run on?")
	(while (not (numberp (input port)))
		(format t "~&Not a number: ~A. Please reenter:" port))
	(format t "~&Loading file ~S on port ~A" world-file port)
	(load-file world-file))

(defun join-game ()
	"Join a running game on the server"
	;; XXX while developing...
	;; (format t "~&What is the IP address of the server you want to join?")
	;; (input-string ip)
	;; (while (not (= (count-instances #\. (to-list ip)) 3))
	;; 	(format t "~&Not an IP address: ~A. Please reenter:" ip)
	;; 	(input-string ip))
	;; (format t "~&What port does the game run on?")
	;; (while (not (numberp (input port)))
	;; 	(format t "~&Not a number: ~A. Please reenter:" port))
	(format t "~&What is your player name?")
	(input-string name)
	;; (format t "~&Joining game on ~A:~A as ~A" ip port name)
	(play-game name))


(defun print-version ()
	(format t "~&Lisp Atlantis ~A.~A.~A"
		(first ATLANTIS-VERSION)
		(second ATLANTIS-VERSION)
		(third ATLANTIS-VERSION))
	(format t "~&Copyright (c) 2015 Daniel Vedder")
	(format t "~&Licensed under the terms of the MIT license.~%"))

(defun print-help ()
	(print-version)
	(format t "~%~%Sorry, the help is not yet available!"))

(defun start-menu ()
	"Show the start menu and take a choice from the user"
	(dolist (line (load-text-file "banner.txt"))
		(unless (null line) (format t "~%~A" line)))
	(format t "~&~%Welcome! What do you want to do?")
	(format t "~&-> (S)tart a server")
	(format t "~&-> (J)oin a game")
	(format t "~&-> (A)bout")
	(format t "~&-> (E)xit")
	(input choice)
	(cond ((equalp choice 's) (start-server))
		((equalp choice 'j) (join-game))
		((equalp choice 'a)
			(print-version)
			(when (y-or-n-p "~%Show the license text?")
				(dolist (line (load-text-file "../LICENSE"))
					(unless (null line) (format t "~%~A" line))))
			(start-menu))
		((equalp choice 'e)
			(format t "~&Goodbye!") (quit))))

(defun cmd-parameter (name &optional truth-value)
	"Return the value of the parameter 'name'. Or T for present if truth-value."
	(let ((argument (member name *args* :test #'equalp)))
		(if argument
			(if truth-value T
				(second argument))
			NIL)))

(defun parse-commandline-args ()
	(when (cmd-parameter "--version" T) (print-version) (quit))
	(when (cmd-parameter "--help" T) (print-help) (quit))
	(let ((server (cmd-parameter "--server"))
			 (world-file (cmd-parameter "--world"))
			 (client (cmd-parameter "--client")))
		;(break)
		(if (or world-file server) ;TODO change OR to AND
			(load-file world-file)
			(format t "~&Sorry, the client is not yet available!"))))


(if *args*
	(parse-commandline-args)
	(start-menu))
