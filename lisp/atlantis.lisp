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


(defun start-server ()
	"Start a new game on a server"
	(format t "~&What world file do you want to load?")
	(input-string world-file) 
    (format t "~&What port should the game run on?")
	(while (not (numberp (input port)))
		(format t "~&Not a number: ~A. Please reenter:" port))
	(format t "~&Loading file ~S on port ~A" world-file port)
	(load-file world-file)
	(break))

(defun join-game ()
	"Join a running game on the server"
	(format t "~&What is the IP address of the server you want to join?")
	(input-string ip)
	(while (not (= (count-instances #\. (to-list ip)) 3))
		(format t "~&Not an IP address: ~A. Please reenter:" ip)
		(input ip))
	(format t "~&What port does the game run on?")
	(while (not (numberp (input port)))
		(format t "~&Not a number: ~A. Please reenter:" port))
	(format t "~&Joining game on ~A:~A" (symbol-name ip) port))


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
	(let ((logo (load-text-file "banner.txt")))
		(dolist (line logo)
			(unless (null line) (format t "~%~A" line))))
	(format t "~&~%Welcome! What do you want to do?")
	(format t "~&-> (S)tart a server")
	(format t "~&-> (J)oin a game")
	(format t "~&-> (A)bout")
	(format t "~&-> (E)xit")
	(input choice)
	(cond ((equalp choice 's) (start-server))
		((equalp choice 'j) (join-game))
		((equalp choice 'a)
			(print-version) (start-menu))
		((equalp choice 'e)
			(format t "~&Goodbye!") (quit))))

;; TODO
;; (defun parse-commandline-args ()
;; 	(do* ((i 0 (1+ i)) (a (nth i *args*) (nth i *args))
;; 			 (param-functions '(("--version" (#'print-version 0))
;; 								   ("--help" (#'print-help 0))
;; 								   ("--server"


(if *args*
	(parse-commandline-args)
	(start-menu))
