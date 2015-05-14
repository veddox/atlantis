;;;
;;; Atlantis is a framework for creating multi-user dungeon worlds.
;;; This is the Common Lisp implementation.
;;;
;;; Licensed under the terms of the MIT license.
;;; author: Daniel Vedder
;;; date: 09/05/2015
;;;

(defconstant ATLANTIS-VERSION '(0 0 1))

(load 'util.lisp)


(defun start-server ()
	"Start a new game on a server"
	(format t "~&What world file do you want to load?")
	(input-string world-file) 
    (format t "~&What port should the game run on?")
	(while (not (numberp (input port)))
		(format t "~&Not a number: ~A. Please reenter:" port))
	(format t "~&Loading file ~A on port ~A" (string world-file) port))

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
	(format t "~&Copyright (c)2015 Daniel Vedder")
	(format t "~&Licensed under the terms of the MIT license."))

(defun start-menu ()
	"Show the start menu and take a choice from the user"
	(with-open-file (logo "banner.txt")
		(do ((line (read-line logo nil nil)
				 (read-line logo nil nil)))
			((null line))
			(format t "~&~A" line)))
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

(start-menu)
