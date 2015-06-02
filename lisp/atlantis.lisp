;;;
;;; Atlantis is a framework for creating multi-user dungeon worlds.
;;; This is the Common Lisp implementation.
;;;
;;; Licensed under the terms of the MIT license.
;;; author: Daniel Vedder
;;; date: 09/05/2015
;;;

(defconstant ATLANTIS-VERSION '(0 1 1))

(load "util.lisp")
(load "game-objects.lisp")
(load "player.lisp")
(load "world.lisp")
(load "interpreter.lisp")
(load "client.lisp")


(defun development ()
	"A method to easily test whatever feature I am currently developing"
	(load-file "../ATL/lisp-test.atl")
	(let ((player (make-player :name "Bilbo"
					  :race (get-game-object 'race "Hobbit")
					  :class (get-game-object 'character-class "Burglar")
					  :place (world-starting-place *world*)
					  :strength 6 :constitution 12
					  :dexterity 19 :intelligence 14)))
		(add-game-object player)
		(set-object-attribute (get-game-object 'place (player-place player))
			'player (player-name player))
		(setf (world-game-manager *world*) (player-name player))
		(play-game (player-name player))))

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
	(format t "~&What is the IP address of the server you want to join?")
	(input-string ip)
	(while (not (= (count-instances #\. ip) 3))
		(format t "~&Not an IP address: ~A. Please reenter:" ip)
		(input-string ip))
	(format t "~&What port does the game run on?")
	(while (not (numberp (input port)))
		(format t "~&Not a number: ~A. Please reenter:" port))
	(format t "~&What is your player name?")
	(input-string name)
	(format t "~&Joining game on ~A:~A as ~A" ip port name)
	(play-game name))


(defun print-version ()
	(format t "~&Lisp Atlantis ~A.~A.~A"
		(first ATLANTIS-VERSION)
		(second ATLANTIS-VERSION)
		(third ATLANTIS-VERSION))
	(format t "~&Copyright (c) 2015 Daniel Vedder")
	(format t "~&Licensed under the terms of the MIT license.~%"))

(defun start-menu ()
	"Show the start menu and take a choice from the user"
	(dolist (line (load-text-file "banner.txt"))
		(unless (null line) (format t "~%~A" line)))
	(format t "~&~%Welcome! What do you want to do?")
	(format t "~&-> (S)tart a server")
	(format t "~&-> (J)oin a game")
	(format t "~&-> (A)bout")
	(format t "~&-> (E)xit")
	(format t "~&-> (D)evelop") ;XXX Remove later
	(input choice)
	(case choice
		('s (start-server))
		('j (join-game))
		('a (print-version)
			(when (y-or-n-p "~%Show the license text?")
				(dolist (line (load-text-file "../LICENSE"))
					(unless (null line) (format t "~%~A" line))))
			(start-menu))
		('e	(format t "~&Goodbye!") (quit))
		('d (development))
		(t (format t "~&Invalid choice!") (start-menu))))

(defun cmd-parameter (name &optional truth-value)
	"Return the value of the parameter 'name'. Or T for present if truth-value."
	(let ((argument (member name *args* :test #'equalp)))
		(if argument
			(if truth-value T (second argument))
			NIL)))

(defun print-help ()
	(print-version)
	(format t "~&~%Commandline options:")
	(let ((tab (string #\Tab)))
		(format t "~&-v --version~AShow the version number and exit" tab)
		(format t "~&-h --help~AShow this help text and exit" tab)
		(format t "~&--license~AShow the license text" tab)
		(format t "~&--server <port>~AStart a server on <port> (requires --world)" tab)
		(format t "~&--world <world-file>~AThe ATL file to load (requires --server)" tab)
		(format t "~&--client <ip>:<port>~AConnect to the game server at <ip>:<port>" tab)))

(defun parse-commandline-args ()
	;; TODO clean this up? (should give error message with unknown params)
	(when (or (cmd-parameter "--version" T) (cmd-parameter "-v" T))
			  (print-version) (quit))
	(when (or (cmd-parameter "--help" T) (cmd-parameter "-h" T))
		(print-help) (quit))
	(when (cmd-parameter "--license" T)
		(dolist (line (load-text-file "../LICENSE"))
			(unless (null line) (format t "~%~A" line)))
		(quit))
	(let ((server (cmd-parameter "--server"))
			 (world-file (cmd-parameter "--world"))
			 (client (cmd-parameter "--client")))
		(unless (or server world-file client)
			(format t "~&Invalid commandline parameter!") (quit))
		;; TODO change OR to AND, change function calls
		(if (or world-file server)
			(load-file world-file)
			(join-game))))


;; Initialize the random state (which would otherwise not be very random...)
(setf *random-state* (make-random-state t))

;; Only show the interactive menu if no commandline parameters are given
(if *args*
	(parse-commandline-args)
	(start-menu))
