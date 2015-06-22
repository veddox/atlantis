;;;
;;; Atlantis is a framework for creating multi-user dungeon worlds.
;;; This is the Common Lisp implementation.
;;;
;;; Licensed under the terms of the MIT license.
;;; author: Daniel Vedder
;;; date: 09/05/2015
;;;

(defconstant ATLANTIS-VERSION '(0 1 3))

(load "util.lisp")
(load "game-objects.lisp")
(load "player.lisp")
(load "world.lisp")
(load "interpreter.lisp")
(load "ui.lisp")
(load "creator.lisp")

(defvar *debugging* NIL)

(defun development ()
	"A method to easily test whatever feature I am currently developing"
	(setf *debugging* T)
	(load-file "../ATL/lisp-test.atl")
	(let ((player (make-player :name "Bilbo"
					  :race "Hobbit" :class "Burglar"
					  :place (world-starting-place *world*)
					  :strength 6 :constitution 12
					  :dexterity 19 :intelligence 14
					  :game-admin T)))
		(add-game-object player)
		(set-object-attribute (get-game-object 'place (player-place player))
			'player (player-name player))
		(play-game (player-name player))))

(defun not-available ()
	"Before I tackle networking..."
	(format t "~&Sorry, multiplayer is currently not supported!")
	(format t "~&Please press ENTER")
	(y-or-n-p "~&OK?")
	(start-menu))

(defun start-server ()
	"Start a new game on a server"
	;; TODO Doesn't actually start a server yet
	(format t "~&What world file do you want to load?")
	(input-string world-file) 
	(format t "~&What port should the game run on?")
	(while (not (numberp (input port)))
		(format t "~&Not a number: ~A. Please reenter:" port))
	(debugging "~&Loading file ~S on port ~A" world-file port)
	(load-file world-file))

(defun join-game ()
	"Join a running game on the server"
	(format t "~&What is the IP address of the server you want to join?")
	(input-string server-ip)
	(while (not (= (count-instances #\. server-ip) 3))
		(format t "~&Not an IP address: ~A. Please reenter:" server-ip)
		(input-string server-ip))
	;(setf (cassoc ip *server-address*) server-ip)
	(format t "~&What port does the game run on?")
	(while (not (numberp (input server-port)))
		(format t "~&Not a number: ~A. Please reenter:" server-port))
	;(setf (cassoc port *server-address*) server-port)
	(format t "~&What is your player name?")
	(input-string name)
	(debugging "~&Joining game on ~A:~A as ~A" server-ip server-port name)
	(play-game name))


(defun single-player ()
	"Start a single-player game"
	(format t "~&What do you want to do?")
	(setf options '("Start a new game" "Load a game" "Back to menu"))
	(case (choose-number-option options)
		(0 (format t "~&What world file do you want to load?")
			(input-string world-file)
			(format t "~&What is your name?")
			(input-string name)
			(load-file world-file)
			(play-game name))
		(1 (format t "~&What game file do you want to load?")
			(input-string game)
			(format t "~&What is your name?")
			(input-string name)
			(load-game game)
			(play-game name))
		(2 (start-menu))))

(defun print-version ()
	(format t "~&Atlantis ~A.~A.~A"
		(first ATLANTIS-VERSION)
		(second ATLANTIS-VERSION)
		(third ATLANTIS-VERSION))
	(format t "~&Copyright (c) 2015 Daniel Vedder")
	(format t "~&Licensed under the terms of the MIT license.~%"))

(defun start-menu ()
	"Show the start menu and take a choice from the user"
	(clear-screen)
	(print-text-file "banner.txt")
	(format t "~&~%Welcome! What do you want to do?")
	(setf options '("Start a server" "Join a game" "Play single-player"
					   "Create worlds" "Develop" "About" "Exit"))
	(case (choose-number-option options)
		(0 (not-available))
		(1 (not-available))
		(2 (single-player))
		(3 (world-creator))
		(4 (development))
		(5 (print-version)
			(when (y-or-n-p "Show the license text?")
				(print-text-file "../LICENSE"))
			(start-menu))
		(6 (format t "~&Goodbye!")
			(quit))))
 
(defun cmd-parameter (name &optional truth-value)
	"Return the value of the parameter 'name'. Or T for present if truth-value."
	(let ((argument (member name *args* :test #'equalp)))
		(if argument
			(if truth-value T (second argument))
			NIL)))

(defun print-help ()
	(print-version)
	(setf help-text "
Commandline options:
-v --version          Show the version number and exit
-h --help             Show this help text and exit
--license             Show the license text
--debugging           Switch on debug mode
--single-player       Start a single-player game
--server <port>       Start a server on <port> (requires --world)
--world <world-file>  The ATL file to load (requires --server)
--client <ip>:<port>  Connect to the game server at <ip>:<port>")
	(format t "~A" help-text))

(defun parse-commandline-args ()
	;; TODO clean this up? (should give error message with unknown params)
	(cond ((or (cmd-parameter "--version" T) (cmd-parameter "-v" T))
			  (print-version) (quit))
		((or (cmd-parameter "--help" T) (cmd-parameter "-h" T))
			(print-help) (quit))
		((cmd-parameter "--license" T)
			(dolist (line (load-text-file "../LICENSE"))
				(unless (null line) (format t "~%~A" line)))
			(quit))
		((cmd-parameter "--debugging")
			(setf *debugging* T))
		((cmd-parameter "--single-player" T)
			(single-player)))
	(let ((server (cmd-parameter "--server"))
			 (world-file (cmd-parameter "--world"))
			 (client (cmd-parameter "--client")))
		(unless (or server world-file client)
			(format t "~&Invalid commandline parameter!") (quit))
		(if (and world-file server)
			(load-file world-file)
			(join-game))))


;; Initialize the random state (which would otherwise not be very random...)
(setf *random-state* (make-random-state t))

;; Only show the interactive menu if no commandline parameters are given
(if *args*
	(parse-commandline-args)
	(start-menu))
