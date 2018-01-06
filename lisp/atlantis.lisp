;;;
;;; Atlantis is a framework for creating text-adventure worlds.
;;; This is the Common Lisp implementation.
;;;
;;; Licensed under the terms of the GNU GPLv3.
;;; author: Daniel Vedder
;;; date: 09/05/2015
;;;

(defconstant ATLANTIS-VERSION '(0 2 6))

(load "util.lisp")
(load "game-objects.lisp")
(load "player.lisp")
(load "world.lisp")
(load "interpreter.lisp")
(load "ui.lisp")

(defvar *debugging* NIL)

;; Game worlds must be registered in this list with a display name
;; and a path relative to ../ATL
(defvar *games* '(("Winnie the Pooh" "Pooh/pooh.atl")
					 ("Lugwey" "Lugwey/lugwey.atl")))

(defun print-version ()
	(format t "~&Atlantis ~A.~A.~A"
		(first ATLANTIS-VERSION)
		(second ATLANTIS-VERSION)
		(third ATLANTIS-VERSION))
	(format t "~&Copyright (c) 2015-2018 Daniel Vedder")
	(format t "~&Licensed under the terms of the GNU GPLv3.")
	(format t "~&www.github.com/atlantis~%"))

(defun start-menu ()
	"Show the start menu and take a choice from the user"
	(clear-screen)
	;; Display the Atlantis ASCII-banner and a message of the day
	;; (the latter is optional and intended for use on the server)
	(print-text-file "banner.txt")
	(print-text-file "../motd.txt")
	;; The actual menu
	(format t "~&~%Greetings! Please choose an option:")
	(case (choose-number-option '("Start playing" "Show player list"
									 "Read the help file"
									 "About Atlantis" "Exit"))
		(0 (start-playing))
		(1 (let ((users (mapcar #'pathname-name (directory "../saves/*"))))
			   (format t "~&~A" (string-from-list users :sep #\Newline))
			   (format t "~&~%Currently, there are ~S players."
				   (length users)))
			(format t "~&~%Please press ENTER")
			(read-line)
			(start-menu))
		(2 (clear-screen)
			(pager "../doc/PLAYING" T)
			(start-menu))
		(3 (print-version)
			(when (y-or-n-p "~&~%Show the license text?")
				(pager "../doc/COPYING" T))
			(start-menu))
		(4 (format t "~&Goodbye!")
			(quit))))

(defun start-playing ()
	"Ask the player for his name, then start a new game or load a saved game."
	(setf player-name "")
	(while (zerop (length player-name))
		(format t "~&What is your name? ")
		(setf player-name (read-line)))
	(setf (world-player-name *world*) player-name)
	(if (member player-name (mapcar #'pathname-name (directory "../saves/*"))
			:test #'equalp)
		(progn (format t "~&Welcome back, ~A! What do you want to do?" player-name)
			(case (choose-number-option
					  '("Load your previous game" "Start a new game" "Go back"))
				(0 (load-game player-name)
					(play-game))
				(1 (new-game player-name T))
				(2 (start-menu))))
		(new-game player-name)))
	
(defun new-game (player-name &optional no-greet)
	"Set up a new game."
	;; let the player choose one of the game worlds
	(if no-greet
		(format t "~&Which world do you want to play?")
		(format t "~&Welcome, ~A! Which world do you want to play?" player-name))
	(let ((world (choose-option (append (keys *games*) '("Back")))))
		(if (equalp world "Back") (start-menu)
			(setf world-file (cassoc world *games*)))
		(setf world-file (concatenate 'string "../ATL/" world-file))
		(load-file world-file)
		;; let the player choose a character
		(let* ((chars (append (list-world-objects 'player)
						  (list "Cancel")))
				  (char-name (first chars)))
			(when (< 2 (length chars))
				(format t "~&Which character do you want to play?")
				(setf char-name (choose-option chars)))
			(setf (world-main-character *world*) char-name)
			(if (equalp char-name "Cancel")
				(start-menu)
				(play-game)))))
	
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
--debugging           Switch on debug mode")
	(format t "~A" help-text))

(defun parse-commandline-args ()
	(cond ((or (cmd-parameter "--version" T) (cmd-parameter "-v" T))
			  (print-version) (quit))
		((or (cmd-parameter "--help" T) (cmd-parameter "-h" T))
			(print-help) (quit))
		((cmd-parameter "--license" T)
			(print-text-file "../COPYING")
			(quit))
		((cmd-parameter "--debugging")
			(setf *debugging* T)))
	(start-menu))


;; Initialize the random state (which would otherwise not be very random...)
(setf *random-state* (make-random-state t))

;; Only show the interactive menu if no commandline parameters are given
(if *args*
	(parse-commandline-args)
	(start-menu))
