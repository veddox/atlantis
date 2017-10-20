;;;
;;; Atlantis is a framework for creating multi-user dungeon worlds.
;;; This is the Common Lisp implementation.
;;;
;;; Licensed under the terms of the MIT license.
;;; author: Daniel Vedder
;;; date: 09/05/2015
;;;

(defconstant ATLANTIS-VERSION '(0 2 4))

(load "util.lisp")
(load "game-objects.lisp")
(load "player.lisp")
(load "world.lisp")
(load "interpreter.lisp")
(load "ui.lisp")
(load "creator.lisp")

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
	(format t "~&Copyright (c) 2015-2017 Daniel Vedder")
	(format t "~&Licensed under the terms of the GNU GPLv3.")
	(format t "~&www.github.com/atlantis~%"))

(defun start-menu ()
	"Show the start menu and take a choice from the user"
	(clear-screen)
	(print-text-file "banner.txt")
	(format t "~&~%Welcome! What do you want to do?")
	(setf options '("Start a new game" "Load a saved game"
					   "Advanced" "Help" "About" "Exit"))
	(case (choose-number-option options)
		(0 ;; ask the player for his/her name
			(format t "~&What is your name? ")
			(setf player-name (read-line))
			(if (and (member player-name
						 (mapcar #'pathname-name (directory "../saves/*"))
						 :test #'equalp)
					(not (yes-or-no-p "A game by this player already exists. Replace it?")))
				(start-menu)
				(setf (world-player-name *world*) player-name))
			;; let the player choose one of the game worlds
			(format t "~&Which world do you want to play?")
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
		;; choose a previously saved game
		(1 (format t "~&What game file do you want to load?")
			(let ((game (choose-option (append (mapcar #'pathname-name
												   (directory "../saves/*"))
										   '("Back")))))
				(if (equalp game "Back") (start-menu)
					(progn (setf game (concatenate 'string "../saves/"
										   game ".world"))
							   (load-game game)
							   (play-game)))))
		(2 (world-creator)) ;;XXX Remove this from the main menu?
		(3 (clear-screen)
			(pager "../doc/PLAYING" T)
			(start-menu))
		(4 (print-version)
			(when (y-or-n-p "~&~%Show the license text?")
				(pager "../doc/COPYING" T))
			(start-menu))
		(5 (format t "~&Goodbye!")
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
