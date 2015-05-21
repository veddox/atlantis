;;;
;;; Atlantis is a framework for creating multi-user dungeon worlds.
;;; This is the Common Lisp implementation.
;;;
;;; The client module is responsible for the actual user interface presented
;;; to a player. (Warning: this will likely change significantly, currently
;;; I am only implementing a mock-up before I get the networking part working.)
;;;
;;; Licensed under the terms of the MIT license.
;;; author: Daniel Vedder
;;; date: 09/05/2015
;;;

(let ((player NIL))
	(defun play-game (&optional player-name)
		"The main game loop"
		;; Initialize the player if necessary
		(when (null player)
			(if player-name
				(setf player (get-game-object 'player player-name))
				(error "~&No player name specified!")))
		(when (null player)
			(create-player player-name))))

(defun create-player (player-name)
	"The user creates a new player"
	(let ((race NIL) (class NIL)
			 (strength 0) (dexterity 0)
			 (constitution 0) (intelligence 0)
			 (items NIL) (weapons NIL) (character-points NIL))
		)) ;; TODO
