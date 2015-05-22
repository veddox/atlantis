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
;;; date: 21/05/2015
;;;

(let ((player NIL))
	(defun play-game (&optional player-name)
		"The main game loop"
		;; XXX Development
		(when (y-or-n-p "~&Load the test world?")
			(load-file "../ATL/lisp-test.atl"))
		;; Initialize the player if necessary
		(when (null player)
			(if player-name
				(setf player (get-game-object 'player player-name))
				(error "~&No player name specified!")))
		(when (null player)
			(create-player player-name))))

(defun create-player (player-name)
	"The user creates a new player"
	;; This function feels somewhat inelegant - lot's of repetetive stuff.
	;; Is it worth cleaning up?
	(let ((player (make-player :name player-name))
			 (race NIL) (character-class NIL)
			 (strength 0) (dexterity 0)
			 (constitution 0) (intelligence 0)
			 (items NIL) (weapons NIL)
			 (character-attributes '(strength dexterity
										constitution intelligence))
			 (character-points NIL))
		(format t "~&The name you have chosen is not registered on this game.")
		(unless (y-or-n-p "~&Create a new player?") (start-menu))
		;; Chose race and class
		(format t "~&Please chose a race:")
		(format t "~&Options: ~A" (string-from-list (list-objects 'race) " - "))
		(setf race (input-string))
		(while (not (member race (list-objects 'race) :test #'equalp))
			(format t "~&Invalid choice. Please reenter:")
			(setf race (input-string)))
		(setf race (get-game-object 'race race))
		(format t "~&Please chose a class:")
		(format t "~&Options: ~A" (string-from-list
									  (list-objects 'character-class) " - "))
		(setf character-class (input-string))
		(while (not (member character-class
						(list-objects 'character-class) :test #'equalp))
			(format t "~&Invalid choice. Please reenter:")
			(setf character-class (input-string)))
		(setf character-class
			(get-game-object 'character-class character-class))
		;; Set character attributes
		(while (< (reduce #'+ character-points) 24) ; Warning: magic number!
			(set-list (random 20) a b c d)
			(setf character-points (list a b c d)))
		(setf text "
Now distribute your attribute points. Random numbers have been chosen, 
you may assign one number to each of the following attributes:")
		(format t "~&~A~%~A~%~%The numbers are: ~%~A"
			text (string-from-list character-attributes " - ")
			(string-from-list character-points " - "))
		;; FIXME Rewrite this section ? >>>
		(dolist (attr character-attributes)
			(simple-input (symbol-value attr)
				(concatenate 'string (string-downcase (symbol-name attr)) ":"))
			;(break)
			(while (not (member attr character-points))
				(format t "~&Sorry, invalid number chosen. Please reenter:")
				(simple-input attr
					(concatenate 'string
						(string-downcase (symbol-name attr)) ":")))
			(setf character-points
				(remove-if #'(lambda (x)
								 (= x (symbol-value attr)))
					character-points)))
		;; Update the player
		(setf (player-race player) race)
		(setf (player-class player) character-class)
		(setf (player-strength player) strength)
		(setf (player-constitution player) constitution)
		(setf (player-dexterity player) dexterity)
		(setf (player-intelligence player) intelligence)
		player))
