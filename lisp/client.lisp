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
	(defun play-game (player-name)
		"The main game loop"
		;; Initialize the player if necessary
		(when (null player)
			(setf player (get-game-object 'player player-name)))
		(when (null player)
			(setf player (create-player player-name))
			(add-game-object player)
			(set-object-attribute (get-game-object 'place (player-place player))
				'player (player-name player)))
		;; The actual game loop
		(let ((place (get-game-object 'place (player-place player))))
			(describe-place place)
			(input-string command)
			(while (not (or (equalp command "quit") (equalp command "exit")))
				(game-command command player)
				(input-string command))
			(format t "~&Goodbye!"))))

(defun create-player (player-name)
	"The user creates a new player"
	;; XXX This function feels somewhat inelegant - lot's of repetetive stuff.
	;; Is it worth cleaning up?
	(let ((player (make-player :name player-name
					  :place (world-starting-place *world*)))
			 (race NIL) (character-class NIL)
			 (char-attr
			 '((strength 0) (dexterity 0)
			 (constitution 0) (intelligence 0)))
			 (items NIL) (weapon "")
			 (character-points NIL))
		(format t "~&The name you have chosen is not registered on this game.")
		(unless (y-or-n-p "~&Create a new player?") (start-menu))
		;; Chose race and class
		(format t "~&Please chose a race:")
		(format t "~&Options: ~A" (string-from-list (list-objects 'race)))
		(setf race (input-string))
		(while (not (member race (list-objects 'race) :test #'equalp))
			(format t "~&Invalid choice. Please reenter:")
			(setf race (input-string)))
		(setf (player-race player) (get-game-object 'race race))
		(format t "~&Please chose a class:")
		(format t "~&Options: ~A" (string-from-list
									  (list-objects 'character-class)))
		(setf character-class (input-string))
		(while (not (member character-class
						(list-objects 'character-class) :test #'equalp))
			(format t "~&Invalid choice. Please reenter:")
			(setf character-class (input-string)))
		(setf (player-class player)
			(get-game-object 'character-class character-class))
		;; Set character attributes
		(while (< (reduce #'+ character-points) 24) ; Warning: magic number!
			(set-list (1+ (random 20)) a b c d)
			(setf character-points (list a b c d)))
		(setf text "
Now distribute your attribute points. Random numbers have been chosen, 
you may assign one number to each of the following attributes:")
		(format t "~&~A~%~A~%~%The numbers are:"
			text (string-from-list (keys char-attr)))
		;; TODO I should replace simple-input with something offering 'magic'
		(do* ((i 0 (1+ i)) (attr (safe-nth i (keys char-attr))
							   (safe-nth i (keys char-attr)))
				 (val (cassoc attr char-attr) (cassoc attr char-attr)))
			((= i (length char-attr)) player)
			(format t "~&~A" (string-from-list character-points))
			(simple-input val (concatenate 'string (symbol-name attr) ":"))
			(while (not (member val character-points))
				(format t "~&Sorry, invalid number chosen. Please reenter:")
				(simple-input val (concatenate 'string (symbol-name attr) ":")))
			;; FIXME Gives problems if two equal numbers are in char-points
			(let ((player-fn (build-symbol "player-" attr)))
				;; XXX Kludge ?!
				(eval `(setf (,player-fn ,player) ,val)))
			(setf character-points
				(remove-if #'(lambda (x) (= x val)) character-points)))))

(defun describe-place (p)
	"Print out a complete description of place p"
	(when (stringp p)
		(setf p (get-game-object 'place p)))
	(format t "~&~%~A" (place-description p))
	(format t "~&Neighbouring places: ~A" (string-from-list (place-neighbour p)))
	(format t "~&Players present: ~A" (string-from-list (place-player p)))
	(format t "~&Items: ~A" (string-from-list (place-item p))))

(defun game-command (cmd player)
	"Execute a typed-in game command"
	;; TODO Instead of converting typed-in text into a function call, would it
	;; be better to keep an association list (command:function)? That would
	;; mean some more book-keeping, but would prevent ugly/fatal error messages.
	(let ((space (position #\Space cmd)))
		(if space
			(call-function (read-from-string cmd)
				(second (cut-string cmd (1+ space))) player)
			(call-function (read-from-string cmd) player))))


;;;
;;; Here follow the functions that define the in-game commands.
;;;

;;; The following commands consist of only one word and take only one argument

(defun help (&optional player)
	"Print out a list of in-game commands"
	(let ((tab (string #\tab)))
		(format t "~&Commands:~%")
		(format t "~&place~A-~ADescribe the current location" tab tab)
		(format t "~&player~A-~ADescribe your player" tab tab)
		(format t "~&goto <place>~A-~AGo to a neighbouring location" tab tab)))

;; XXX Will the following two functions give problems?
;; (Their name is identical with the struct name)
(defun place (player)
	"Describe the player's current location"
	(describe-place (player-place player)))

(defun player (player)
	"Print a description of this player"
	;; TODO
	)

;;; These next functions have to take exactly two argument (the argument
;;; to the function and a player instance).

(defun goto (location player)
	"Go to the specified location"
	(format t "~&~A is going to ~A." (player-name player) location)
	(when (symbolp location) (setf location (symbol-name location)))
	(when (not (member location
				   (place-neighbour (get-game-object 'place
										(player-place player)))
				   :test #'equalp))
		(format t "~&This place does not border your current location!")
		(return-from goto NIL))
	(remove-object-attribute (get-game-object 'place (player-place player))
		'player (player-name player))
	(set-object-attribute player 'place location)
	(set-object-attribute (get-game-object 'place (player-place player))
		'player (player-name player))
	(describe-place location))
