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
			(when (null (list-world-objects 'player))
				(setf (player-game-admin player) T))
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
	;; XXX This function feels somewhat ugly - any possibility of a cleanup?
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
		(setf race (choose-option (list-world-objects 'race)))
		(setf (player-race player) (get-game-object 'race race))
		(format t "~&Please chose a class:")
		(setf character-class
			(choose-option (list-world-objects 'character-class)))
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
	(when (stringp p) (setf p (get-game-object 'place p)))
	(format t "~&~%~A" (place-description p))
	(format t "~&Neighbouring places: ~A" (string-from-list (place-neighbour p)))
	(format t "~&Players present: ~A" (string-from-list (place-player p)))
	(format t "~&Items: ~A" (string-from-list (place-item p)))
	(format t "~&NPCs: ~A" (string-from-list (place-npc p)))
	(format t "~&Monsters: ~A" (string-from-list (place-monster p))))

(defun game-command (cmd player)
	"Execute a typed-in game command"
	(let* ((command (read-from-string cmd))
			  (space (position #\Space cmd))
			  (arg (if space (second (cut-string cmd (1+ space))) NIL)))
		(if (member command *commands*)
			(if space (funcall command player arg)
				(funcall command player))
			(progn (format t "~&Sorry, this command does not exist!")
			(format t "~&Type 'help' for a list of commands.")))))
		

;;;
;;; Here follow the functions that define the in-game commands.
;;;


;; A list of all in-game commands. Each new command must be registered here.
(defvar *commands*
	'(help place player goto save))

;;; The following commands don't take any arguments except for a player

(defun help (player)
	"Print out a list of in-game commands"
	;; TODO Prettify the typesetting (instead of using tabs)
	(let ((tab (string #\tab)))
		(format t "~&Commands:~%")
		(format t "~&help~A-~AShow this list of game commands" tab tab)
		(format t "~&quit/exit~A-~AExit the game" tab tab)
		(format t "~&place~A-~ADescribe the current location" tab tab)
		(format t "~&player~A-~ADescribe your player" tab tab)
		(format t "~&goto <place>~A-~AGo to a neighbouring location" tab tab)
		(when (player-game-admin player)
			(format t "~&save <game-file>~A-~ASave the game to file" tab tab))))

;; XXX Will the following two functions give problems? (Their name is
;; identical with the struct name) Probably not, but best to be aware.
(defun place (player)
	"Describe the player's current location"
	(describe-place (player-place player)))

(defun player (p)
	"Print a description of this player"
	(when (stringp p) (setf p (get-game-object 'player p)))
	(format t "~&Player ~A:" (player-name p))
	(format t "~&~%Current place: ~A" (player-place p))
	(format t "~&Race: ~A~AClass: ~A"
		(race-name (player-race p)) (string #\Tab)
		(character-class-name (player-class p)))
	(format t "~&=====")
	(format t "~&Attributes:")
	(format t "~&Intelligence: ~A~AStrength: ~A"
		(player-intelligence p) (string #\Tab) (player-strength p))
	(format t "~&Constitution: ~A~ADexterity: ~A"
		(player-constitution p) (string #\Tab) (player-dexterity p))
	(format t "~&=====")
	(format t "~&Weapon: ~A"
		(if (player-weapon p) (weapon-name (player-weapon p)) ""))
	(format t "~&Items: ~A" (string-from-list (list-player-objects 'item p))))

;;; These next functions have to take exactly two argument (the argument
;;; to the function and a player instance).

(defun goto (player location)
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

(defun save (player &optional game-file)
	"Save a game to file (wrapper method around save-world)"
	(unless game-file
		(format t "~&Where do you want to save the game?")
		(input-string game-file))
	(save-world game-file)
	(format t "~&Game saved."))
