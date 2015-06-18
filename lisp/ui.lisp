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
		(clear-screen)
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
		(clear-screen)
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
			 (char-attr
			 '((strength 0) (dexterity 0)
			 (constitution 0) (intelligence 0)))
			 (items NIL) (weapon "")
			 (character-points NIL))
		(format t "~&The name you have chosen is not registered on this game.")
		(unless (y-or-n-p "~&Create a new player?") (start-menu))
		;; Chose race and class
		(format t "~&Please chose a race:")
		(setf (player-race player) (choose-option (list-world-objects 'race)))
		(format t "~&Please chose a class:")
		(setf (player-class player)
			(choose-option (list-world-objects 'character-class)))
		(dolist (i (character-class-special-item
					   (get-game-object 'character-class (player-class player))))
			(set-object-attribute player 'item i))
		;; Set character attributes
		(while (or (< (reduce #'+ character-points) 24) ; XXX magic number!
				   (not (set-p character-points)))
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
			(let ((player-fn (build-symbol "player-" attr)))
				;; XXX Kludge ?!
				(eval `(setf (,player-fn ,player) ,val)))
			(setf character-points
				(remove-if #'(lambda (x) (= x val)) character-points)))))

(defun describe-place (p)
	"Print out a complete description of place p"
	(when (stringp p) (setf p (get-game-object 'place p)))
	(format t "~&~A" (string-upcase (place-name p)))
	(format t "~&~%~A" (place-description p))
	(format t "~&~%Neighbouring places: ~A"
		(string-from-list (place-neighbour p)))
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
	'(help place player
		 goto pickup drop talk
		 weapon fight shoot
		 about save clear))

;;; The following commands don't take any arguments except for a player

(defun help (player)
	"Print out a list of in-game commands"
	(setf help-text "
Commands:
help             -  Show this list of game commands
quit/exit        -  Exit the game
clear            -  Clear the screen
place            -  Describe the current location
player           -  Describe your player
goto <place>     -  Go to a neighbouring location
about <object>   -  Show a description of this entity
talk <npc>       -  Talk to an NPC
pickup <item>    -  Pick up an item lying around
drop <item>      -  Drop the item
shoot <monster>  -  Take a shot at a monster
fight <monster>  -  Fight a monster
save <game-file> -  Save the game to file")
	(format t "~A" help-text))

;; XXX Will the following two functions give problems? (Their name is
;; identical with the struct name) Probably not, but best to be aware.
(defun place (player)
	"Describe the player's current location (wrapper function)"
	(describe-place (player-place player)))

(defun clear (player)
	"Clear the screen (wrapper function)"
	(clear-screen)
	(place player))

(defun player (p)
	"Print a description of this player"
	(let ((tab (string #\tab)))
		(when (stringp p) (setf p (get-game-object 'player p)))
		(format t "~&Player ~A:" (player-name p))
		(format t "~&~%Current place: ~A" (player-place p))
		(format t "~&Race: ~A~AClass: ~A" (player-race p) tab (player-class p))
		(format t "~&=====")
		(format t "~&Attributes:")
		(format t "~&Intelligence: ~A~AStrength: ~A"
			(player-intelligence p) tab (player-strength p))
		(format t "~&Constitution: ~A~ADexterity: ~A"
			(player-constitution p) tab (player-dexterity p))
		(format t "~&=====")
		(format t "~&Weapon: ~A" (player-weapon p))
		(format t "~&Items: ~A" (string-from-list (player-item p)))
		(format t "~&=====")
		(format t "~&Max health: ~A~ACurrent health: ~A"
			(player-max-health p) tab (player-health p))
		(format t "~&Experience: ~A" (player-experience p))))

;;; These next functions have to take two arguments (the argument
;;; to the function and a player instance).

(let ((last-save NIL))
	(defun save (player &optional game-file)
		"Save a game to file (wrapper method around save-world)"
		;; XXX Include a permissions check (only allow admins to save)?
		;; Could give problems in single-player mode.
		(cond (game-file (setf last-save game-file))
			((and last-save (not game-file)) (setf game-file last-save))
			((not (or last-save game-file))
				(format t "~&Where do you want to save the game?")
				(input-string game-file)))
		(when (y-or-n-p "Save game to ~A?" game-file)
			(save-world game-file)
			(format t "~&Game saved."))))

(defun goto (player &optional location)
	"Go to the specified location"
	(unless location
		(format t "~&Please specify a location!")
		(return-from goto))
	(when (symbolp location) (setf location (symbol-name location)))
	(when (not (member location
				   (place-neighbour (get-game-object 'place
										(player-place player)))
				   :test #'equalp))
		(format t "~&This place does not border your current location!")
		(return-from goto))
	(clear-screen)
	(debugging "~&~A is going to ~A." (player-name player) location)
	(remove-object-attribute (get-game-object 'place (player-place player))
		'player (player-name player))
	(set-object-attribute player 'place location)
	(set-object-attribute (get-game-object 'place (player-place player))
		'player (player-name player))
	(describe-place location))

(defun about (player &optional object-name)
	"Print a description of this object"
	(unless object-name
		(format t "~&Please specify the object you wish to inspect!")
		(return-from about))
	;; TODO There's got to be a more elegant way of doing this...
	(let ((place (get-game-object 'place (player-place player)))
			 (description NIL))
		(macrolet ((set-descr (place-object place-description object-type)
					   `(when (member object-name (,place-object place)
								  :test #'equalp)
							(setf description (,place-description
												  (get-game-object
													  ',object-type
													  object-name))))))
			(set-descr place-item item-description item)
			(set-descr place-monster monster-description monster)
			(set-descr place-npc npc-description npc))
		(if description
			(format t "~&(~A) ~A" object-name description)
			(format t "~&Could not find ~A!" object-name))))


		;; (cond ((member object-name (place-item place))
		;; 		  (setf description (item-description
		;; 								(get-game-object 'item object-name))))
		;; 	((member object-name (place-monster place))
		;; 		(setf description (monster-description
		;; 							  (get-game-object 'monster object-name))))
		;; 	((member object-name (place-monster place))
		;; 		(setf description (monster-description
		;; 							  (get-game-object 'monster object-name))))
		;; 	(t (format t "~&Could not find ~A!" object-name)
		;; 		(return-from about)))
		;; (format t "~&~A" description)))

(defun talk (player &optional npc-name)
	"Talk to the desired NPC"
	;; TODO Add interactive facility
	(unless npc-name
		(format t "~&Please specify an NPC to talk to!")
		(return-from talk))
	(let* ((place (get-game-object 'place (player-place player)))
			  (npc (when (member npc-name (place-npc place) :test #'equalp)
					   (get-game-object 'npc npc-name))))
		(if npc
			(format t "~&~A: ~A" (string-upcase npc-name) (npc-says npc))
			(format t "~&~A is not here!" npc-name))))

(defun pickup (player &optional item-name)
	"The player picks up an item"
	(unless item-name
		(format t "~&Please specify an item to pick up!")
		(return-from pickup))
	(let ((place (get-game-object 'place (player-place player)))
			 (item (get-game-object 'item item-name)))
		(if (member item-name (place-item place) :test #'equalp)
			(progn
				(set-object-attribute player 'item item-name)
				(when (item-function item)
					(funcall (item-function item)))
				(remove-object-attribute place 'item item-name)
				(format t "~&You have picked up: ~A" item-name))
			(format t "~&Sorry, this item is not here!"))))

(defun drop (player &optional item)
	"The player drops the specified item"
	(unless item
		(format t "~&Please specify an item to drop!")
		(return-from drop))
	(if (member item (player-item player) :test #'equalp)
		(progn
			(remove-object-attribute player 'item item)
			(when (equalp (item-weapon (get-game-object 'item item)) "yes")
				(set-object-attribute player 'weapon ""))
			(set-object-attribute
				(get-game-object 'place (player-place player)) 'item item)
			(format t "~&You have dropped: ~A" item))
		(format t "~&You do not possess this item!")))

(defun weapon (player &optional new-weapon)
	"The player sets another item to be his weapon"
	(when (or (not new-weapon) (equalp new-weapon "none"))
		(setf (player-weapon player) "")
		(format t "~&You no longer have any weapon equipped.")
		(return-from weapon))
	(if (and (member new-weapon (player-item player) :test #'equalp)
			(equalp (item-weapon (get-game-object 'item new-weapon)) "yes"))
		(progn
			(setf (player-weapon player) new-weapon)
			(format t "~&You have equipped: ~A" new-weapon))
		(format t "~&Sorry, this item is not available as a weapon!")))

(defun fight (player &optional opponent)
	"The player enters combat"
	(unless opponent
		(format t "~&Please specify an opponent!")
		(return-from fight))
	;; TODO
	)
