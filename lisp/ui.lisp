;;;
;;; Atlantis is a framework for creating multi-user dungeon worlds.
;;; This is the Common Lisp implementation.
;;;
;;; This module is responsible for the interactive user interface. All
;;; in-game UI should be done here. (Pre-game UI goes into atlantis.lisp.)
;;;
;;; Licensed under the terms of the MIT license.
;;; author: Daniel Vedder
;;; date: 21/05/2015
;;;

;; TODO Out-source all game logic to other modules
;; (This module should be purely UI)


(defun play-game ()
	"The main game loop"
	(let ((player (get-game-object 'player (world-main-player *world*))))
		;; The actual game loop
		(clear-screen)
		(let ((place (get-game-object 'place (player-place player))))
			(describe-place place)
			(input-string command)
			;; TODO Ask for confirmation before quitting
			(while (not (or (equalp command "quit") (equalp command "exit")))
				(game-command command player)
				(input-string command))
			(format t "~&Goodbye!"))))

(defun describe-place (p)
	"Print out a complete description of place p"
	(when (stringp p) (setf p (get-game-object 'place p)))
	(objectify-place-monsters p)
	(format t "~&~A" (string-upcase (place-name p)))
	(format t "~&~%~A" (place-description p))
	(format t "~&~%Neighbouring places: ~A"
		(string-from-list (place-neighbour p)))
	(format t "~&Items: ~A" (string-from-list (place-item p)))
	(format t "~&NPCs: ~A" (string-from-list (place-npc p)))
	(format t "~&Monsters: ~A" (string-from-list
								   (list-place-objects 'monster p))))

(defun game-command (cmd player)
	"Execute a typed-in game command"
	(let* ((command (read-from-string cmd))
			  (space (position #\Space cmd))
			  (arg (if space (second (cut-string cmd (1+ space))) NIL))
			  (cmd-fn (when (member command *commands* :test #'eq) command)))
		(dolist (i (objectify-name-list 'item (player-item player)))
			(when (member (to-string command) (item-command i) :test #'equalp)
				(setf cmd-fn command)
				(return)))
		(if cmd-fn
			(if space (funcall cmd-fn player arg)
				(funcall cmd-fn player))
			(progn (format t "~&Sorry, this command does not exist!")
				(format t "~&Type 'help' for a list of commands.")))))

;;;
;;; Here follow the functions that define the in-game commands.
;;;

;; A list of all in-game commands. Each new command must be registered here.
(defvar *commands*
	'(help player goto take
		 drop talk trade
		 equip attack spell
		 look save clear))

;;; The following commands don't take any arguments except for a player

(defun help (player)
	"Print out a list of in-game commands"
	(setf help-text "
Commands:
help             -  Show this list of game commands
quit/exit        -  Exit the game
clear            -  Clear the screen
look [here]      -  Describe the current location
player           -  Describe your player
goto <place>     -  Go to a neighbouring location
look <object>    -  Show a description of this entity
talk [to] <npc>  -  Talk to an NPC
take <item>      -  Pick up an item lying around
drop <item>      -  Drop the item
equip <weapon>   -  Equip this item as your weapon
attack <monster> -  Fight a monster
save <game-file> -  Save the game to file

Some items may provide additional commands.")
	(format t "~A" help-text))

(defun clear (player)
	"Clear the screen (wrapper function)"
	(clear-screen)
	(place player))

;; XXX Will the following two functions give problems? (Their name is
;; identical with the struct name) Probably not, but best to be aware.
(defun place (player)
	"Describe the player's current location (wrapper function)"
	(describe-place (player-place player)))

(defun player (p)
	"Print a description of this player"
	;; TODO update
	(let ((tab (string #\tab)))
		(when (stringp p) (setf p (get-game-object 'player p)))
		(format t "~&Player ~A:" (player-name p))
		(format t "~&~%Current place: ~A" (player-place p))
		(format t "~&=====~&Attributes:")
		(format t "~&Intelligence: ~A~AStrength: ~A"
			(player-intelligence p) tab (player-strength p))
		(format t "~&Constitution: ~A~ADexterity: ~A"
			(player-constitution p) tab (player-dexterity p))
		(format t "~&=====~&Abilities:~&~A"
			(let ((abilities (player-ability p)))
				(dolist (i (player-item p) (string-from-list abilities))
					(let ((ia (item-ability (get-game-object 'item i))))
						(when ia (setf abilities (append abilities ia)))))))
		(format t "~&=====")
		(format t "~&Weapon: ~A" (player-weapon p))
		;; XXX This will need adjusting for large item numbers
		(format t "~&Items: ~A" (string-from-list (player-item p)))
		(format t "~&=====")
		(format t "~&Max health: ~A~ACurrent health: ~A"
			(player-max-health p) tab (player-health p))
		(format t "~&Experience: ~A~AMoney: ~A"
			(player-experience p) tab (player-money p))))

;;; These next functions have to take two arguments (the argument
;;; to the function and a player instance).

(let ((last-save NIL))
	(defun save (player &optional game-file)
		"Save a game to file (wrapper method around save-world)"
		(cond (game-file (setf last-save game-file))
			((and last-save (not game-file)) (setf game-file last-save))
			((not (or last-save game-file))
				(format t "~&What do you want to call the save file?")
				(input-string game-file)
				(setf game-file (concatenate 'string
									"../saves/" game-file ".world"))
				(setf last-save game-file)))
		(when (y-or-n-p "Save game to ~A?" game-file)
			(save-world game-file)
			(format t "~&Game saved."))))

(defun goto (player &optional location)
	"Go to the specified location"
	;; Look before you leap ;-)
	(unless location
		(format t "~&Please specify a location!")
		(return-from goto))
	(unless (member location
				(place-neighbour (get-game-object 'place
									 (player-place player)))
				:test #'equalp)
		(format t "~&This place does not border your current location!")
		(return-from goto))
	(let ((req (place-requires (get-game-object 'place location))))
		(unless (equalp req "")
			(unless (or (player-has-ability req player)
						(member req (player-item player) :test #'equalp))
				(format t "~&You cannot enter this place unless you have: ~A" req)
				(return-from goto))))
	;; Change places
	(clear-screen)
	(debugging "~&~A is going to ~A." (player-name player) location)
	(change-player-location player location)
	(spawn-monsters location)
	(add-player-experience player 1)
	(describe-place location))

(defun look (player &optional object-name)
	"Print a description of this object"
	(unless object-name
		(place player)
		(return-from look))
	;; A bit of syntactic sugar...
	(cond ((equalp object-name "me") (player player) (return-from look))
		((equalp object-name "here") (place player) (return-from look)))
	(let ((description (get-object-description object-name
						   (player-place player))))
		;; Don't forget items the player is carrying
		(when (member object-name (player-item player) :test #'equalp)
			(setf description
				(item-description (get-game-object 'item object-name))))
		(if description
			(format t "~&(~A) ~A" object-name description)
			(format t "~&Could not find ~A!" object-name))))

(defun talk (player &optional npc-name)
	"Talk to the desired NPC"
	;; TODO Add interactive facility
	(unless npc-name
		(format t "~&Please specify an NPC to talk to!")
		(return-from talk))
	;; Allow for a bit of syntactic sugar (note: interface inconsistency?)
	(let ((split-name (cut-string npc-name 3)))
		(when (equalp (first split-name) "to ")
			(setf npc-name (second split-name))))
	(let* ((place (get-game-object 'place (player-place player)))
			  (npc (when (member npc-name (place-npc place) :test #'equalp)
					   (get-game-object 'npc npc-name))))
		;; Check if the NPC is here
		(unless npc
			(format t "~&~A is not here!" npc-name)
			(return-from talk))
		(format t "~&~A: ~A" (string-upcase npc-name) (npc-says npc))
		;; Trade with the NPC
		(when (and (npc-sells npc)
				  (y-or-n-p "Trade with ~A?" npc-name))
			(trade player npc))
		;; Handle quests
		(let ((quest (get-game-object 'quest (npc-quest npc))))
			(when quest
				(if (dolist (i (quest-proof-item quest))
						(unless (member i (player-item player) :test #'equalp)
							(return T)))
					(when (y-or-n-p "~%~A has a quest. Accept it?" npc-name)
						(format t "~&~A: ~A" (string-upcase npc-name)
							(quest-say-before quest)))
					(when (y-or-n-p "~%Give to ~A: ~A?" npc-name
							  (string-from-list (quest-proof-item quest) ", "))
						(dolist (j (quest-proof-item quest))
							(remove-object-attribute player 'item j))
						(dolist (k (quest-reward-item quest))
							(set-object-attribute player 'item k))
						(add-player-experience player (quest-experience quest))
						(add-player-money player (quest-money quest))
						(format t "~&~A: ~A" (string-upcase npc-name)
							(quest-say-after quest))
						(format t "~&~%Quest complete. You gain:")
						(format t "~&Money: ~A Experience: ~A~&Items: ~A"
							(quest-money quest) (quest-experience quest)
							(string-from-list (quest-reward-item quest)))))))))

(defun trade (player npc)
	"The player trades with this NPC"
	;; TODO Implement player-sells-to-npc feature
	(when (and (stringp npc)
			  (member npc
				  (place-npc (get-game-object 'place (player-place player)))
				  :test #'equalp))
		(setf npc (get-game-object 'npc npc))
		(unless (npc-sells npc)
			(format t "~&This NPC doesn't sell anything!")
			(return-from trade)))
	(format t "~&~%What do you want to buy? Your money: ~S"
		(player-money player))
	(let* ((choice (choose-option (append (npc-sells npc) (list "None"))))
			  (item (get-game-object 'item choice))
			  (cost (if item (item-cost item) NIL)))
		;; TODO The cost should be displayed alongside every item.
		(cond ((equalp choice "None") NIL)
			((not item)
				(format t "~&This object does not exist!")
				(format t "~&(This is a bug in the game world.)")
				NIL)
			((< (player-money player) cost)
				(format t "~&This costs ~S. You do not have enough money!"
					cost))
			((y-or-n-p "Buy ~A for ~S?" choice cost)
				(decf (player-money player) cost)
				(set-object-attribute player 'item choice)
				(format t "~&Bought ~A for ~A." choice cost))))
	(if (y-or-n-p "Buy something else?")
		(trade player npc) (clear player)))

(defun take (player &optional item-name)
	"The player picks up an item"
	(unless item-name
		(format t "~&Please specify an item to pick up!")
		(return-from take))
	(let ((place (get-game-object 'place (player-place player)))
			 (item (get-game-object 'item item-name)))
		(if (member item-name (place-item place) :test #'equalp)
			(if (item-fixed item)
				(format t "~&You cannot pick this up!")
				(progn
					(set-object-attribute player 'item item-name)
					(unless (item-infinite item)
						(remove-object-attribute place 'item item-name))
					(format t "~&You have picked up: ~A" item-name)
					(when (item-command item)
						(format t "~&This item provides commands: ~A"
							(string-from-list (item-command item))))
					(when (item-pickup-hook item)
						(funcall (read-from-string
									 (item-pickup-hook item)) player))))
			(format t "~&Sorry, this item is not here!"))))

(defun drop (player &optional item)
	"The player drops the specified item"
	(unless item
		(format t "~&Please specify an item to drop!")
		(return-from drop))
	(if (member item (player-item player) :test #'equalp)
		(progn
			(remove-object-attribute player 'item item)
			(when (and (item-weapon (get-game-object 'item item))
					  (equalp (player-weapon player) item))
				(set-object-attribute player 'weapon ""))
			(set-object-attribute
				(get-game-object 'place (player-place player)) 'item item)
			(format t "~&You have dropped: ~A" item)
			(when (item-drop-hook (get-game-object 'item item))
				(funcall (read-from-string
							 (item-drop-hook (get-game-object 'item item)))
					player)))
		(format t "~&You do not possess this item!")))

(defun equip (player &optional new-weapon)
	"The player sets another item to be his weapon"
	(unless new-weapon
		(format t "~&Please specify a weapon to be equipped!")
		(return-from equip))
	(when (equalp new-weapon "none")
		(setf (player-weapon player) "")
		(format t "~&You no longer have any weapon equipped.")
		(return-from equip))
	(if (and (member new-weapon (player-item player) :test #'equalp)
			(item-weapon (get-game-object 'item new-weapon)))
		(progn
			(setf (player-weapon player) new-weapon)
			(format t "~&You have equipped: ~A" new-weapon))
		(format t "~&Sorry, this item is not available as a weapon!")))

(defun attack (player &optional opponent)
	"The player launches an attack at a monster"
	;; Check input for validity
	(unless opponent
		(format t "~&Please specify an opponent!")
		(return-from attack))
	(unless (member opponent
				(list-place-objects 'monster
					(get-game-object 'place (player-place player)))
				:test #'equalp)
		(format t "~&This monster is not here!")
		(return-from attack))
	;; Bind all relevant values to variables (saves typing later)
	(let* ((place (get-game-object 'place (player-place player)))
			  (monster (dolist (m (place-monster place))
						   (when (equalp (monster-name m) opponent)
							   (return m))))
			  (m-str (monster-strength monster))
			  (m-dex (monster-dexterity monster))
			  (m-ac (monster-armour-class monster))
			  (m-weapon (get-game-object 'weapon (monster-weapon monster)))
			  (p-str (player-strength player))
			  (p-dex (player-dexterity player))
			  (p-ac (player-armour-class player))
			  (p-weapon (if (not (equalp (player-weapon player) "")) ;lbyl
							(get-game-object 'weapon (player-weapon player))
							(make-weapon :name "Fists" :damage 0)))
			  (damage 0))
		;; Print information about the combattants
		(format t "~&Health ~A: ~A    Health ~A: ~A" (player-name player)
			(player-health player) opponent (monster-health monster))
		;; Determine whether the player or the monster strikes
		(if (> (+ (random 10) p-dex) (+ (random 10) m-dex)) ;XXX magic numbers!
			(setf damage (calculate-damage p-str p-weapon m-dex m-ac))
			(setf damage (- 0 (calculate-damage m-str m-weapon p-dex p-ac))))
		;; Negative damage denotes that the player has been hit
		(cond ((plusp damage)
				  (decf (monster-health monster) damage)
				  (format t "~&You hit! ~A points damage." damage)
				  (when (> 1 (monster-health monster))
					  (let ((experience (round (average m-str m-dex))))
						  (dolist (i (monster-item monster))
							  (set-object-attribute place 'item i))
						  (remove-object-attribute place
							  'monster monster)
						  (add-player-experience player experience)
						  (format t "~&You killed the monster! ")
						  (format t "~A points experience." experience))))
			((minusp damage)
				(change-player-health player damage)
				(format t "~&You missed. Your opponent hit! ")
				(format t "~A points damage." damage))
			(t (format t "~&You missed. Your opponent missed.")))))

(defun calculate-damage (att-str att-weapon def-dex def-ac)
	"A private function to calculate the damage caused by an attack"
	(let ((damage 0))
		(incf damage (if (zerop att-str) 0 (random att-str)))
		(incf damage (weapon-damage att-weapon))
		(decf damage (if (zerop def-dex) 0 (random def-dex)))
		(decf damage def-ac)
		(if (minusp damage) 0 damage)))
