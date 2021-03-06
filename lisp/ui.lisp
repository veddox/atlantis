;;;
;;; Atlantis is a framework for creating text-adventure worlds.
;;; This is the Common Lisp implementation.
;;;
;;; This module is responsible for the interactive user interface. All
;;; in-game UI should be done here. (Pre-game UI goes into atlantis.lisp.)
;;;
;;; Licensed under the terms of the GNU GPLv3.
;;; author: Daniel Vedder
;;; date: 21/05/2015
;;;

;; TODO Out-source all game logic to other modules
;; (This module should be purely UI)
;; Yeah, probably not going to happen ;-)

;; The number of items/places/characters/etc. to display per line
;; TODO Change to 5 once the (string-from-list) bug is fixed
(setf *max-line-items* 10)

;; The probability of finding a hidden item
;; XXX 100% is cheating somehow, but I fear anything might prove a
;; stumbling block for new players who only search once (if at all)...
(setf *find-probability* 100)

(defun play-game ()
	"The main game loop"
	(let ((player (get-game-object 'player (world-main-character *world*))))
		;; Don't let dead characters resurrect
		(unless (plusp (player-health player))
			(error "You are dead! You cannot continue this game."))
		;; If the player's starting position is not specified, choose at random
		(when (zerop (length (player-place player)))
			(format t "~&Choosing a random starting location.")
			(setf (player-place player)
				(random-elt (list-world-objects 'place))))
		;; The actual game loop
		(clear-screen)
		(let ((place (get-game-object 'place (player-place player))))
			(describe-place place)
			(format t "~&>>> ")
			(do ((command (read-line) (read-line)))
				((and (or (equalp command "quit") (equalp command "exit"))
					 (y-or-n-p "~&Really quit?"))
					(save-world)
					(format t "~&Goodbye!")
					(quit))
				(cond ((zerop (length command)) NIL)
					((not (alpha-char-p (aref command 0)))
						(format t "~&Invalid input."))
					(T (game-command command player)
						(save-world)))
				(format t "~&>>> ")))))

(defun describe-place (p)
	"Print out a complete description of place p"
	(when (stringp p) (setf p (get-game-object 'place p)))
	(objectify-place-monsters p)
	(format t "~&~A" (string-upcase (place-name p)))
	(format t "~&~%~A" (place-description p))
	(format t "~&~%Neighbouring places: ~A"
		(string-from-list (place-neighbour p) :line-length *max-line-items*))
	(when (place-item p)
		(format t "~&Items: ~A" (string-from-list (place-item p)
									:line-length *max-line-items*)))
	(when (place-npc p)
		(format t "~&NPCs: ~A" (string-from-list (place-npc p)
								    :line-length *max-line-items*)))
	(when (place-monster p)
		(format t "~&Monsters: ~A" (string-from-list
									   (list-place-objects 'monster p)
									    :line-length *max-line-items*)))
	(when (place-command p)
		(format t "~&Commands: ~A" (string-from-list (place-command p)
									    :line-length *max-line-items*))))

(defun describe-player (p)
	"Print a description of this player"
	(let ((tab (string #\tab)))
		(when (stringp p) (setf p (get-game-object 'player p)))
		(format t "~&~A~%~%~A"
			(string-upcase (player-name p)) (player-description p))
		(format t "~&~%Current place: ~A" (player-place p))
		(format t "~&=====")
		(format t "~&Intelligence: ~A~AStrength: ~A"
			(player-intelligence p) tab (player-strength p))
		(format t "~&Constitution: ~A~ADexterity: ~A"
			(player-constitution p) tab (player-dexterity p))
		(format t "~&=====")
		(format t "~&Max health: ~A~ACurrent health: ~A"
			(player-max-health p) tab (player-health p))
		(format t "~&Experience: ~A~AMoney: ~A"
			(player-experience p) tab (player-money p))
		(format t "~&=====")
		(format t "~&Holding: ~A" (player-tool p))
		(format t "~&Items: ~A" (string-from-list (player-item p)
									:line-length *max-line-items*))))

(defun game-command (cmd player)
	"Execute a typed-in game command"
	(let* ((command (read-from-string cmd))		
			  (space (position #\Space cmd))
			  (arg (if space (second (cut-string cmd (1+ space))) NIL))
			  ;; Check default commands
			  (fuzzy-cmd (fuzzy-match (to-string command)
							 (mapcar #'to-string *commands*)))
			  (cmd-fn (when fuzzy-cmd (read-from-string fuzzy-cmd))))
		;; Search for place commands
		(let ((place-cmd (fuzzy-match (to-string command)
							 (place-command (get-game-object 'place
												(player-place player)))
							 :strict T)))
				 (when place-cmd (setf cmd-fn (read-from-string place-cmd))))
		;; Search for item commands (highest priority)
		(dolist (i (objectify-name-list 'item (player-item player)))
			(let ((item-cmd (fuzzy-match (to-string command)
								(item-command i) :strict T)))
				(when item-cmd (setf cmd-fn (read-from-string item-cmd)) (return))))
		;; If found, execute the command
		(if cmd-fn
			(progn 
				(when (member cmd-fn (keys *aliases*))
					(setf cmd-fn (cassoc cmd-fn *aliases*)))
				(if space (funcall cmd-fn player arg)
					(funcall cmd-fn player)))
			(progn (format t "~&Sorry, this command is not available!")
				(format t "~&Type 'help' for a list of commands.")))))

;;;
;;; Here follow the functions that define the in-game commands.
;;;

;; A list of all in-game commands. Each new command must be registered here.
(defvar *commands*
	'(help look goto take inventory
		 drop talk hold attack
		 search clear manual archive))

;; A list of aliases (used to avoid naming conflicts with inbuilt functions)
(defvar *aliases*
	'((search seek)))

(defun add-alias (alias fn)
	"Add an alias to the list"
	(setf *aliases* (append *aliases* (list (list alias fn)))))

;;; Command functions have to take two arguments (a player instance and
;;; an optional(!) argument to the function).

(defun help (player &optional arg)
	"Print out a list of in-game commands"
	(print-text-file "../doc/COMMANDS"))

(defun manual (player &optional arg)
	"Show the game manual in a pager"
	(pager "../doc/PLAYING" T)
	(clear player))

(defun clear (player &optional arg)
	"Clear the screen (wrapper function)"
	(clear-screen)
	(describe-place (player-place player)))

(defun archive (player &optional arg)
	"Save a snapshot of the current game state (especially for debugging)"
	(let ((archive-name (concatenate 'string "../"
							(world-player-name *world*) "_archive.world")))
		(save-world archive-name)
		(format t "~&Archived game to ~A." archive-name)))

(defun inventory (player &optional arg)
	"A wrapper for 'look me'"
	(look player "me"))

(defun goto (player &optional location)
	"Go to the specified location"
	;; Look before you leap ;-)
	(unless location
		(format t "~&Please specify a location!")
		(return-from goto))
	(setf location (fuzzy-match location
					   (place-neighbour (get-game-object 'place
											(player-place player)))))
	(unless location
		(format t "~&This place does not border your current location!")
		(return-from goto))
	;; You can only enter a place if you have one of the requirements
	;; TODO Deal with multiple possible requirements
	(let ((req (place-requires (get-game-object 'place location))))
		(unless (equalp req "")
			(unless (or (player-has-ability req player)
						(member req (player-item player) :test #'equalp))
				(format t "~&You cannot enter this place unless you have: ~A" req)
				(return-from goto))))
	;; Change places
	(let ((hook (place-exit-hook (get-game-object 'place (player-place player))))) ;exit hook
		(unless (zerop (length hook)) (funcall (read-from-string hook) player)))
	(clear-screen)
	(debugging "~&~A is going to ~A." (player-name player) location)
	(change-player-location player location)
	(spawn-monsters location)
	(add-player-experience player 1)
	(describe-place (player-place player))
	(let ((hook (place-entry-hook (get-game-object 'place location)))) ;entry hook
		(unless (zerop (length hook)) (funcall (read-from-string hook) player)))
	;; Aggressive monsters attack
	(dolist (m (place-monster (get-game-object 'place (player-place player))))
		(when (> (monster-aggression m) (random 100))
			(format t "~&~%You are attacked by ~A!" (monster-name m))
			(attack player (monster-name m)))))

(defun look (player &optional object-name)
	"Print a description of this object"
	(unless object-name (setf object-name "here"))
	;; A bit of syntactic sugar...
	(cond ((equalp object-name "me")
			  (describe-player player) (return-from look))
		((equalp object-name "here")
			(describe-place (player-place player)) (return-from look))
		((equalp object-name "around")
			(seek player) (return-from look)))
	(let* ((place (get-game-object 'place (player-place player)))
			  (o-name (fuzzy-match object-name (append (place-item place)
												   (place-npc place)
												   (list-place-objects 'monster place)
												   (player-item player))))
			  (description (get-object-description o-name place)))
		;; Don't forget items the player is carrying
		(when (member o-name (player-item player) :test #'equalp)
			(setf description
				(item-description (get-game-object 'item o-name))))
		(if description
			(progn
				(format t "~&(~A) ~A" o-name description)
				(when (and (get-game-object 'item o-name)
						  (plusp (item-weapon-damage
									 (get-game-object 'item o-name))))
					(format t "~&~%This item has a weapon strength of ~S."
						(item-weapon-damage (get-game-object 'item o-name)))))
			(format t "~&Could not find ~A!" object-name))))

(defun seek (player &optional arg)
	"Search for hidden items in the current room"
	(format t "~&You start hunting around.") (sleep (random 4))
	(let* ((place (get-game-object 'place (player-place player)))
			  (hidden (place-hidden place)))
		(dolist (h hidden)
			(when (> *find-probability* (random 100))
				(format t "~&You find: ~A" h)
				(set-object-attribute place 'item h)
				(remove-object-attribute place 'hidden h))))
	(format t "~&You finish searching."))

(defun talk (player &optional npc-name)
	"Talk to the desired NPC"
	(unless npc-name
		(format t "~&Please specify an NPC to talk to!")
		(return-from talk))
	;; Allow for a bit of syntactic sugar
	(let ((split-name (cut-string npc-name 3)))
		(when (and (listp split-name) (equalp (first split-name) "to "))
			(setf npc-name (second split-name))))
	(let* ((place (get-game-object 'place (player-place player)))
			  (npc-name (fuzzy-match npc-name (place-npc place)))
			  (npc (when (member npc-name (place-npc place) :test #'equalp)
					   (get-game-object 'npc npc-name))))
		;; Check if the NPC is here
		(unless npc
			(format t "~&This NPC is not here!")
			(return-from talk))
		;; The NPC says one of its lines
		(when (npc-says npc)
			(format t "~&~A: ~A" (string-upcase npc-name)
			(random-elt (npc-says npc))))
		;; Interaction hook
		(let ((hook (npc-interaction-hook npc)))
			(unless (zerop (length hook))
				(funcall (read-from-string hook) player)))
		;; Trade with the NPC
		(when (and (npc-sells npc)
				  (y-or-n-p "Trade with ~A?" npc-name))
			(trade player npc))
		;; Handle quests
		(unless (zerop (length (npc-quest npc)))
			(quest player npc))))

(defun quest (player npc)
	"Handle NPC quests"
	(let ((quest (get-game-object 'quest (npc-quest npc)))
			 (npc-name (npc-name npc)))
		(unless quest (format t "~&This NPC doesn't have a quest!")
			(return-from quest))
		(if (dolist (i (quest-proof-item quest))
				(unless (member i (player-item player) :test #'equalp)
					(return T)))
			(when (y-or-n-p "~%~A has a quest. Do you want to hear about it?" npc-name)
				(format t "~&~A: ~A" (string-upcase npc-name)
					(quest-say-before quest)))
			(when (y-or-n-p "~%Give to ~A: ~A?" npc-name
					  (string-from-list (quest-proof-item quest) :sep ", "
						  :line-length *max-line-items*))
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
					(string-from-list (quest-reward-item quest)
						:line-length *max-line-items*))
				(let ((hook (quest-completed-hook quest)))
					(unless (zerop (length hook)) (funcall (read-from-string hook) player)))
				(unless (quest-infinite quest)
					(remove-object-attribute npc 'quest npc))))))		

(defun trade (player npc)
	"The player trades with this NPC"
	;; XXX This is no longer a game command - remove the leading check?
	(when (and (stringp npc)
			  (member npc
				  (place-npc (get-game-object 'place (player-place player)))
				  :test #'equalp))
		(setf npc (get-game-object 'npc npc))
		(unless (or (npc-sells npc) (npc-buys npc))
			(format t "~&This NPC doesn't buy or sell anything!")
			(return-from trade)))
	(if (and (npc-sells npc) (npc-buys npc))
		(setf trade (choose-option '("Buy" "Sell" "None")))
		(if (npc-sells npc) (setf trade "Buy") (setf trade "Sell")))
	(cond ((equalp trade "None") (return-from trade))
		((equalp trade "Buy")
			(format t "~&What do you want to buy? Your money: ~S"
				(player-money player))
			(let* ((choice (choose-option (append (npc-sells npc) (list "None"))))
					  (item (get-game-object 'item choice))
					  (cost (if item (item-cost item) NIL)))
				;; TODO If possible, the cost should be displayed alongside every item.
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
						(format t "~&Bought ~A for ~A." choice cost)))))
		((equalp trade "Sell")
			(format t "~&~A will buy:" (npc-name npc))
			(let* ((choice (choose-option (append (npc-buys npc) (list "None"))))
					   (item (get-game-object 'item choice))
					   (cost (if item (item-cost item) NIL)))
				 (cond ((equalp choice "None") NIL)
					 ((not (member choice (player-item player) :test #'equalp))
						 (format t "~&You cannot sell an item you don't have!")
						 NIL)
					 ((not item)
						 (format t "~&This object does not exist!")
						 (format t "~&(This is a bug in the game world.)")
						 NIL)
					 ((y-or-n-p "Sell ~A for ~S?" choice cost)
						 (incf (player-money player) cost)
						 (remove-object-attribute player 'item choice)
						 (format t "~&Sold ~A for ~S." choice cost))))))
	(if (y-or-n-p "Continue trading?")
		(trade player npc) (clear player)))

(defun take (player &optional item-name)
	"The player picks up an item"
	(unless item-name
		(format t "~&Please specify an item to pick up!")
		(return-from take))
	(let* ((place (get-game-object 'place (player-place player)))
			 (item-name (fuzzy-match item-name (place-item place)))
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
							(string-from-list (item-command item)
								:line-length *max-line-items*)))
					(unless (zerop (length (item-pickup-hook item)))
						(funcall (read-from-string (item-pickup-hook item))
							player))))
			(format t "~&Sorry, this item is not here!"))))

(defun drop (player &optional item)
	"The player drops the specified item"
	(unless item
		(format t "~&Please specify an item to drop!")
		(return-from drop))
	(setf item (fuzzy-match item (player-item player)))
	(if item
		(progn
			(remove-object-attribute player 'item item)
			(when (equalp (player-tool player) item)
				(set-object-attribute player 'tool ""))
			(unless (item-infinite (get-game-object 'item item))
				;; Infinite items are destroyed when dropped
				;; (otherwise they create new "spawn points")
				(set-object-attribute (get-game-object 'place
										  (player-place player)) 'item item))
			(format t "~&You have dropped: ~A" item)
			(unless (zerop (length (item-drop-hook (get-game-object 'item item))))
				(funcall (read-from-string
							 (item-drop-hook (get-game-object 'item item)))
					player)))
		(format t "~&You do not possess this item!")))

(defun hold (player &optional new-item)
	"The player chooses another item to hold as a tool/weapon"
	(unless new-item
		(format t "~&Please specify an item to hold!")
		(return-from hold))
	(when (equalp new-item "none")
		(setf (player-tool player) "")
		(format t "~&You are no longer holding anything.")
		(return-from hold))
	(setf new-item (fuzzy-match new-item (player-item player)))
	(if new-item
		(progn
			(setf (player-tool player) new-item)
			(format t "~&You are now holding: ~A" new-item))
		(format t "~&You do not possess this item!")))

(defun attack (player &optional opponent)
	"The player launches an attack at a monster"
	;; Check input for validity
	(unless opponent
		(format t "~&Please specify an opponent!")
		(return-from attack))
	(setf opponent
		(fuzzy-match opponent
			(list-place-objects 'monster
				(get-game-object 'place (player-place player)))))
	(unless opponent
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
			  (m-weapon (if (not (equalp (monster-weapon monster) "")) ;lbyl
							(get-game-object 'item (monster-weapon monster))
							(make-item :name "Fists" :weapon-damage 1)))
			  (p-str (player-strength player))
			  (p-dex (player-dexterity player))
			  (p-ac (player-armour-class player))
			  (p-weapon (if (not (equalp (player-tool player) "")) ;lbyl
							(get-game-object 'item (player-tool player))
							(make-item :name "Fists" :weapon-damage 1)))
			  (monster-hook (monster-attack-hook monster))
			  (damage 0))
		;; Call the monster's attack hook
		(unless (zerop (length monster-hook))
			(funcall (read-from-string monster-hook) player))
		;; Determine whether the player or the monster strikes
		(if (> (+ (random 10) p-dex) (+ (random 10) m-dex)) ;XXX magic numbers!
			(setf damage (calculate-damage p-str p-weapon m-dex m-ac))
			(setf damage (- 0 (calculate-damage m-str m-weapon p-dex p-ac))))
		;; Negative damage denotes that the player has been hit
		(cond ((plusp damage)
				  (decf (monster-health monster) damage)
				  (format t "~&You hit! ~A points damage." damage))
			((minusp damage)
				(format t "~&You missed. Your opponent hit! ")
				(format t "~A points damage." damage)
				(change-player-health player damage))
			(t (format t "~&You missed. Your opponent missed.")))
		;; Print information about the combattants
		(format t "~&Health ~A: ~A    Health ~A: ~A" (player-name player)
			(player-health player) opponent (monster-health monster))
		;;When the monster is killed
		(when (> 1 (monster-health monster))
			(let ((experience (* (+ m-str m-dex) 5))) ;XXX magic numbers!
				(dolist (i (monster-item monster))
					(set-object-attribute place 'item i))
				(remove-object-attribute place
					'monster monster)
				(add-player-experience player experience)
				(if (monster-death-msg monster)
					(format t "~&~%~A" (monster-death-msg monster))
					(format t "~&You killed the monster!"))
				(format t "~&~A points experience." experience)))))

(defun calculate-damage (att-str att-weapon def-dex def-ac)
	"A private function to calculate the damage caused by an attack"
	(let ((damage 0))
		(incf damage (if (zerop att-str) 0 (random att-str)))
		(incf damage (item-weapon-damage att-weapon))
		(decf damage (if (zerop def-dex) 0 (random def-dex)))
		(decf damage def-ac)
		(if (minusp damage) 0 damage)))
