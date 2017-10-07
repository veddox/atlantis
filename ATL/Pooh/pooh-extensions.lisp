; The 100 Acre Wood was invented by A.A. Milne for his Winnie-the-Pooh stories.
; This Atlantis world is based on the novels.
;
; This file holds Lisp functions for hooks and the like, to expand on the
; capabilities of the basic Atlantis framework.
;
; author: Daniel Vedder
; date: 20/07/2017

(defun eat (player &optional arg)
	"Allow the player to eat something."
	(cond ((null arg) (format t "~&What do you want to eat?"))
		;; Berries can be eaten any time, but don't have any effect
		((and (equalp arg "berries")
			 (member "berries" (player-item player) :test #'equalp))
			(format t "~&Mmmh, these berries are really delicious!")
			(remove-object-attribute player 'item "berries"))
		;; Honey is reserved as medicine :-)
		((and (or (equalp arg "hunny") (equalp arg "honey"))
			 (member "Hunny" (player-item player) :test #'equalp))
			(if (> (player-health player) 10)
				(format t "~&The honey looks incredibly tempting, but perhaps you should save it for later.")
				(progn (format t "~&You really shouldn't, but you are feeling sore enough to eat some anyway.") (sleep 1)
					(format t "~&You stick your paw deeply into the jar, then draw it out again.") (sleep 1)
					(format t "~&Smooth golden honey runs into your mouth, making you feel much better.")
					(format t "~&+10 HP")
					(change-player-health player 10)
					(remove-object-attribute player 'item "Hunny")
					(set-object-attribute player 'item "Jar"))))
		;; Extract of Malt is very healthy, so obviously it can't be tasty...
		((and (or (equalp arg "extract of malt") (equalp arg "extract") (equalp arg "malt"))
			 (member "Extract of Malt" (player-item player) :test #'equalp))
			(progn (format t "~&You open the bottle and tip it over a spoon.") (sleep 1)
				(format t "~&A big dollop of Extract flows out slowly.") (sleep 3)
				(format t "~&The smell of it makes you wrinkle your nose.") (sleep 2)
				(format t "~&Without thinking much longer, you shove it into your mouth.") (sleep 1)
				(format t "~&That's bitter! You scrunch up your face and try to swallow.") (sleep 3)
				(format t "~&Something that disgusting can only be healthy. +1 HP")
				(change-player-health player 1)))
		(T (format t "~&You can't eat that!"))))

(defun jump (player &optional arg)
	"Jump off Pooh's branch onto his porch."
	(format t "~&You look down nervously, then jump off the branch.")
	(sleep 1)
	(if (> 50 (random 100))
		(progn (format t "~&You land safely. That was fun! You gain 3 XP.")
			(add-player-experience player 3))
		(progn (format t "~&Ouch! That hurt! You take 2 HP fall damage.")
			(change-player-health player -2)))
	(read-line)
	(goto player "Pooh's porch"))

(defun kanga-healing (player)
	"If the player is hurt, Kanga looks after him."
	(when (< (player-health player) (player-max-health player))
		(format t "~&KANGA: Oh my dear, you look hurt! Here, let me take care of you.")
		(sleep 1)
		(format t "~&~%Kanga bandages your wounds. You feel better.")
		(setf (player-health player) (player-max-health player))))

(defun bouncy-tigger (player)
	"Tigger bounces the player, then moves on to a random location"
	(let* ((place (get-game-object 'place (player-place player)))
			  (neighbour (get-game-object 'place
							 (random-elt (place-neighbour place)))))
		(sleep 1)
		(format t "~&~%A large yellow-and-black object comes flying out of nowhere")
		(format t "~&and knocks you over. When you sit up again, you see Tigger")
		(format t "~&grinning widely at you.") (sleep 4)
		(format t "~&~%Tigger bounces away toward ~A." (place-name neighbour))
		(remove-object-attribute place 'npc "Tigger")
		(set-object-attribute neighbour 'npc "Tigger")))

(let ((lost-in NIL))
	(defun lost-in-the-forest (player place prob)
		"Walking through a forest, it's easy to end up going in circles..."
		(when (> prob (random 100))
			;; Make sure all neighbouring places have the is-lost hook
			(dolist (p (place-neighbour (get-game-object 'place place)))
				(let ((p (get-game-object 'place p)))
					(when (zerop (length (place-entry-hook p)))
						(set-object-attribute p 'entry-hook "is-lost"))))
			;; Set the lost-in variable to the current place
			(setf lost-in place)))

	(defun is-lost (player)
		"Return the player to where he started from"
		(when lost-in
			(format t "~&Suddenly, you are no longer sure you are walking in the right")
			(format t "~&direction. Perhaps you should keep more to your left. Or to")
			(format t "~&your right? The trees all look the same here...")
			(format t "~&You are walking in circles!")
			(read-line)
			(change-player-location player lost-in)
			(spawn-monsters lost-in)
			(clear-screen)
			(setf lost-in NIL))))
	
(defun misty-forest (player)
	"A wrapper function for lost-in-the-forest for the misty forest location"
	(lost-in-the-forest player "Misty forest" 67))

(defun deep-forest (player)
	"A wrapper function for lost-in-the-forest for the deep forest location"
	(lost-in-the-forest player "Deep forest" 40))

(defun climb (player &optional arg)
	"Climb something - currently either the bee tree or the rock at the rapids"
	(cond ((null arg) (format t "~&What do you want to climb?"))
		((equalp (player-place player) "Bee tree") (climb-tree player arg))
		((equalp (player-place player) "Rapids") (climb-rock player arg))))

(let ((climbed NIL))
	(defun climb-tree (player &optional arg)
		"Try to climb the bee tree. Warning: bees sting, and trees are tall ;-)"
		(let ((place (get-game-object 'place (player-place player))))
			(when climbed
				(if (member 'down (extract-elements arg))
					(climb-down player)
					(format t "~&You are already sitting up the tree."))
				(return-from climb-tree))
			(format t "~&You start climbing up the tree.")
			(sleep 3)
			;; The player has a 60% chance of success.
			(if (> 60 (random 100))
				(progn (setf climbed T) (add-player-experience player 2)
					(format t "~&You make it to the top."))
				(progn (format t "~&A branch breaks beneath you! You fall into a gorse bush.")
					(sleep 1)
					(format t "~&You take 4 HP fall damage.")
					(change-player-health player -4)))
			;; The bees attack if they are still present
			(dolist (m (place-monster place))
				(when (> (monster-aggression m) (random 100))
					(format t "~&~%You are attacked by ~A!" (monster-name m))
					(attack player (monster-name m))))))

	(defun collect (player &optional arg)
		"Collect honey from the bees' nest (requires an empty jar)"
		(cond ((not (member "Jar" (player-item player) :test #'equalp))
				  (format t "~&If you want to collect honey, you need an empty jar!"))
			((not climbed)
				(format t "~&The honey is up in the tree. You're going to need to climb it first."))
			(T  ;; Collect the honey
				(remove-object-attribute player 'item "Jar")
				(set-object-attribute player 'item "Hunny")
				(format t "~&You fill your jar with honey.")
				;; The bees attack if they are still present
				(dolist (m (place-monster (get-game-object 'place (player-place player))))
					(when (> (monster-aggression m) (random 100))
						(format t "~&~%You are attacked by ~A!" (monster-name m))
						(attack player (monster-name m)))))))

	(defun climb-down (player)
		"Climb down the tree."
		(if climbed
			(progn (format t "~&Slowly you climb back down the tree.")
				(sleep 2)
				(if (> 60 (random 100))
					(format t "~&You reach the ground safely.")
					(progn (format t "~&You lose your grip!")
						(sleep 1)
						(format t "~&Well, that was rather faster than expected.")
						(format t "~&You take 4 HP fall damage.")
						(change-player-health player -4)))
				(setf climbed NIL))
			(format t "~&You are already on the ground.")))

	(defun leave-bee-tree (player)
		"Make sure you've climbed down before leaving the bee tree."
		(when climbed (climb-down player) (read-line))))

(let ((honey-found NIL))
	(defun climb-rock (player &optional arg)
		"Climb the rock at the rapids"
		(if (> 25 (random 100))
			(progn (format t "~&You slip!")
				(read-line)
				(goto player "Stream"))
			(progn (format t "~&You clamber up on the rock.")
				(unless honey-found
					(format t "~&You find a pot of honey!")
					(set-object-attribute (get-game-object 'place (player-place player))
						'item "Hunny")
					(setf honey-found T))))))

(defun play (player &optional arg)
	"Let the player play a game"
	(let ((place (player-place player)))
		(cond ((equalp place "Sandy pit") (build-sandcastle))
			((equalp place "Bridge") (poohsticks player)))))

(let ((sandcastle 0))
	(defun build-sandcastle ()
		"The player builds a sandcastle at the sandy pit."
		(case sandcastle
			(0 (format t "~&You decide to build a sandcastle!")
				(symbol-macrolet ((description (place-description (get-game-object 'place "Sandy pit"))))
					(setf description (string-from-list (list description "Somebody has been building a sandcastle here.")
										  :sep #\newline))))
			(20 (format t "~&You dig a large moat and erect the walls."))
			(40 (format t "~&You build four towers, one at each corner."))
			(60 (format t "~&You pile up sand for a big strong keep in the center."))
			(80 (format t "~&You decorate the castle, adding pretty little details."))
			(100 (format t "~&You stand back and admire your handiwork. What a fine castle!")
				(set-object-attribute (get-game-object 'place "Sandy pit") 'item "Sandcastle")))
		(unless (= sandcastle 100) (incf sandcastle 20))))

(let ((score 0))
	(defun poohsticks (player)
		"Play Poohsticks"
		(if (< (count-instances "Stick" (player-item player) :test #'equalp) 2)
			(format t "~&You need at least two sticks to play Poohsticks!")
			(progn (remove-object-attribute player 'item "Stick")
				(remove-object-attribute player 'item "Stick")
				(format t "~&Which stick do you think will win?")
				(setf choice (choose-number-option '("Stick A" "Stick B")))
				(setf winner (random 2))
				(format t "~&You throw both sticks into the stream on one side of the bridge.") (sleep 1)
				(format t "~&You run to the other side and lean over the railing.") (sleep (random 4))
				(format t "~&~A comes out first!" (nth winner '("Stick A" "Stick B"))) (sleep 1)
				(if (= winner choice)
					(progn (incf score) (format t "~&You win! Your score is now ~A." score))
					(progn (decf score) (format t "~&You lose! Your score is now ~A." score)))))))

(defun nap (player &optional arg)
	"Take a nap in front of Pooh's house"
	(format t "~&You lie down on the bench and close your eyes.") (sleep 1)
	(format t "~&Slowly, you start drifting off to dream land...")  (sleep 1)
	(format t "~&~%Zzzzz Zzzzz Zzzzz")  (sleep 3)
	(format t "~&~%You wake up again.")
	(when (< (player-health player) (player-max-health player))
		(format t "~&You feel better. +1 HP")
		(change-player-health player 1)))

(defun ring (player &optional arg)
	"Ring the bell at Owl's porch"
	(let ((place (get-game-object 'place (player-place player))))
		(if (member "bellrope" (list-place-objects 'item place) :test #'equalp)
			(format t "~&You pull the bellrope. A loud ringing can be heard from inside.")
			(format t "~&Try as you might, you cannot find a bellrope to pull. You knock instead.")))
	(sleep 1)
	(let ((consequences '("Nothing happens. Owl hasn't been too good of hearing lately..."
							 "Nobody answers. Perhaps you should ring louder?"
							 "You wait, but nobody comes. You can hear noises inside, though."
							 "Nothing happens." "Nobody answers." "No reply.")))
		(format t "~&~A" (random-elt consequences))))


;; The golden ring is an easter egg referencing, of course,
;; The Lord of the Rings.

(defun wear (player &optional arg)
	"Wear the mystical golden ring..."
	(if (member 'ring (extract-elements arg))
		(progn
			(format t "~&You slip the golden ring on your finger.") (sleep 1)
			(format t "~&You feel something ought to happen.~&Nothing does."))
		(format t "~&What do you want to wear?")))

(defun ring-of-destiny (player)
	"When the ring is picked up"
	(format t "~&You feel a strange stirring of destiny."))

(defun annoying-ring (player)
	"The ring cannot be dropped!"
	(format t "~&You feel a stab of pain in your heart as you watch the ring drop.")
	(format t "~&On second thoughts, you pick it up again.~%~%")
	(take player "Golden ring"))


;; When somebody plays on my server (Helios), he can leave me a message...

(defun daniel-says (player)
	"Leave a message for the real me"
	(unless (y-or-n-p "~&~%Daniel has more to say to you. Do you want to hear it?")
		(return-from daniel-says))
	(setf msg "~%DANIEL:
Hi there! This is the 'real' Daniel speaking now... Great to see that you've 
gotten so far in the game! I've spent two years and many, many hours getting 
it into shape, so I'm very happy to have other people actually using it :-)
~%If you happen to be playing on my server at the moment, you can actually
leave me a message if you like. I'll definitely read it, and would very much
appreciate any feedback!")
	(format t msg)
	(when (y-or-n-p "~&~%Leave Daniel a message?")
		(format t "~&~%What is your name?")
		(input-string name)
		(setf message (lisp-ed))
		(unless (null message)
			(write-to-file message
				(string-from-list (list "../" name ".msg") "") T)
			(format t "~&Thank you very much :-)"))))
