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
				(progn (format t "~&You really shouldn't, but you are feeling sore enough to eat some anyway.")
					(format t "~&You stick your paw deeply into the jar, then draw it out again.")
					(format t "~&Smooth golden honey runs into your mouth, making you feel much better.")
					(format t "~&+10 HP")
					(change-player-health player 10)
					(remove-object-attribute player 'item "Hunny")
					(set-object-attribute player 'item "Jar"))))
		(T (format t "~&You can't eat that!"))))

(defun jump (player &optional arg)
	"Jump off Pooh's branch onto his porch."
	(format t "~&You look down nervously, then jump off the branch.")
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
		(format t "~&~%Kanga bandages your wounds. You feel better.")
		(setf (player-health player) (player-max-health player))))

(defun bouncy-tigger (player)
	"Tigger bounces the player, then moves on to a random location"
	(let* ((place (get-game-object 'place (player-place player)))
			  (neighbour (get-game-object 'place
							 (random-elt (place-neighbour place)))))
		(format t "~&~%A large yellow-and-black object comes flying out of nowhere")
		(format t "~&and knocks you over. When you sit up again, you see Tigger")
		(format t "~&grinning widely at you.")
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

(let ((climbed NIL))
	(defun climb (player &optional arg)
		"Try to climb the bee tree. Warning: bees sting, and trees are tall ;-)"
		(let ((place (get-game-object 'place (player-place player))))
			(when climbed
				(if (member 'down (extract-elements arg))
					(climb-down player)
					(format t "~&You are already sitting up the tree."))
				(return-from climb))
			(format t "~&You start climbing up the tree.")
			;; The player has a 60% chance of success.
			(if (> 60 (random 100))
				(progn (setf climbed T) (add-player-experience player 2)
					(format t "~&You make it to the top."))
				(progn (format t "~&A branch breaks beneath you! You fall into a gorse bush.")
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
				(if (> 60 (random 100))
					(format t "~&You reach the ground safely.")
					(progn (format t "~&You lose your grip!")
						(format t "~&Well, that was rather faster than expected.")
						(format t "~&You take 4 HP fall damage.")
						(change-player-health player -4)))
				(setf climbed NIL))
			(format t "~&You are already on the ground.")))

	(defun leave-bee-tree (player)
		"Make sure you've climbed down before leaving the bee tree."
		(when climbed (climb-down player))
		(read-line)))

;; The golden ring is an easter egg referencing, of course,
;; The Lord of the Rings.

(defun wear (player &optional arg)
	"Wear the mystical golden ring..."
	(if (member 'ring (extract-elements arg))
		(progn
			(format t "~&You slip the golden ring on your finger.")
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
