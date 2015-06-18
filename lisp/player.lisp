;;;
;;; Atlantis is a framework for creating multi-user dungeon worlds.
;;; This is the Common Lisp implementation.
;;;
;;; This file represents a single player.
;;;
;;; Licensed under the terms of the MIT license
;;; author: Daniel Vedder
;;; date: 15/05/2015
;;;


(defstruct player
	(name "")
	(race "")
	(class "")
	(strength 0)
	(dexterity 0)
	(constitution 0)
	(intelligence 0)
	(item NIL)
	(weapon "")
	(place "")
	(experience 0)
	(max-health 50)
	(health 50)
	(game-admin NIL))


(defstruct race
	(name "")
	(description "")
	(strength-bonus 0)
	(dexterity-bonus 0)
	(constitution-bonus 0)
	(intelligence-bonus 0)
	(special-ability NIL))

(defstruct character-class
	(name "")
	(description "")
	(special-item NIL)
	(special-ability NIL))


;; This function is probably superfluous, as the player struct should only store
;; names of game objects (the actual objects are stored in *world*)
(let ((list-function (make-list-function 'player NIL)))
	(defun list-player-objects (object-type player)
		"Get a list of the names of all the player's objects of this type."
		(funcall list-function object-type player)))
