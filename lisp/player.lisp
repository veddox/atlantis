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
	(money 0)
	(item NIL)
	(weapon "")
	(armour-class 0)
	(place "")
	(experience 0)
	(level 0)
	(max-health 50)
	(health 50)
	(night-vision NIL)
	(game-admin NIL))

;; How many XP are needed to level up?
;; XXX Make this configurable in ATL?
(defvar *level-experience* 100)

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


(defun add-player (player)
	"Add this player to the game world"
	(when (null (list-world-objects 'player))
		(setf (player-game-admin player) T))
	(change-player-location player (player-place player))
	(add-game-object player))

(defun change-player-location (player location)
	"Change the player's location and do housekeeping"
	(setf location (to-string location))
	(objectify-place-monsters location)
	(when (player-place player)
		(remove-object-attribute (get-game-object 'place (player-place player))
			'player (player-name player)))
	(set-object-attribute player 'place location)
	(set-object-attribute (get-game-object 'place location)
		'player (player-name player)))

;; XXX This function is probably superfluous, as the player struct should only 
;; store names of game objects (the actual objects are stored in *world*)
(let ((list-function (make-list-function 'player NIL)))
	(defun list-player-objects (object-type player)
		"Get a list of the names of all the player's objects of this type."
		(funcall list-function object-type player)))

(defun change-player-health (player amount)
	"Change the player's health points"
	(incf (player-health player) amount)
	(when (> 1 (player-health player))
		(error "You died!"))) ;; TODO adjust this (especially for multiplayer)

(defun add-player-experience (player amount)
	"The player gains experience points"
	(incf (player-experience player) amount)
	(when (zerop (rem (player-experience player) *level-experience*))
		(incf (player-level player) player)))

(defun add-player-money (player amount)
	"Increase the player's money by amount"
	(incf (player-money player) amount))
