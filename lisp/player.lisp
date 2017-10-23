;;;
;;; Atlantis is a framework for creating text-adventure worlds.
;;; This is the Common Lisp implementation.
;;;
;;; This file represents a single player.
;;;
;;; Licensed under the terms of the GNU GPLv3.
;;; author: Daniel Vedder
;;; date: 15/05/2015
;;;


(defstruct player
	(name "")
	(description "")
	(strength 0)
	(dexterity 0)
	(constitution 0)
	(intelligence 0)
	(money 0)
	;TODO Remove abilities again
	(ability NIL)
	(item NIL)
	(weapon "")
	(armour-class 0)
	(place "")
	(experience 0)
	(level 0)
	(max-health 50)
	(health 50))

;; How many XP are needed to level up?
;; And what do levels do anyway?
;; XXX Make this configurable in ATL?
(defvar *level-experience* 100)

(defun add-player (player)
	"Add this player to the game world"
	(change-player-location player (player-place player))
	(add-game-object player))

(defun change-player-location (player location)
	"Change the player's location and do housekeeping"
	(setf location (to-string location))
	(objectify-place-monsters location)
	(set-object-attribute player 'place location))

;;TODO limit the number of carryable items?

;; XXX This function is probably superfluous, as the player struct should only 
;; store names of game objects (the actual objects are stored in *world*)
(let ((list-function (make-list-function 'player NIL)))
	(defun list-player-objects (object-type player)
		"Get a list of the names of all the player's objects of this type."
		(funcall list-function object-type player)))

(defun player-has-ability (ability player)
	;;TODO Remove this
	"Check whether a player has the given ability"
	(or	(member ability (player-ability player) :test #'equalp)
		(dolist (i (player-item player) NIL)
			(when (member ability (item-ability (get-game-object 'item i))
					  :test #'equalp) (return T)))))

(defun change-player-health (player amount)
	"Change the player's health points"
	(incf (player-health player) amount)
	(when (> (player-health player) (player-max-health player))
		(setf (player-health player) (player-max-health player)))
	(when (> 1 (player-health player))
		(error "You died!"))) ;; TODO adjust this

(defun add-player-experience (player amount)
	"The player gains experience points"
	(incf (player-experience player) amount)
	(when (zerop (rem (player-experience player) *level-experience*))
		(incf (player-level player) player)))

(defun add-player-money (player amount)
	"Increase the player's money by amount"
	(incf (player-money player) amount))
