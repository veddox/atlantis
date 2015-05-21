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
	(race NIL)
	(class NIL)
	(strength 0)
	(dexterity 0)
	(constitution 0)
	(intelligence 0)
	(items NIL)
	(weapons NIL)
	(place ""))


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
