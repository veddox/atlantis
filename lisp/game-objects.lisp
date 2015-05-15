;;;
;;; Atlantis is a framework for creating multi-user dungeon worlds.
;;; This is the Common Lisp implementation.
;;;
;;; This file contains all the various kinds of in-game objects like
;;; places, monsters, NPCs, items...
;;;
;;; Licensed under the terms of the MIT license.
;;; author: Daniel Vedder
;;; date: 15/05/2015
;;;

(defstruct place
	(name "")
	(neighbours NIL)
	(items NIL)
	(monsters NIL)
	(npcs NIL))

;;; Temporary, just so I have the structs >>>

(defstruct npc
	(name "")
	(says ""))

(defstruct monster
	(name "")
	(description "")
	(strength 0))

(defstruct item
	(name "")
	(description "")
	(functions NIL))
