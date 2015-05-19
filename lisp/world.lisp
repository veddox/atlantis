;;;
;;; Atlantis is a framework for creating multi-user dungeon worlds.
;;; This is the Common Lisp implementation.
;;;
;;; The world stores the current state of the game.
;;;
;;; Licensed under the terms of the MIT license
;;; author: Daniel Vedder
;;; date: 15/05/2015
;;;


(defvar *world*)


;; The world is basically a list of struct instances representing
;; each object created in this game

(defstruct world
	(players NIL)
	(places NIL)
	(monsters NIL)
	(npcs NIL)
	(items NIL))

(setf *world* (make-world))


;FIXME Needs work
(defmacro add-game-object (game-object)
	"Add a game-object to the *world*"
	(let ((attribute-list
			  (cond ((player-p game-object) '(world-players *world*))
				  ((place-p game-object) '(world-places *world*))
				  ((monster-p game-object) '(world-monsters *world*))
				  ((npc-p game-object) '(world-npcs *world*))
				  ((item-p game-object) '(world-items *world*)))))
		`(setf ,attribute-list (append ,attribute-list ,game-object))))

; TODO
(defmacro get-game-object (object-name))

