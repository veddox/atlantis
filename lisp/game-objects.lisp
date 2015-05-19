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
	(description "")
	(neighbour NIL)
	(item NIL)
	(monster NIL)
	(npc NIL))

;;; INCOMPLETE STRUCTS >>>

(defstruct npc
	(name "")
	(says ""))

(defstruct monster
	(name "")
	(description "")
	(strength 0)
	(armour-class 0))

(defstruct item
	(name "")
	(description "")
	(function NIL))

(defun set-object-attribute (game-object property value)
	"Set the attribute 'property' of 'game-object' to 'value'"
	;; Here follows Lisp magic :D      (that took ages to get right...)
	;; I'm not sure how elegant it is to call (eval) explicitly, but in this
	;; case I couldn't avoid it - I needed a mix between a macro and a function
	(let ((command (read-from-string
						 (concatenate 'string
							 (symbol-name (type-of game-object))
							 "-" (if (stringp property) property
									 (symbol-name property))))))
		(eval `(setf (,command ,game-object) ,value))))

