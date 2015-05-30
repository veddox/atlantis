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
	(name "")
	(players NIL)
	(character-classs NIL) ;Bad English because of parsing issues
	(races NIL)
	(places NIL)
	(monsters NIL)
	(npcs NIL)
	(items NIL)
	(starting-place "")
	(game-manager "")) ;The player in charge of the game

(setf *world* (make-world))

(defun add-game-object (game-object)
	"Add 'game-object' to *world*"
	;; XXX: Very similar in structure to the function set-object-attribute in
	;; game-objects.lisp. Can that be abstracted away into a macro or some such?
	(let ((world-list (build-symbol "world-" (type-of game-object) "s")))
		(eval `(setf (,world-list *world*)
				   (append (,world-list *world*) (list ,game-object))))))
							  
(defun get-game-object (object-type object-name)
	"Return the desired game object"
	(let ((get-world-objects (build-symbol "world-" object-type "s"))
			 (get-object-name (build-symbol object-type "-name")))
		(dolist (object (eval `(,get-world-objects *world*)) NIL)
			(when (equalp (eval `(,get-object-name ,object)) object-name)
				(return object)))))

(defun list-objects (object-type)
	"Return a list of the names of all objects of the specified type"
	(let ((get-world-objects (build-symbol "world-" object-type "s"))
			 (get-object-name (build-symbol object-type "-name"))
			 (name-list NIL))
		(dolist (object (eval `(,get-world-objects *world*)) name-list)
			(setf name-list (cons (eval `(,get-object-name ,object)) name-list)))))

(defun name-world (name)
	"Set the name of the *world*"
	(format t "~&The name of the world is ~A." name)
	(setf (world-name *world*) name)
	NIL)

