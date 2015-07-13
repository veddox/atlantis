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
	(weapons NIL)
	(quests NIL)
	(game-functions NIL))

(setf *world* (make-world)) ;XXX Move this to another module?

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

(let ((list-function (make-list-function 'world)))
	(defun list-world-objects (object-type)
		"Get a list of the names of all the world objects of this type."
		(funcall list-function object-type *world*)))

(defun name-world (name)
	"Set the name of the *world*"
	(debugging "~&The name of the world is ~A." name)
	(setf (world-name *world*) name)
	NIL)

(defun load-game (game-file)
	"Load a saved game from disk"
	(with-open-file (g game-file)
		(let ((version-number (read g))
				 (loaded-world (read g)))
			(when (!= version-number ATLANTIS-VERSION :test equal)
				(format t "~&WARNING: The loaded game was saved by a ")
				(format t "different version of Atlantis!"))
			(if (world-p loaded-world)
				(setf *world* loaded-world)
				(error "World file ~A is corrupted!" game-file)))))

(defun save-world (game-file)
	"Save a game to file"
	(with-open-file (g game-file :direction :output)
		(format g "~S~%~S~%" ATLANTIS-VERSION *world*)))
