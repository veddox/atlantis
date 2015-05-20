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

(defun add-game-object (game-object)
	"Add 'game-object' to *world*"
	;; XXX: Very similar in structure to the function set-object-attribute in
	;; game-objects.lisp. Can that be abstracted away into a macro or some such?
	(let ((world-list (read-from-string
						  (concatenate 'string "world-"
							  (symbol-name (type-of game-object)) "s"))))
		(eval `(setf (,world-list *world*)
				   (append (,world-list *world*) (list ,game-object))))))
							  
; TODO
(defun get-game-object (object-name))

