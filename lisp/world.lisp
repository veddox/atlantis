;;;
;;; Atlantis is a framework for creating text-adventure worlds.
;;; This is the Common Lisp implementation.
;;;
;;; The world stores the current state of the game.
;;;
;;; Licensed under the terms of the GNU GPLv3.
;;; author: Daniel Vedder
;;; date: 15/05/2015
;;;


(defvar *world*)


;; The world is basically a list of struct instances representing
;; each object created in this game

(defstruct world
	(player-name "")
	(main-character "")
	(players NIL)
	(places NIL)
	(monsters NIL)
	(npcs NIL)
	(items NIL)
	(weapons NIL)
	(quests NIL)
	(extension-files NIL))

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

(defun load-game (game-file)
	"Load a saved game from disk"
	;; If only the player name was passed, expand it into a file path
	(unless (search ".world" game-file)
		(setf game-file (concatenate 'string "../saves/" game-file ".world")))
	(with-open-file (g game-file)
		;; Compare versions
		(unless (equal (read g) ATLANTIS-VERSION)
			(format t "~&WARNING: The loaded game was saved by a ")
			(format t "different version of Atlantis!")
			(format t "~&Press ENTER to continue.")
			(read-line))
		;; Read the actual world
		(let ((loaded-world (read g)))
			(if (world-p loaded-world)
				(progn (setf *world* loaded-world)
					(dolist (lisp-file (world-extension-files *world*))
						(load lisp-file)))
				(error "World file ~A is corrupted!" game-file)))))

(defun save-world (&optional game-file)
	"Save a game to file"
	(unless game-file
		(setf game-file (concatenate 'string "../saves/"
							(world-player-name *world*) ".world")))
	(with-open-file (g game-file :direction :output)
		(format g "~S~%~S~%" ATLANTIS-VERSION *world*)))

(defun save-state (var &optional (value T))
	"Save the value of a game variable in a special game item."
	;; This is intended for game developers who need to save
	;; state between game sessions.
	(let ((aso (get-game-object 'item "Atlantis Storage Object")))
		(when (null aso)
			(setf aso (make-item :name "Atlantis Storage Object"
						  :description '()))
			(add-game-object aso))
		(if (member var (keys (item-description aso)))
			(setf (cassoc var (item-description aso)) value)
			(setf (item-description aso)
				(append (item-description aso) (list (list var value)))))))

(defun get-state (var)
	"Return the saved value of the given variable."
	;; This is intended for game developers who need to save
	;; state between game sessions.
	(let ((aso (get-game-object 'item "Atlantis Storage Object")))
		(unless (or (null aso) (not (member var (keys (item-description aso)))))
			(cassoc var (item-description aso)))))

