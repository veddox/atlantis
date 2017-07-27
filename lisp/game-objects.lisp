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
	;; Missing 's' in the following list names due to parsing issues
	(neighbour NIL)
	(item NIL)
	(hidden NIL)
	(monster NIL)
	(npc NIL)
	(spawns NIL)
	(command NIL)
	(entry-hook "")
	(exit-hook "")
	(requires "")) ;Can be an ability or an item

(defstruct npc
	(name "")
	(description "")
	(says NIL)
	(sells NIL)
	(buys NIL)
	(quest "")
	(interaction-hook ""))

(defstruct monster
	(name "")
	(description "")
	(health 0)
	(strength 0)
	(dexterity 0)
	(aggression 0)
	(spawn 0)
	(item NIL)
	(weapon "")
	(armour-class 0)
	(attack-hook "")
	(death-msg ""))

(defstruct item
	;; XXX Items containing items?
	(name "")
	(description "")
	(cost 0)
	(weapon)
	(fixed)
	(infinite)
	(pickup-hook "")
	(drop-hook "")
	(command NIL)
	(ability NIL)) ;XXX Remove abilities again?

(defstruct weapon
	(name "")
	(description "")
	(type "")
	(damage 0))

(defstruct quest
	(name "")
	(say-before "")
	(say-after "")
	(proof-item NIL)
	(reward-item NIL)
	(money 0)
	(experience 0)
	(infinite))


(defun set-object-attribute (game-object property value)
	"Set the attribute 'property' of 'game-object' to 'value'"
	;; Here follows Lisp magic :D      (that took ages to get right...)
	;; XXX It's not elegant to call (eval) explicitly, but in this case I can't
	;; find a way to avoid it - I needed a mix between a macro and a function
	(let ((command (build-symbol (type-of game-object) "-" property)))
		(eval `(if (or (null (,command ,game-object))
					   (listp (,command ,game-object)))
				   (setf (,command ,game-object)
					   (append (,command ,game-object) '(,value)))
				   (setf (,command ,game-object) ,value)))))

(defun remove-object-attribute (game-object property value)
	"Remove 'value' from the attribute 'property' in 'game-object'"
	;; Same comment applies as above
	(let ((command (build-symbol (type-of game-object) "-" property)))
		(eval `(cond ((listp (,command ,game-object))
						 (setf (,command ,game-object)
							 (remove-first-if #'(lambda (x) (equalp x ,value))
								 (,command ,game-object))))
				   ((numberp (,command ,game-object))
					   (setf (,command ,game-object) 0))
				   ((stringp (,command ,game-object))
					   (setf (,command ,game-object) ""))
				   (t (setf (,command ,game-object) NIL))))))

(defun objectify-name-list (object-type name-list)
	"Turn all the string names in name-list into instances of the object type"
	;; Basically the inverse of a make-list-function function (cf. util.lisp)
	(let ((objects NIL) (copy-fn (build-symbol "copy-" object-type)))
		(dolist (n name-list objects)
			(if (stringp n)
				(setf objects
					(cons (funcall copy-fn (get-game-object object-type n))
							  objects))
				(setf objects (cons n objects))))))

(defun objectify-place-monsters (place)
	"Objectify all the monsters in this place"
	(let* ((p (if (place-p place) place (get-game-object 'place place))))
		(setf (place-monster p)
			(objectify-name-list 'monster (place-monster p)))
		p))

(defun spawn-monsters (place)
	"Spawn monsters in this location"
	(let ((p (if (stringp place) (get-game-object 'place place) place)))
		(dolist (m (place-spawns p) p)
			(let ((monster (get-game-object 'monster m)))
				(when (< (random 100) (monster-spawn monster))
					(set-object-attribute p 'monster (copy-monster monster)))))))
	
(let ((list-function (make-list-function 'place NIL)))
	(defun list-place-objects (object-type place)
		"Get a list of the names of all the place's objects of this type."
		(funcall list-function object-type place)))

(defun get-object-description (object-name place)
	"Get a description of this object in place (or nil if not there)"
	(let ((p (if (place-p place) place (get-game-object 'place place))))
		(cond ((member object-name (list-place-objects 'item p) :test #'equalp)
				  (item-description (get-game-object 'item object-name)))
			((member object-name (list-place-objects 'monster p) :test #'equalp)
				(monster-description (get-game-object 'monster object-name)))
			((member object-name (list-place-objects 'npc p) :test #'equalp)
				(npc-description (get-game-object 'npc object-name)))
			(t NIL))))
