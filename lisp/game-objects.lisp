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
	(player NIL)
	(item NIL)
	(monster NIL)
	(npc NIL)
	(dark NIL))

;;; WORK IN PROGRESS >>>

(defstruct npc
	(name "")
	(description "")
	(says "")
	(sells NIL)
	(quest ""))

(defstruct monster
	(name "")
	(description "")
	(health 0)
	(strength 0)
	(dexterity 0)
	(aggression 0)
	(spawn-probability 0)
	(weapon "")
	(armour-class 0))

(defstruct item
	(name "")
	(description "")
	(cost 0)
	(weapon "no")
	(function NIL))

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
	(experience 0))

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
		(eval `(if (listp (,command ,game-object))
				   (setf (,command ,game-object)
					   (remove-first-if #'(lambda (x) (equalp x ,value))
						   (,command ,game-object)))
				   ;; TODO set numbers to 0, strings to ""
				   (setf (,command ,game-object) NIL)))))

(defun objectify-name-list (object-type name-list)
	"Turn all the string names in name-list into instances of the object type"
	;; Basically the inverse of a make-list-function function (cf. util.lisp)
	(let ((objects NIL))
		(dolist (n name-list objects)
			(setf objects (cons (get-game-object object-type n) objects)))))

(defun objectify-place-monsters (place)
	"Objectify all the monsters in this place"
	;; XXX This introduces a side effect!
	(let* ((p (if (place-p place) place (get-game-object 'place place)))
			  (monster-list (objectify-name-list 'monster (place-monster p))))
		(if (monster-p (first (place-monster p)))
			(return-from objectify-place-monsters place)
			(setf (place-monster p) monster-list))))
		
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
		
