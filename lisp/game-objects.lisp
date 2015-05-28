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
	(npc NIL))

;;; WORK IN PROGRESS >>>

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
	;; [And half the magic is gone after being outsourced to build-symbol :-(]
	;; I'm not sure how elegant it is to call (eval) explicitly, but in this
	;; case I couldn't avoid it - I needed a mix between a macro and a function
	(let ((command (build-symbol (type-of game-object) "-" property)))
		;; XXX This following section is rather ugly...
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
				   ;; XXX This is going to give problems with multiple values
				   (setf (,command ,game-object)
					   (remove-if #'(lambda (x) (equalp x ,value))
						   (,command ,game-object)))
				   (setf (,command ,game-object) NIL)))))
