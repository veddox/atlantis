; The 100 Acre Wood was invented by A.A. Milne for his Winnie-the-Pooh stories.
; This Atlantis world is based on the novels.
;
; This file holds Lisp functions for hooks and the like, to expand on the
; capabilities of the basic Atlantis framework.
;
; author: Daniel Vedder
; date: 20/07/2017

(defun eat (player &optional arg)
	"Allow the player to eat something."
	(cond ((null arg) (format t "~&What do you want to eat?"))
		((and (equalp arg "berries")
			 (member "berries" (player-item player) :test #'equalp))
			(format t "~&Mmmh, these berries are really delicious!")
			(remove-object-attribute player 'item "berries"))
		((and (or (equalp arg "hunny") (equalp arg "honey"))
			 (member "Hunny" (player-item player) :test #'equalp))
			(format t "~&The honey looks incredibly tempting, but perhaps you should save it for later."))
		(T (format t "~&You can't eat that!"))))

(defun kanga-healing (player)
	"If the player is hurt, Kanga looks after him."
	(when (< (player-health player) (player-max-health player))
		(format t "~&KANGA: Oh my dear, you look hurt! Here, let me take care of you.")
		(format t "~&~%Kanga bandages your wounds. You feel better.")
		(setf (player-health player) (player-max-health player))))
