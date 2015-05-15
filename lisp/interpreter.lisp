;;;
;;; Atlantis is a framework for creating multi-user dungeon worlds.
;;; This is the Common Lisp implementation.
;;;
;;; The interpreter file loads an ATL source file and parses it.
;;;
;;; Licensed under the terms of the MIT license
;;; author: Daniel Vedder
;;; date: 09/05/2015
;;;

(load 'util.lisp)
(load 'game-objects.lisp)
(load 'world.lisp)


(defun define-place (name)
	(format t "~&Making place ~A" name)
	(make-place :name name))

(defun start-place (place)
	;not yet defined
	)

(defun load-atl-file (file-name)
	"Load an ATL source file"
	(do* ((line-nr 0 (1+ line-nr)) (source (load-file file-name))
			 (line (nth line-nr source) (nth line-nr source))
			 (current-object NIL))
		((= line-nr (length source)) NIL)
		(unless (or (zerop (length line))
					(eql (aref line 0) #\;))
					(eql (aref line 0) #\SPACE)
					(eql (aref line 0) #\TAB))
			; interpret a define command
			(funcall (symbol-function (read-from-string line))
				; here follows is a kludge to work around a clisp bug (the 
				; :start keyword in read-from-string is not recognized)
				(read-from-string
					(second (cut-string line (find-char #\space line)))))))
