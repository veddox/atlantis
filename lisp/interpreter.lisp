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
(load 'player.lisp)
(load 'world.lisp)


(defun load-atl (atl-file)
	;not yet defined
	NIL
	)

(defun define-place (name)
	(format t "~&Making place ~A" name)
	(make-place :name name))

(defun start-place (place)
	;not yet defined
	NIL
	)

(defun load-atl-file (file-name)
	"Load an ATL source file"
	(do* ((line-nr 0 (1+ line-nr)) (source (load-file file-name))
			 (line (nth line-nr source) (nth line-nr source))
			 (current-object NIL))
		((= line-nr (length source)) NIL)
		(cond ((zerop (length line)) (setf current-object NIL))
			; interpret a define command
			((not (or (eql (aref line 0) #\;)
					  (eql (aref line 0) #\SPACE)
					  (eql (aref line 0) #\TAB)))
				(setf current-object (funcall (symbol-function
												  (read-from-string line))
					; here follows is a kludge to work around a clisp bug (the 
					; :start keyword in read-from-string is not recognized)
					(read-from-string
						(second (cut-string line (find-char #\space line)))))))
			; interpret an option command
			((not (eql (aref line 0) #\;))
				(setf line (string-left-trim '(#\Space #\Tab) line))
				(set-object-attribute current-object (read-from-string line)
					(read-from-string
						(second (cut-string line (find-char #\space line))))))
			(T (format t "~&ERROR: unrecognized syntax on line ~A: '~A~"
				   line-nr line)))))
