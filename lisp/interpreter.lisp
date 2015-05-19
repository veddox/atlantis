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


(defun define-place (name)
	(format t "~&Making place ~A" name)
	(make-place :name name))

(defun start-place (place)
	;not yet defined
	)

(let ((world-directory NIL))
	(defun load-file (file-name)
		"Load and interpret an ATL source file"
		; save/load the current working directory
		(if (null (pathname-directory file-name))
			(setf file-name (make-pathname
								:directory world-directory
								:name (pathname-name file-name)
								:type (pathname-type file-name)))
			(setf world-directory (pathname-directory file-name)))
		; parse the ATL file
		(do* ((line-nr 0 (1+ line-nr)) (source (load-text-file file-name))
				 (line (nth line-nr source) (nth line-nr source))
				 (current-object NIL))
			((= line-nr (length source)) NIL)
			;concatenate string arguments spanning several lines
			(while (= (count-vector-instances #\" line) 1)
				(incf line-nr)
				(setf line (concatenate 'string line (nth line-nr source))))
			(cond ((zerop (length line))
					  ;; TODO
					  ;(when current-object (add-game-object current-object))
					  (setf current-object NIL))
				((eql (aref line 0) #\;)) ;Comments are ignored
			    ; interpret a define command
				((not (or (eql (aref line 0) #\;)
						  (eql (aref line 0) #\SPACE)
						  (eql (aref line 0) #\TAB)))
					(setf current-object (funcall (symbol-function
													  (read-from-string line))
					     ; here follows a kludge to work around a clisp bug (the
					     ; :start keyword in read-from-string is not recognized)
						 (read-from-string (second
							   (cut-string line (find-char #\space line)))))))
			    ; interpret an option command
				((or (eql (aref line 0) #\Space)
					 (eql (aref line 0) #\Tab))
					(setf line (string-left-trim '(#\Space #\Tab) line))
					(set-object-attribute current-object (read-from-string line)
						(read-from-string
							(second (cut-string line (find-char #\space line))))))
				(T (format t "~&ERROR: unrecognized syntax on line ~A: '~A'"
					   ;can't happen
					   (1+ line-nr) line))))))
