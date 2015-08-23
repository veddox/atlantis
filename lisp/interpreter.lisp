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

;; A list of ATL language constructs
;; (Note: not complete - each (defcommand) appends to this list)
(defvar *atl-commands* '(load-file name-world))

(defun build-define-command (object-type)
	"Build a new define command function for the specified object type"
	#'(lambda (name)
		  (debugging "~&Making ~A ~A"
			  (string-downcase (to-string object-type)) name)
		  (funcall (build-symbol "make-" object-type) :name name)))

(defmacro defcommand (command-name object-type)
	;; XXX Macros should not have side-effects?
	(setf *atl-commands* (cons command-name *atl-commands*))
	`(defun ,command-name (name)
		 (funcall ,(build-define-command object-type) name)))

;;; Syntax for new define commands:
;;; (defcommand <command-name> <created-object>)

(defcommand define-place place)
(defcommand define-race race)
(defcommand define-class character-class)
(defcommand define-player player)
(defcommand define-monster monster)
(defcommand define-weapon weapon)
(defcommand define-item item)
(defcommand define-npc npc)
(defcommand define-quest quest)


(let ((world-directory NIL)
		 (loaded-files NIL))
	(defun load-file (file-name)
		"Load and interpret an ATL source file or a Lisp extension file"
		;; XXX Should this be split up into several functions?
		;; save/load the current working directory
		(if (null (pathname-directory file-name))
			(setf file-name (make-pathname
								:directory world-directory
								:name (pathname-name file-name)
								:type (pathname-type file-name)))
			(progn
				(setf world-directory (pathname-directory file-name))
				(setf file-name (parse-namestring file-name))))
		;; check if this file has already been loaded
		(if (member file-name loaded-files :test #'equalp)
			(return-from load-file)
			(setf loaded-files (cons file-name loaded-files)))
		;; check if this is a Lisp extension file
		(when (equalp (pathname-type file-name) "lisp")
			(load file-name)
			(return-from load-file))
		;; otherwise, parse the ATL file
		(do* ((line-nr 0 (1+ line-nr)) (source (load-text-file file-name))
				 (line (nth line-nr source) (nth line-nr source))
				 (current-object NIL))
			((= line-nr (length source)) NIL)
			;; concatenate string arguments spanning several lines
			(while (= (count-instances #\" line) 1)
				(incf line-nr)
				(setf line (concatenate 'string line (to-string #\Newline)
							   (trim-whitespace (nth line-nr source)))))
			(cond ((zerop (length line))
					  (when current-object (add-game-object current-object))
					  (setf current-object NIL))
				((eql (aref line 0) #\;)) ;Comments are ignored
			    ;; interpret a define command
				((not (or (eql (aref line 0) #\;)
						  (eql (aref line 0) #\SPACE)
						  (eql (aref line 0) #\TAB)))
					(let ((def-cmd (read-from-string line)))
						(if (member def-cmd *atl-commands*)
							(setf current-object
								(funcall def-cmd
									;; clisp doesn't recognize the :start
									;; keyword in read-from-string
									(read-from-string (second (cut-string line
												(position #\space line))))))
							(error "~&ERROR: unrecognized syntax: '~A'" line))))
			    ;; interpret an option command
				((or (eql (aref line 0) #\Space)
					 (eql (aref line 0) #\Tab))
					(let ((options (extract-elements line)))
						(case (length options)
							(1 (set-object-attribute current-object
								   (first options) T))
							(2 (set-object-attribute current-object
								   (first options) (second options)))
							;; FIXME gives problems with lines like this:
							;; "    ;commented"
							(t (error "~&ERROR: too many arguments: '~A'"
								   line)))))
				(T ;; can't happen
					(error "~&ERROR: unrecognized syntax: '~A'" line))))))
				
