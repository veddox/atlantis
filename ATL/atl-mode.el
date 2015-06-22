;;;
;;; This is an Emacs mode for writing ATL, the descriptive language
;;; for the Atlantis RPG.
;;;
;;; author: Daniel Vedder
;;; date: 22/05/2015
;;;

(require 'generic-x) ;; we need this apparently - don't ask me why ;-)

(define-generic-mode atl-mode
	'(";") ;; comments
	;; define commands
	'("define-place" "define-item" "define-monster" "define-npc"
		 "define-race" "define-place" "define-weapon"
		 "define-class" "name-world" "load-file" "start-place")
	'() ;; other commands (adjust this?)
	'("\\.atl$") ;; files for which to activate this mode 
	'(linum-mode)   ;; other functions to call
	"An Emacs mode for the ATL game description language" ;; doc string 
)
