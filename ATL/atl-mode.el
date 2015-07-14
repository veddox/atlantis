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
		 "define-class" "define-player" "define-quest"
		 "define-function" "name-world" "load-file" "load")
	'() ;; other commands (adjust this?)
	'("\\.atl$") ;; files for which to activate this mode 
	'(linum-mode)   ;; other functions to call - doesn't work for some reason
	"An Emacs mode for the ATL game description language" ;; doc string 
)
