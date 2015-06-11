;;;
;;; Atlantis is a framework for creating multi-user dungeon worlds.
;;; This is the Common Lisp implementation.
;;;
;;; This module provides the necessary Lisp networking functions. The actual
;;; server will be implemented in C at some later stage.
;;; Currently, this file should be treated as a mockup.
;;;
;;; Licensed under the terms of the MIT license.
;;; author: Daniel Vedder
;;; date: 30/05/2015
;;;

(defvar *server-address*
	'((ip "127.0.0.1") (port 8888)))

;; XXX Does this need to be a macro? (so the request doesn't have to be quoted)
(defun send-server-request (ip port request)
	"Send a request string to the server"
	;; TODO this is a mockup function
	(server-process-request request))

(defmacro server-send (request)
	"Saves some quoting"
	`(send-server-request
		 (cassoc ip *server-address*) (cassoc port *server-address*)
		 ',request))

(defun server-process-request (request)
	"The game server processes a request"
	;; XXX Is simply calling (eval) on the request good enough?
	;; Or should I come up with a proper networking protocol?
	;; (Considering a possible Python implementation.)
	(break)
	(eval request))

(defun update-world ()
	"Update the world to match the one on the server"
	(setf *world* (server-send *world*)))
