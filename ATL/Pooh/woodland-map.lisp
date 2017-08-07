; The 100 Acre Wood was invented by A.A. Milne for his Winnie-the-Pooh stories.
; This Atlantis world is based on the novels.
;
; This file shows the game map.
;
; author: Daniel Vedder
; date: 31/07/2017

(defun study (player &optional arg)
	"Print out the map"
	(unless (member 'map (extract-elements arg))
		(format t "~&What do you want to study?")
		(return-from study))
	(setf map-string "
..............................[  100 ACRE WOODS  ]..............................
:                                                                              :
:                          Misty forest                      Rapids            :
:                                                                              :
:                                               Bee tree                       :
:               Sandy pit                                                      :
:                                                                              :
:                                                 Rabbit                       :
:       Kanga                      Northern                                    :
:                                   woods                 Rabbit's             :
:                 Stream              |              friends-and-relations     :
:                                     |                                        :
:                           Deep      |       Hill                             :
:                          forest     |                                        :
:                                     |                                        :
: POOH'S       Western ___________ Central _____________ Eastern               :
:  HOME         woods               woods                 woods                :
:                                     |                                        :
:                                     |       Owl                Christopher   :
:             Six pine                |                             Robin      :
:              trees                  |                                        :
:                                     |                                        :
:                                  Southern                                    :
:       Piglet                      woods                                      :
:                                                                              :
:                                                                              :
:                   Floody                                 Bridge              :
:                   place                Eeyore                                :
:..............................................................................:
")
	(format t "~A" map-string))
