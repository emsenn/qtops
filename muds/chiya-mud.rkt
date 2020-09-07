#lang racket

;; first, load what we need from the qtMUD code.
(require "../qtmud/main.rkt"
         "../qtmud/library/animation-for-mudsocket.rkt"
         "../qtmud/library/containers.rkt"
         "../qtmud/library/descriptions.rkt"
         "../qtmud/library/exits.rkt"
         "../qtmud/library/mudsocket.rkt")

;; create Chiya's Universe
(define chiya-verse (create-universe "Chiya's Universe"))

;; and set up a (create-thing) procedure that creates things
;; into the Universe.
(define create-thing
  (create-thing-creator-for-universe chiya-verse))

;; next we start to change the universe, by adding
;; procedures for handling certain qualities.
(add-container-procedures-to-universe! chiya-verse)
(add-description-procedures-to-universe! chiya-verse)
(add-exit-procedures-to-universe! chiya-verse)

;; then we change the universe by creating things!
(define wetlands (create-thing "Chiya's Wetlands"))

;; let's make those wetlands interesting
(add-qualities-to-thing!
 '(contents description exits)
 wetlands)

(set-thing-quality!
 wetlands 'description
 "Ramin trees nearly competely shadow the ground here, a dark brown humus tinged with red. There are a few small pools of black water, churning invisibly with life.")

(define ramin-trees (create-thing "ramin trees"))
(add-qualities-to-thing!
 '(container description)
 ramin-trees)
(set-thing-quality!
 ramin-trees 'description
 "Thick-trunked trees stretch upward from the ground, some maybe 60 or 70 meters tall. The trunks are covered in long horizontal strips of thick red bark, which has fallen in places. In other places, its been covered over by a thick moss.")
(set-thing-adjectives!
 ramin-trees '("thick-trunked" "thick" "tall" "mossy" "ramin"))
(move-thing-into-thing! ramin-trees wetlands)

(add-event-to-universe!
 (make-mudsocket-tick-event-for-universe chiya-verse)
 chiya-verse)

(set-universe-procedure!
 chiya-verse 'handle-mudsocket-connection
 (λ (logged-in-thing)
   (add-quality-to-thing! 'container logged-in-thing)
   (add-keyvalues-to-thing-quality!
    (list
     (cons "look"
           (make-look-mudsocket-command-for-thing logged-in-thing))
     (cons "move"
           (make-move-mudsocket-command-for-thing logged-in-thing)))
    logged-in-thing 'mudsocket-commands)
   ((universe-procedure chiya-verse
                        'move-thing-into-thing!)
    logged-in-thing wetlands)))

(run-universe chiya-verse)
