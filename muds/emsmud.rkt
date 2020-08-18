#lang racket

(require "../qtmud.rkt"
         "../components/areas.rkt"
         "../components/animation.rkt"
         "../components/containers.rkt"
         "../components/mudsocket.rkt"
         "../components/talker.rkt"
         "../components/user-accounts.rkt")<
(define emsverse (make-universe "emsVerse"))
(run-universe-logger (make-universe-logger emsverse))
(define make-thing (make-thing-maker-for-universe emsverse))
(define clearing (make-thing "grassy clearing"))
(define garden (make-thing "small garden"))
(define home (make-thing "emsenn's home"))
(define emsverse-map (make-map-from-areas
 `((clearing . ,clearing) (garden . ,garden) (home . ,home))))
(set-thing-area-exits! clearing `(("garden" . ,garden) ("home" . ,home)))
(set-thing-area-exits! garden `(("clearing" . ,clearing)))
(set-thing-area-exits! home `(("out" . ,clearing)))
(set-thing-area-description! clearing
			     "This is a grassy field, roughly 100 meters across, located in a temperate decidious forest. There is a small structure made from coppice poles and woven panels of bark constructed in the western part of the clearing, Near the middle of the clearing is a garden, just south of a large mulberry tree.")
(set-thing-area-description! garden
			     "This is a garden.")
(set-thing-area-description! home
			     "This is a small structure made of young pine tree trunks driven into the ground to form a cone, with long planes of wood bark woven between the beams.")
(add-event-to-universe-schedule! (make-mudsocket-tick-event-for-universe emsverse) emsverse)
(add-event-to-universe-schedule! (add-user-accounts-to-universe!-event "emsmud-user-accounts.rktd") emsverse)
(set-universe-procedure! emsverse 'handle-mudsocket-connection
 (λ (connected-thing)
  (add-string-to-thing-mudsocket-output-buffer! "You've connected to emsMUD, a small virtual world created by emsenn. Please, enter your [desired] user-name and press ENTER to connect [or register a new account]." connected-thing)))

(set-universe-procedure!
 emsverse 'handle-mudsocket-login
 (λ (connected-thing)
   (add-string-to-thing-mudsocket-output-buffer!
    (format
     "Welcome to emsMUD, ~a. Interact by typing commands and pressing ENTER. Try starting with the \"help\" command.\n\nYou form, a fully-realized adult homo sapien, clothed in buckskin, into the universe."
     (thing-name connected-thing))
    connected-thing)
   (add-thing-to-thing-contents! connected-thing clearing)
   (add-new-mudsocket-commands-to-thing!
    (list
     (cons "look" (make-look-mudsocket-command-for-thing connected-thing))
     (cons "move" (make-move-mudsocket-command-for-thing connected-thing)))
    connected-thing)))
(define emsmud (run-universe emsverse))
