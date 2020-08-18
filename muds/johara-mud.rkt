#lang racket

(require "../qtmud.rkt"
         "../components/mudsocket.rkt"
         "../components/talker.rkt"
         "../components/user-accounts.rkt")

(define joharaverse (make-universe "Joharaverse"))
(run-universe-logger (make-universe-logger joharaverse))
(set-universe-procedure!
 joharaverse 'handle-mudsocket-connection
 (λ (connected-thing)
   (add-string-to-thing-mudsocket-output-buffer!
   "Welcome to Johara settlement, weary traveler. Enter your [desired] user-name and press ENTER to connect [or register a new account]."
   connected-thing))
(set-universe-procedure!
 joharaverse 'handle-mudsocket-login
 (λ (connected-thing)
   (add-string-to-thing-mudsocket-output-buffer!
   "You rise up from the soil of Johara Forest, just outside the settlement, and wander inside."
   connected-thing))
(add-event-to-universe-schedule! (make-mudsocket-tick-event-for-universe joharaverse) joharaverse)
(add-event-to-universe-schedule! (add-user-accounts-to-universe!-event) joharaverse)
(define johara-mud (run-universe joharaverse))
