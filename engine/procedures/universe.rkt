#lang racket

(require "../structs/mud.rkt"
         "creation.rkt")

(define (increment-universe-time! incremented-universe
					[addition 1])
  (unless (integer? addition)
    (raise-argument-error 'increment-universe-tick-count!
                          "integer?" addition))
  (log-debug "Moving ~a forward ~a moment~a in time."
             (universe-name incremented-universe)
             addition
             (cond [(> 1 addition) "s "]
                   [else " "]))
  (set-universe-time! incremented-universe
                      (+ (universe-time incremented-universe)
                         addition)))

(define (add-event-to-universe! new-event
                                         changed-universe)
  (unless (procedure? new-event)
    (raise-argument-error 'add-event-to-universe-schedule!
                          "procedure?" new-event))
  (unless (universe? changed-universe)
    (raise-argument-error 'add-event-to-universe-schedule!
                          "universe?" changed-universe))
  (log-debug "Adding new event to ~a's schedule:\n   ~a"
             (universe-name changed-universe)
             new-event)
  (define current-schedule (universe-schedule changed-universe))
  (set-universe-schedule! changed-universe
                          (append current-schedule
                                  (list new-event))))

(define (add-thing-to-universe! new-thing
                                changed-universe)
  (unless (thing? new-thing)
    (raise-argument-error 'add-thing-to-universe-things!
                          "thing?"
                          new-thing))
  (unless (universe? changed-universe)
    (raise-argument-error 'add-thing-to-universe-things!
                          "universe?"
                          changed-universe))
  (set-universe-things! changed-universe
                        (append (universe-things changed-universe)
                                (list new-thing))))

(define (universe-has-procedure? queried-universe
                                 queried-procedure)
  (unless (universe? queried-universe)
    (raise-argument-error 'universe-has-procedure?
                          "universe?"
                          queried-universe))
  (unless (symbol? queried-procedure)
    (raise-argument-error 'universe-has-procedure?
                          "symbol?"
                          queried-procedure))
  (hash-has-key? (universe-procedures queried-universe)
                 queried-procedure))

(define (universe-procedure queried-universe queried-procedure)
  (unless (universe? queried-universe)
    (raise-argument-error 'universe-procedure
                          "universe?"
                          queried-universe))
  (unless (symbol? queried-procedure)
    (raise-argument-error 'universe-procedure
                          "symbol?"
                          queried-procedure))
  (hash-ref (universe-procedures queried-universe)
            queried-procedure))

(define (set-universe-procedure! changed-universe
                                 new-procedure-key
                                 new-procedure)
  (unless (universe? changed-universe)
    (raise-argument-error 'set-universe-procedure!
                          "universe?"
                          changed-universe))
  (unless (symbol? new-procedure-key)
    (raise-argument-error 'set-universe-procedure!
                          "symbol?"
                          new-procedure-key))
  (unless (procedure? new-procedure)
    (raise-argument-error 'set-universe-procedure!
                          "procedure?"
                          new-procedure))
  (hash-set! (universe-procedures changed-universe)
             new-procedure-key
             new-procedure))

(define (add-procedures-to-universe! procedures-list target-universe)
  (unless (and (list? procedures-list)
               (not (null? procedures-list))
               (andmap procedure? procedures-list))
    (raise-argument-error 'add-procedures-to-universe!
                          "listof procedure?" procedures-list))
  (unless (universe? target-universe)
    (raise-argument-error 'add-procedures-to-universe!
                          "universe?" target-universe))
  (define length-of-procedures-list
    (length procedures-list))
  (define target-universe-name
    (universe-name target-universe))
  (define target-universe-procedures
    (universe-procedures target-universe))
  (log-debug "Adding ~a new procedure~a to ~a."
             length-of-procedures-list
             (cond [(> 1 length-of-procedures-list) "s "]
                   [else " "]))
  (map (λ (added-procedure)
         (set-universe-procedure! target-universe
                                  (car added-procedure)
                                  (cdr added-procedure)))
       procedures-list))


(module+ test
  (require rackunit)
  (require "logging.rkt")
  (define qtmud-universe-procedures-tests
    (test-suite
     "Tests for qtMUD universe procedures."
     (run-logger (create-logger 'qtMUD-Tests
                                'debug))
     (test-case
         "Manipulating time."
       (define averse (create-universe))
       (define current-time (universe-time averse))
       (increment-universe-time! averse)
       (check-eq? (+ current-time 1)
                  (universe-time averse)))
     (test-case
         "Scheduling a new event"
       (define bverse (create-universe))
       (add-event-to-universe! (λ () (void))
                               bverse)
       (check-eq? 1 (length (universe-schedule bverse))))
     (test-case
         "Adding a thing to a universe."
       (define cverse (create-universe))
       (define athing (create-thing))
       (add-thing-to-universe! athing cverse)
       (check-eq? 1 (length (universe-things cverse)))
       (check-eq? athing
                  (first (universe-things cverse)))))))
