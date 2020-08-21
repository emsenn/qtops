#lang racket

(require "../qtmud.rkt"
	 "../components/mudsocket.rkt")

(define test-universe (make-universe "Testverse"))

(run-universe-logger (make-universe-logger test-universe))

(add-event-to-universe-schedule!
 (make-mudsocket-tick-event-for-universe test-universe)
 test-universe)

(define test-mud (run-universe test-universe))
