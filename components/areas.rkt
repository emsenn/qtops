#lang racket

(require "../qtmud.rkt")

(provide add-area-procedures-to-universe!)

(define (set-thing-area-exits! changed-thing new-exits)
  (hash-set! (thing-qualities changed-thing) 'area-exits
	     (make-hash new-exits)))

(define (add-area-procedures-to-universe! target-universe)
  (set-universe-procedure!
   target-universe
   'set-thing-area-exits! set-thing-area-exits!))
