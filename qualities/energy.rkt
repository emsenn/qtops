#lang racket

(provide make-energy-procedures)

(define ((energy t)) 0)
(define ((set-energy! t) m)
  (when (< m 0) (set! m 0))
  (t 'set-procedure! 'energy (λ () m)))
(define ((increment-energy! t) [m 1])
  (t 'set-energy! (+ (t 'energy) m)))

(define (make-energy-procedures t)
  (list
   (cons 'energy (energy t))
   (cons 'set-energy! (set-energy! t))
   (cons 'increment-energy! (increment-energy! t))))
