#lang racket

(provide make-universe-thing-procedures)

(define ((universe t))
  (void))
(define ((set-universe! t) u)
  (t 'set-procedure! 'universe (λ () u)))

(define (make-universe-thing-procedures t)
  (list
   (cons 'universe (universe t))
   (cons 'set-universe! (set-universe! t))))
