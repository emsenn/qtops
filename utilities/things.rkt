#lang racket

(provide make-and-set-procedures!)

(define (make-and-set-procedures! t p)
  (map (λ (o) (t 'set-procedures! (o t)))
       p))
