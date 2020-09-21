#lang racket

(provide make-radioactive-procedures)

(define ((radiation t)) (list))
(define ((set-radiation! t) r)
  (t 'set-procedure! 'radiation (λ () r)))

(define (make-radioactive-procedures t)
  (list
   (cons 'radiation (radiation t))
   (cons 'set-radiation! (set-radiation! t))))
