#lang racket

(provide make-notable-procedures)

(define ((notable? t)) #t)
(define ((set-notable! t) n)
      (t 'set-procedure! 'notable (λ () n)))

(define (make-notable-procedures t)
  (list
   (cons 'notable? (notable? t))
   (cons 'set-notable! (set-notable! t))))
