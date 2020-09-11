#lang racket

(provide make-description-procedures)


(define ((description t)) "This is a thing.")
(define ((set-description! t) n)
  (unless (string? n)
    (raise-argument-error 'set-name!
                          "string?"
                          n))
  (t 'set-procedure! 'description (λ () n)))

(define (make-description-procedures t)
  (list
   (cons 'description (description t))
   (cons 'set-description! (set-description! t))))
