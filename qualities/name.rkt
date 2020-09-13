#lang racket

(provide make-name-procedures)


(define ((name t)) "thing")
(define ((set-name! t) n)
  (unless (string? n)
    (raise-argument-error 'set-name!
                          "string?"
                          n))
  (t 'set-procedure! 'name (λ () n)))
(define (name=? t)
  (define (real-name=? l)
    (or (string=? (t 'name) l)
        (string=? (string-downcase (t 'name)) l)))
  (cond
    [(t 'has-procedure? 'term=?)
     (define old-term=? (t 'procedure 'term=?))
     (t 'set-procedure! 'term=?
        (λ (l) (or (old-term=? l) (t 'name=?))))]
    [else
     (t 'set-procedure! 'term=? real-name=?)])
  real-name=?)

(define (make-name-procedures t)
  (list
   (cons 'name (name t))
   (cons 'set-name! (set-name! t))
   (cons 'name=? (name=? t))))