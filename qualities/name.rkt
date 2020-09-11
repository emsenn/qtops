#lang racket

(provide make-name-procedures)


(define ((name t)) "thing")
(define ((set-name! t) n)
  (unless (string? n)
    (raise-argument-error 'set-name!
                          "string?"
                          n))
  (t 'set-procedure! 'name (λ () n)))
(define (term=? t)
  (define (name=? l)
    (or
     (string=? (string-downcase (t 'name)) l)
     (string=? (t 'name) l)))
  (define real-term=?
    (cond
      [(t 'has-procedure? 'term=?)
       (define cterm=? (t 'term=?))
       (λ (l)
         (ormap (λ (p) (p l))
                (list cterm=? name=?)))]
      [else
       name=?]))
  (λ (l)
    (real-term=? l)))


(define (make-name-procedures t)
  (list
   (cons 'name (name t))
   (cons 'set-name! (set-name! t))
   (cons 'term=? (term=? t))))
