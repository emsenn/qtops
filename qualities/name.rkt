#lang racket

(provide make-name-procedures)


(define ((name t)) "thing")
(define ((set-name! t) n)
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
  (log-debug "Making name procedures for something.")
              ;; hard to say what until we name it!
  (list (cons 'name (name t))
        (cons 'set-name! (set-name! t))
        (cons 'name=? (name=? t))))
