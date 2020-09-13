#lang racket

(provide make-and-set-procedures!)

(define (make-and-set-procedures! t p)
  (map (λ (o) (t 'set-procedures! (o t)))
       p))

(define (filter-by-quality T q)
  (filter procedure?
          (map
           (λ (t)
             (if (t 'has-procedure? q)
                 t
                 #f)))))
