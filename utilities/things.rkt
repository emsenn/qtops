#lang racket

(provide make-and-set-procedures!
         filter-by-quality
         apply-quality!
         apply-qualities!
         apply-procedures!)

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

(define ((apply-quality! t) k p)
  (t 'without-procedure k
     (t 'set-procedures! (p t))))

(define (apply-qualities! t Q)
  (map (λ (q) (apply (apply-quality! t) q)) Q))

(define (apply-procedures! t P)
  (map (λ (p) (apply (λ (k v) ((t 'with-procedure k) v)) p)) P))
