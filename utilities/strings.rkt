#lang racket

(provide §)

(define (§ . S)
  (λ ()
    (define r "")
     (map
      (λ (s)
        (set! r
              (string-append
               r (if (string? s) s
                     (format "~a" (s))))))
      S)
     r))
