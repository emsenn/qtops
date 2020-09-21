#lang racket

(provide make-moisture-procedures)

(define ((moisture t)) 0)
(define ((set-moisture! t) m)
  (when (< m 0) (set! m 0))
  (t 'set-procedure! 'moisture (λ () m)))
(define ((increment-moisture! t) [m 1])
  (t 'set-moisture! (+ (t 'moisture) m)))

(define (make-moisture-procedures t)
  (list
   (cons 'moisture (moisture t))
   (cons 'set-moisture! (set-moisture! t))
   (cons 'increment-moisture! (increment-moisture! t))))
