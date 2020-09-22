#lang racket

(provide make-animate-procedures)

(define ((animations t)) (list))
(define ((set-animations! t) A) (t 'set-procedure! 'animations
                                   (λ () A)))
(define ((add-animations! t) A)
  (t 'set-animations! (append (t 'animations) A)))
(define ((add-animation! t) a)
  (t 'add-animations! (list a)))


(define ((animate t))
  (define i 120)
  (map
   (λ (a)
     (define p (t 'procedure (car a)))
     (define f (let ([w (car (cdr a))]) (if (procedure? w) (w) w)))
     (define c (let ([w (car (cdr (cdr a)))])
                 (if (procedure? w) (w) w)))
     (when (>= i f) (set! i f))
     (when (<= c 100) ((t 'universe) 'schedule-event! p f)))
   (t 'animations))
  ((t 'universe) 'schedule-event! (t 'procedure 'animate) i))


(define (make-animate-procedures t)
  (list
   (cons 'animations (animations t))
   (cons 'set-animations! (set-animations! t))
   (cons 'add-animations! (add-animations! t))
   (cons 'add-animation! (add-animation! t))
   (cons 'animate (animate t))))
