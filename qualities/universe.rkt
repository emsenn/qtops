#lang racket

(require (prefix-in qtmud: "../engine/main.rkt")
         "universe-thing.rkt")

(provide make-universe-procedures)

(define ((things t)) (list))

(define ((set-things! t) C)
  (t 'set-procedure! 'things (λ () C)))

(define ((add-thing! t) c)
  (t 'set-things! (append (t 'things) (list c))))

(define ((remove-thing t) c)
  (t 'set-things! (remove c (t 'things))))

(define ((create-thing t))
  (define c (qtmud:create-thing))
  (c 'set-procedures! (make-universe-thing-procedures c))
  (c 'set-universe! t)
  (t 'add-thing! c)
  c)

(define (make-universe-procedures t)
  (log-debug "Making universe procedures for ~a."
             (if (t 'has-procedure? 'name) (t 'name) t))
  (list
   (cons 'create-thing (create-thing t))
   (cons 'things (things t))
   (cons 'set-things! (set-things! t))
   (cons 'add-thing! (add-thing! t))))
