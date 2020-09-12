#lang racket

(require "../utilities/lists.rkt")

(provide make-animate-procedures)

(define ((animate t a f))
  ((random-element a))
  ((t 'universe) 'schedule-event!
                 (t 'procedure 'animate) f))


(define (make-animate-procedures t a)
  (list
   (cons 'animate (animate t a))))
