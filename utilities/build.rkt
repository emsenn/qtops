#lang racket

(require "../main.rkt")

(provide build-area!)

(define (build-area! t)
  (map (λ (p) (t 'set-procedures! (p t)))
       (list make-name-procedures
             make-description-procedures
             make-content-procedures
             make-exit-procedures)))
