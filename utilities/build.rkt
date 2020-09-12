#lang racket

(require "../main.rkt"
         "things.rkt")

(provide build-area!
         build-lookable!)

(define (build-lookable! t)
  (make-and-set-procedures!
   t
   (list make-name-procedures
         make-description-procedures
         make-container-procedures))
  (t 'set-description! "This is a thing."))

(define (build-area! t)
  (map (λ (p) (t 'set-procedures! (p t)))
       (list make-name-procedures
             make-description-procedures
             make-content-procedures
             make-exit-procedures)))
