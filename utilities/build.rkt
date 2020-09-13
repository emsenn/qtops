#lang racket

(require "../main.rkt"
         "things.rkt")

(provide build-area!
         build-lookable!)

(define (build-lookable! t
                         #:name [name #f]
                         #:description [description #f])
  (make-and-set-procedures!
   t
   (list make-name-procedures
         make-noun-procedures
         make-description-procedures
         make-container-procedures))
  (when name (t 'set-name! name))
  (when description (t 'set-description! description)))

(define (build-area! t)
  (map (λ (p) (t 'set-procedures! (p t)))
       (list make-name-procedures
             make-description-procedures
             make-content-procedures
             make-exit-procedures)))
