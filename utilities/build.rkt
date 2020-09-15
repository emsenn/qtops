#lang racket

(require "../main.rkt"
         "things.rkt")

(provide »area
         »lookable
         »npc)

(define (»area t
               #:name [name #f]
               #:description [description #f]
               #:contents [contents #f])
  (make-and-set-procedures!
   t (list make-name-procedures
           make-description-procedures
           make-content-procedures
           make-exit-procedures))
  (when name (t 'set-name! name))
  (when description (t 'set-description! description))
  (when contents
    (map
     (λ (r)
       ((r ((t 'universe) 'create-thing)) 'move-thing! t))
     contents))
  t)


(define (»lookable t
                    #:name [name #f]
                    #:description [description #f]
                    #:nouns [nouns #f]
                    #:adjectives [adjectives #f])
  (make-and-set-procedures!
   t
   (list make-name-procedures
         make-noun-procedures
         make-description-procedures
         make-container-procedures))
  (when name (t 'set-name! name))
  (when description (t 'set-description! description))
  (when nouns (t 'add-nouns! nouns))
  (when adjectives (t 'add-adjectives! adjectives))
  t)

(define (»npc t
              #:name [name #f]
              #:description [description #f]
              #:nouns [nouns #f]
              #:adjectives [adjectives #f]
              #:pronouns [pronouns #f])
  (»lookable t #:name name #:description description
             #:nouns nouns #:adjectives adjectives)
  (t 'set-procedures! (make-notable-procedures t))
  (if pronouns
      (t 'set-procedures! (pronouns t))
      (t 'set-procedures! (make-e-pronoun-procedures t)))
  t)
