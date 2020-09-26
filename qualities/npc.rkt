#lang racket

(require "notable.rkt"
         "pronouns.rkt")


(provide <>npc)

(define (<>npc t #:pronouns [pronouns #f])
  (unless (t 'has-procedure? 'notable?)
    (t 'set-procedures! (>>make-notable-procedures t)))
  (if pronouns
      (t 'set-procedures! (pronouns t))
      (t 'set-procedures! (>>make-e-pronoun-procedures t)))
  t)
