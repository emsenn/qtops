#lang racket

(require "mass.rkt"
         "notable.rkt")

(provide <>object)

(define (<>object t)
  (unless (t 'has-procedure? 'notable?)
    (t 'set-procedures! (>>make-notable-procedures t)))
  (unless (t 'has-procedure? 'mass)
    (t 'set-procedures! (>>make-mass-procedures t)))
  t)
