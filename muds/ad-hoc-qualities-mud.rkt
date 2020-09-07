#lang racket

(require "../main.rkt")

(define banana (create-thing "banana")) ; #<thing>
(add-quality-to-thing! 'folic-acid banana)
(set-thing-quality! banana 'folic-acid 140)
(thing-quality banana 'folic-acid) ; 140

(define (set-thing-folic-acid! acidic-thing quantity)
 (unless (number? quantity)
  (raise-argument-error 'set-thing-folic-acid!
                        "number?"
                        quantity))
 (set-thing-quality! acidic-thing 'folic-acide quantity #:skip #t))

(set-thing-procedure! banana
                      'set-thing-folic-acid!
                       set-thing-folic-acid!)

(set-thing-quality! banana 'folic-acid "Lots")
