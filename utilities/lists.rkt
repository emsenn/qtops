#lang racket

(provide random-element)

(define (random-element given-list)
  (list-ref given-list (random (length given-list))))
