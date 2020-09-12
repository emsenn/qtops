#lang racket

(provide §)

(define (§ . s)
  (unless (andmap string? s)
    (raise-argument-error '§ "listof string?" s))
  (string-join s ""))
