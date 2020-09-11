#lang racket

(provide make-sight-procedures)

(define ((look-thing t) o)
  (define r "")
  (define R (λ (b) (set! r (string-join (list r b) ""))))
  (when (o 'has-procedure? 'name)
    (R (format "[  ~a  ]\n" (o 'name))))
  (when (o 'has-procedure? 'description)
    (R (format "~a\n" (o 'description))))
  (when (o 'has-procedure? 'exits)
    (R (format "  Exits: ~a\n") (string-join (hash-keys (o 'exits) ", "))))
  (cond
    [(> (string-length r) 0) (t 'message! r)]
    [else (t 'message! "You can't look at that.")]))

(define (make-sight-procedures t)
  (list
   (cons 'look-thing (look-thing t))))
