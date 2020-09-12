#lang racket

(provide make-sight-procedures)

(define ((look-thing t) o)
  (define r "")
  (define R (λ (b) (set! r (string-join (list r b) ""))))
  (define c
    (cond
      [(o 'has-procedure? 'contents)
       (filter values
               (map (λ (t) (when (t 'has-procedure? 'name) t))
                    (o 'contents)))]
      [else #f]))
  (when (o 'has-procedure? 'name)
    (R (format "[  ~a  ]\n" (o 'name))))
  (when (o 'has-procedure? 'description)
    (R (format "~a\n" (o 'description))))
  (when (and (o 'has-procedure? 'exits)
             (not (null? (hash-keys (o 'exits)))))
    (R (format "  Exits: ~a\n"
               (string-join (hash-keys (o 'exits)) ", "))))
  (when c
    (R (format "Contents: ~a\n"
               (string-join (map (λ (t) (t 'name)) c) ", "))))
  (cond
    [(> (string-length r) 0) (t 'message! r)]
    [else (t 'message! "You can't look at that.")]))

(define (make-sight-procedures t)
  (list
   (cons 'look-thing (look-thing t))))
