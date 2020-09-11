#lang racket

(provide make-sight-mudsocket-commands)

(define ((make-look-mudsocket-command t) a)
  (cond
    [(hash-empty? a)
     (cond
       [(and (t 'has-procedure? 'container)
             (procedure? (t 'container)))
        (define c (t 'container))
        (cond
          [(c 'has-procedure? 'looked-at-by-thing)
           (c 'looked-at-by-thing t)]
          [else
           (t 'look-thing c)])]
       [else
        (t 'message! "You look around, but you aren't anyplace.")])]
    [(hash-has-key? a "container")
     (t 'message! "Looking inside things doesn't work yet.")]
    [(hash-has-key? a 'line)
     (define l (hash-ref a 'line))
     (define e (list))
     (define E! (λ (q) (set! e (append e q))))
     (when (t 'has-procedure? 'container)
       (define k (t 'container))
       (E! (list k))
       (when (k 'has-procedure? 'contents)
         (E! (k 'contents))))
     (define m ((t 'universe) 'search-things-by-term e l))
     (cond
       [(null? m)
        (t 'message! (format "You cannot see \"~a\"." l))]
       [(= (length m) 1)
        (define m (first m))
        (cond
          [(m 'has-procedure? 'looked-at-by-thing)
           (m 'looked-at-by-thing t)]
          [else
           (t 'look-thing m)])]
       [else
        (t 'message!
           (format "There are multiple matches for \"~a\": ~a."
                   l m))])]))

(define (make-sight-mudsocket-commands t)
  (list
   (cons "look" (make-look-mudsocket-command t))))
