#lang racket

(provide make-admin-mudsocket-commands)

(define ((make-set-name-mudsocket-command t) a)
  (cond
    [(hash-has-key? a 'line)
     (t 'set-name! (hash-ref a 'line))
     (t 'message! (format "You set your name to ~a" (t 'name)))]
    [else
     (t 'message! "You need to specify a new name.")]))

(define ((make-set-description-mudsocket-command t) a)
    (cond
    [(hash-has-key? a 'line)
     (t 'set-description! (hash-ref a 'line))
     (t 'message! (format "You set your name to ~a" (t 'description)))]
    [else
     (t 'message! "You need to specify a new name.")]))

(define (make-admin-mudsocket-commands t)
  (list
   (cons "!set-name" (make-set-name-mudsocket-command t))
   (cons "!set-description" (make-set-description-mudsocket-command t))))
