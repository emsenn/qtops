#lang racket

(require "../engine/main.rkt")


(provide move-thing-from-thing!
         move-thing-into-thing!
         add-containers-for-mudsocket-procedures-to-thing!
         add-containers-for-mudsocket-procedures-to-universe!)

(define (move-thing-from-thing! mover origin)
  (set-thing-quality!
   origin 'contents
   (remove mover
           (thing-quality origin 'contents)))
  (set-thing-quality!
   mover 'container
   (void)))

(define (move-thing-into-thing! mover destination)
  (when (and (thing-has-quality? mover 'container)
             (thing? (thing-quality mover 'container)))
    (move-thing-from-thing! mover (thing-quality mover 'container)))
  (set-thing-quality!
   destination 'contents
   (append (thing-quality destination 'contents)
           (list mover)))
  (set-thing-quality! mover 'container destination))

(define (add-containers-for-mudsocket-procedures-to-x! x)
  (define cfMs-procedures
    (list
     (cons 'move-thing-from-thing!
           move-thing-from-thing!)
     (cons 'move-thing-into-thing!
           move-thing-from-thing!)
     ))
  (log-debug "Adding Containers for MUDSocket procedures to ~a."
             (cond [(universe? x)
                    (universe-name x)]
                   [(thing? x)
                    (thing-name x)]))
  (cond
    [(thing? x)
     (add-procedures-to-thing! cfMs-procedures x)]
    [(universe? x)
     (add-procedures-to-universe! cfMs-procedures x)]))

(define (add-containers-for-mudsocket-procedures-to-thing!
         changed-thing)
  (add-containers-for-mudsocket-procedures-to-x! changed-thing))

(define (add-containers-for-mudsocket-procedures-to-universe!
         changed-universe)
  (add-containers-for-mudsocket-procedures-to-x! changed-universe))
