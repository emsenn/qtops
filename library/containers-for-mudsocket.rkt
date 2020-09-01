#lang racket

(require "../engine/main.rkt")


(provide add-containers-for-mudsocket-procedures-to-thing!
         add-containers-for-mudsocket-procedures-to-universe!)

(define (moving-thing-into-thing moving-thing destination-thing)
  (add-string-to-quality-of-things-with-quality!
   (format "~a is moving to ~a."
           (thing-name moving-thing)
           (thing-name destination-thing))
   'mudsocket-output-buffer
   'mudsocket-output-buffer
   (remove moving-thing
           (thing-quality
            (thing-quality moving-thing 'container)
            'contents))))

(define (moved-thing-from-thing moved-thing old-container)
  (add-string-to-quality-of-things-with-quality!
   (format "~a has moved here from ~a."
           (thing-name moved-thing)
           (cond
             [(thing? old-container)
              (thing-name old-container)]
             [else "someplace"]))
   'mudsocket-output-buffer
   'mudsocket-output-buffer
   (remove moved-thing
           (thing-quality
            (thing-quality moved-thing 'container)
            'contents))))

(define (add-containers-for-mudsocket-procedures-to-x! x)
  (define cfMs-procedures
    (list
     (cons 'moving-thing-into-thing
           moving-thing-into-thing)
     (cons 'moved-thing-from-thing
           moved-thing-from-thing)
     ))
  (log-debug "Adding containers for MUDSocket procedures to ~a."
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
