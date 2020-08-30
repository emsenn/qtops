#lang racket

(require "../engine/main.rkt")

(provide add-exits-to-thing!
         set-thing-exits!
         add-exit-procedures-to-thing!)

(define (add-exits-to-thing changed-thing)
  (log-info "Adding the exits quality to ~a."
            (thing-name changed-thing))
  (set-thing-quality! changed-thing
                      'exits
                      (make-hash)
                      #:skip #t
                      #:force #t))

(define (set-thing-exits! changed-thing new-exits)
  (unless (and (list? new-exits)
               (andmap pair? new-exits)
               (andmap
                (map
                 (λ (e)
                   (cond
                     [(and (symbol? (car e))
                          (thing? (cdr e)))
                      #t]
                     [else #f]))
                 new-exits)))
    (raise-argument-error 'set-thing-exits!
                          "listof (symbol? . thing?)"
                          new-exits))
  (set-thing-quality! changed-thing
                      'exits
                      (make-hash
                       new-exits)
                      #:skip #t))

(define (add-keyvalue-to-thing-exits! changed-thing
                                      new-exit)
  (unless (or (symbol? (car new-exit))
              (thing? (cdr new-exit)))
    (raise-argument-error 'add-keyvalue-to-thing-exits!
                          "(symbol? . thing?)"
                          new-exit))
  (hash-set! (thing-quality changed-thing 'exits)
             (car new-exit)
             (cdr new-exit)))

(define (add-exit-procedures-to-x! x)
  (define area-procedures
    (map (λ (p) (cons p (eval p)))
         (list 'add-exits-to-thing!
               'set-thing-exits!
               'add-keyvalue-to-thing-exits!))))

(define (add-exit-procedures-to-thing! changed-thing)
  (add-exit-procedures-to-x! changed-thing))

(define (add-exit-procedures-to-universe! changed-universe)
  (add-exit-procedures-to-x! changed-universe))
