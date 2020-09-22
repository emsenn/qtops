#lang racket


(provide <>animate
         >>make-animation-procedures
         >animations
         >set-animations!
         >add-animations!
         >add-animation!
         >animate~~)

(define (animations-list? A)
  (and (list? A)
                 (andmap list? A)
                 (andmap (λ (a)
                           (and (procedure? (first a))
                                (or (integer? (second a))
                                    (procedure? (second a)))
                                (or (integer? (third a))
                                    (procedure? (third a)))))
                         A)))

(define (>animations t)
  (unless (procedure? t)
    (raise-argument-error '>animations
                          "procedure?"
                          t))
  (log-debug "Creating animations procedure for ~a"
             ((t 'with-procedure 'name)
              #:alternate "thing"))
  (λ ()
    (log-debug "Querying ~a's animations"
               ((t 'with-procedure 'name)
                #:alternate "thing"))
    (list)))

(define (>set-animations! t)
  (unless (procedure? t)
    (raise-argument-error '>set-animations!
                          "procedure?"
                          t))
  (log-debug "Creating set-animations! procedure for ~a"
             ((t 'with-procedure 'name)
              #:alternate "thing"))
  (λ (A)
    (unless (animations-list? A)
      (raise-argument-error
       'set-animations!
       "listof (list procedure (proc/int) (proc/int)"
       A))
    (log-debug "Setting ~a's animations to ~a"
               ((t 'with-procedure 'name)
                #:alternate "thing")
               A)
    (t 'set-procedure!
       'animations
       (λ ()
         (log-debug "Querying ~a's animations."
                    ((t 'with-procedure 'name)
                     #:alternate "thing"))
         A))))


(define (>add-animations! t)
  (unless (procedure? t)
    (raise-argument-error '>add-animations!
                          "procedure?"
                          t))
  (log-debug "Creating add-animations! procedure for ~a"
             (t 'name))
  (λ (A)
    (unless (animations-list? A)
      (raise-argument-error
       'add-animations!
       "listof (list proc (proc/int) (proc/int))"
       A))
    (log-debug "Adding to ~a's animations: ~a"
               ((t 'with-procedure 'name)
                #:alternate "thing")
               A)
    (t 'set-animations! (append (t 'animations) A))))
(define (>add-animation! t)
  (unless (procedure? t)
    (raise-argument-error '>add-animation!
                          "procedure?"
                          t))
  (log-debug "Creating add-animation! procedure for ~a"
             (t 'name))
  (λ (a)
    (log-debug "Adding ~a to ~a's animations"
               a (t 'name))
    (t 'add-animations! (list a))))


(define (>animate~~ t)
  (unless (procedure? t)
    (raise-argument-error '>animate~~
                          "procedure?"
                          t))
  (log-debug "Creating animate~~ procedure for ~a" (t 'name))
  (λ ()
    (log-debug "Processing ~a's animations" (t 'name))
    (define i 120)
    (map
     (λ (a)
       (log-debug "Processing ~a's ~a animation"
                  (t 'name) a)
       (define p (t 'procedure (car a)))
       (define f (let ([w (car (cdr a))]) (if (procedure? w) (w) w)))
       (define c (let ([w (car (cdr (cdr a)))])
                   (if (procedure? w) (w) w)))
       (when (>= i f) (set! i f))
       (when (<= c 100)
         ((t 'with-procedure~~ 'universe)
          'schedule-event! p f
          #:alternate
          (log-warning
           "~a has no universe; cannot schedule animation ~a"
           (t 'name) a))))
     (t 'animations))
    ((t 'with-procedure~~ 'universe)
     'schedule-event! (t 'procedure 'animate~~) i
     #:alternate (log-warning
                  "~a has no universe; cannot reschedule animations"
                  (t 'name)))))


(define (>>make-animation-procedures t)
  (list
   (cons 'animations (>animations t))
   (cons 'set-animations! (>set-animations! t))
   (cons 'add-animations! (>add-animations! t))
   (cons 'add-animation! (>add-animation! t))
   (cons 'animate~~ (>animate~~ t))))

(define (<>animate t)
  (t 'set-procedures! (>>make-animation-procedures t))
  t)
