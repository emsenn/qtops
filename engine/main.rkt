#lang racket


(provide create-thing)

(define ((procedures t))
  ;(log-debug "Querying procedures of ~a" t)
  (t 'call (λ (T) T)))

(define ((procedure t) k)
  (hash-ref (t 'procedures) k))

(define ((set-procedure! t) k p)
  ;(log-debug "Setting procedure ~a of ~a to ~a" k t p)
  (t 'call (λ (T) (hash-set! T k p))))

(define ((set-procedures! t) p)
  (map (λ (P) (t 'set-procedure! (car P) (cdr P)))
       p)
  (void))

(define ((remove-procedure! t) p)
  ;(log-debug "Removing ~a procedure from ~a" p t)
  (t 'call (λ (T) (hash-remove! T p))))

(define ((has-procedure? t) p)
  ;(log-debug "Querying if ~a has procedure ~a" t p)
  (t 'call (λ (T) (hash-has-key? T p))))

(define (((with-procedure t) k) #:alternate [alternate #f] . a)
  (if (t 'has-procedure? k)
      (if (null? a)
          (t k)
          (apply (t 'procedure k) a))
      (if alternate
          (alternate)
          (void))))

(define ((without-procedure t) k a)
  (unless (t 'has-procedure? k) a))

(define ((prerender-string t) S)
  (λ ()
    (define r "")
    (map (λ (s)
           (set! r
                 (string-append
                  r (if (symbol? s) (t s) s))))
         S)
    r))

(define (create-thing)
  (log-debug "Creating a new thing.")
  (define T (make-hash))
  (define (t p . a)
    (cond
      [(hash-has-key? T p) (apply (hash-ref T p) a)]
      [else (printf "No procedure ~a" p)]))
  (hash-set! T 'call (λ (p) (p T)))
  (hash-set! T 'procedure (procedure t))
  (hash-set! T 'procedures (procedures t))
  (hash-set! T 'set-procedure! (set-procedure! t))
  (hash-set! T 'set-procedures! (set-procedures! t))
  (hash-set! T 'remove-procedure! (remove-procedure! t))
  (hash-set! T 'has-procedure? (has-procedure? t))
  (hash-set! T 'with-procedure (with-procedure t))
  (hash-set! T 'without-procedure (without-procedure t))
  (hash-set! T 'prerender-string (prerender-string t))
  t)
