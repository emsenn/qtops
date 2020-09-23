#lang racket

(provide create-core-thing
         create-thing
         >procedures
         >procedure
         >set-procedure!
         >set-procedures!
         >remove-procedure!
         >has-procedure?
         >with-procedure~~
         >prerender-string)

(define (>procedures t)
  (unless (procedure? t)
    (raise-argument-error 'procedures
                          "procedure?"
                          t))
  (λ ()
    (log-debug "querying procedures of ~a"
               ((t 'with-procedure~~ 'name)
                #:alternate t))
    (t 'call~ (λ (T) T))))

(define (>procedure t)
  (unless (procedure? t)
    (raise-argument-error 'procedure
                          "procedure?"
                          t))
  (λ (k)
    (unless (symbol? k)
      (raise-argument-error 'procedure
                            "symbol?"
                            k))
    (log-debug "querying ~a procedure of ~a"
               k
               ((t 'with-procedure~~ 'name)
                #:alternate t))
    (if (hash-has-key? (t 'procedures) k)
        (hash-ref (t 'procedures) k)
        (raise-result-error (string->symbol
                             (if (hash-has-key? (t 'procedures) 'name)
                                 (t 'name)
                                 "thing"))
                            (format "thing with ~a procedure"
                                    k)
                            (format "~a with ~a procedures"
                                    (if (hash-has-key?
                                         (t 'procedures) 'name)
                                        (t 'name)
                                        "thing")
                                    (hash-keys (t 'procedures)))))))

(define (>set-procedure! t)
  (unless (procedure? t)
    (raise-argument-error 'set-procedure!
                          "procedure?"
                          t))
  (λ (k p)
    (unless (symbol? k)
      (raise-argument-error 'set-procedure!
                            "symbol?"
                            k))
    (unless (procedure? p)
      (raise-argument-error 'set-procedure!
                            "procedure?"
                            p))
    (log-debug "setting ~a procedure of ~a to ~a"
               k
               ((t 'with-procedure~~ 'name)
                #:alternate t))
    (t 'call~ (λ (T) (hash-set! T k p)))))

(define (>set-procedures! t)
  (unless (procedure? t)
    (raise-argument-error 'set-procedures!
                          "procedure?"
                          t))
  (λ (P)
    ;; need better handling here
    (log-debug "setting new procedures for ~a: ~a"
               ((t 'with-procedure~~ 'name)
                #:alternate t))
    (map (λ (p) (t 'set-procedure! (car p) (cdr p)))
         P)
    (void)))

(define (>remove-procedure! t)
  (unless (procedure? t)
    (raise-argument-error 'remove-procedure!
                          "procedure?"
                          t))
  (λ (k)
    (unless (symbol? k)
      (raise-argument-error 'remove-procedure!
                            "symbol?"
                            k))
    (log-debug "removing ~a procedure from ~a"
               k
               ((t 'with-procedure~~ 'name)
                #:alternate t))
    (t 'call~ (λ (T) (hash-remove! T k)))))

(define (>has-procedure? t)
  (unless (procedure? t)
    (raise-argument-error 'has-procedure?
                          "procedure?"
                          t))
  (λ (k)
    (log-debug "querying if ~a has ~a procedure."
               ((t 'with-procedure~~ 'name)
                #:alternate t)
               k)
    (t 'call~ (λ (T) (hash-has-key? T k)))))

(define (>with-procedure~~ t)
  (unless (procedure? t)
    (raise-argument-error 'with-procedure~~
                          "procedure?"
                          t))
  (λ (k)
    (unless (symbol? k)
      (raise-argument-error 'with-procedure~~
                            "symbol?"
                            k))
    (λ (#:alternate [alternate #f] . a)
      (if (t 'has-procedure? k)
          (if (null? a)
              (t k)
              (apply (t 'procedure k) a))
          (if alternate
              (if (procedure? alternate)
                  (alternate)
                  alternate)
              (void))))))

(define (>prerender-string t)
  (unless (procedure? t)
    (raise-argument-error 'prerender-string
                          "procedure?"
                          t))
  (λ (S)
    (λ ()
      (define r "")
      (map (λ (s)
             (set! r
                   (string-append
                    r (if (symbol? s) (t s) s))))
           S)
      r)))

(define ((>name t)) "thing")
(define ((>set-name! t) n) (t 'set-procedure! 'name
                             (λ () n)))

(define (>name=? t)
  (define (real-name=? l)
    (or (string=? (t 'name) l)
        (string=? (string-downcase (t 'name)) l)))
  (cond
    [(t 'has-procedure? 'term=?)
     (define old-term=? (t 'procedure 'term=?))
     (t 'set-procedure! 'term=?
        (λ (l) (or (old-term=? l) (t 'name=?))))]
    [else
     (t 'set-procedure! 'term=? real-name=?)])
  real-name=?)

(define (create-core-thing)
  (log-debug "creating new core thing")
  (define T (make-hash))
  (define (t k . a)
    (if (hash-has-key? T k)
        (apply (hash-ref T k) a)
        (raise-argument-error 'thing
                              (format "~a is missing procedure ~a"
                                      (if (hash-has-key? T 'name)
                                          (hash-ref T 'name)
                                          "thing")
                                      k)
                              (hash-keys T))))
  (hash-set! T 'call~ (λ (p) (p T)))
  t)

(define (create-thing [given-name "thing"] [additional-procedures #f])
  (log-debug "creating a thing")
  (define t (create-core-thing))
  (t 'call~ (λ (T) (hash-set! T 'set-procedure!
                             (>set-procedure! t))))
  (t 'set-procedure! 'set-procedures! (>set-procedures! t))
  (t 'set-procedures!
     (list (cons 'has-procedure? (>has-procedure? t))
           (cons 'procedures (>procedures t))
           (cons 'procedure (>procedure t))
           (cons 'remove-procedure! (>remove-procedure! t))
           (cons 'with-procedure~~ (>with-procedure~~ t))
           (cons 'prerender-string (>prerender-string t))
           (cons 'name (>name t))
           (cons 'name=? (>name=? t))
           ;; ^-- also sets term=?
           (cons 'set-name! (>set-name! t))))
  (when additional-procedures
    (map
     (λ (P) (t 'set-procedures! (P t)))
     additional-procedures))
  (when given-name (t 'set-name! given-name))
  t)

(module+ test
  (require rackunit)
  (provide qtops-tests:things)
  (define qtops-tests:things
    (test-suite
     "Tests for qtOps things."
     (test-case
         "Created thing is a procedure with the procedures we expect."
       (check-pred procedure? (create-thing))))))
