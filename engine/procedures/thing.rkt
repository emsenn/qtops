#lang racket

(require "../structs/mud.rkt"
         "raising-errors.rkt"
         "universe.rkt"
         "utilities.rkt")

(provide use-thing-procedure
         use-thing-quality-procedure
         thing-has-procedure?
         thing-procedure
         set-thing-procedure!
         add-procedures-to-thing!
         thing-has-universe?
         thing-has-quality?
         thing-quality
         add-quality-to-thing!
         set-thing-quality!
         add-string-to-thing-quality!
         element-in-thing-quality?
         add-element-to-thing-quality!
         add-elements-to-thing-quality!
         remove-element-from-thing-quality!
         add-keyvalue-to-thing-quality!
         add-keyvalues-to-thing-quality!
         remove-key-from-thing-quality!
         thing-quality-key
         list-thing-names)

(define (use-thing-procedure handler-procedure
                      handled-thing
                      handled-symbol
                      [new-value #f]
                      #:pass-symbol [pass-symbol #t]
                      #:flip-syntax [flip-syntax #f])
  (unless (symbol? handler-procedure)
    (raise-argument-error 'use-thing-procedure
                          "symbol?" handler-procedure))
  (unless (thing? handled-thing)
    (raise-argument-error handler-procedure
                          "thing?" handled-thing))
  (unless (symbol? handled-symbol)
    (raise-argument-error handler-procedure
                          "symbol?" handled-symbol))
  (define (do-target-procedure target-procedure
                               handled-thing
                               handled-symbol
                               new-value)
    (cond
      [new-value
       (cond
         [pass-symbol
          (cond
            [flip-syntax
              (target-procedure new-value
                                handled-thing
                                handled-symbol)]
            [else
             (target-procedure handled-thing
                               handled-symbol
                               new-value)])]
         [else
          (cond
            [flip-syntax
             (printf "\n\n\n\nTHIS ONE SHOULD BE HAPPENING!!\n\n\n")
             (target-procedure new-value
                               handled-thing)]
            [else
             (target-procedure handled-thing
                               new-value)])])]
      [else
       (cond
         [pass-symbol
          (target-procedure handled-thing handled-symbol)]
         [else
          (target-procedure handled-thing)])]))
  (define handled-thing-name (thing-name handled-thing))
  (define handled-thing-universe
    (thing-universe handled-thing))
  (log-debug "Checking if ~a has a ~a procedure."
             handled-thing-name
             handler-procedure)
  (cond [(thing-has-procedure? handled-thing handler-procedure #t)
         (log-debug "~a has a ~a procedure, using it."
                    handled-thing-name
                    handler-procedure)
          (do-target-procedure
           (thing-procedure handled-thing handler-procedure #t)
           handled-thing handled-symbol new-value)]
        [(and handled-thing-universe
              (universe-has-procedure? handled-thing-universe
                                       handler-procedure))
         (log-debug "~a's universe ~a has a ~a procedure, using it."
                    handled-thing-name
                    (universe-name handled-thing-universe)
                    handler-procedure)
         (do-target-procedure
          (universe-procedure handled-thing-universe
                              handler-procedure)
          handled-thing handled-symbol new-value)]
        [else
         (log-debug "~a doesn't have a ~a procedure."
                    handled-thing-name
                    handler-procedure)
         #f]))

(define (use-thing-quality-procedure handler-procedure
                                     handled-thing
                                     handled-symbol
                                     [new-value #f]
                                     #:flip-syntax [flip-syntax #f])
  (log-debug (§ "Checking if ~a has a ~a procedure for the ~a "
                "quality.")
             (thing-name handled-thing)
             handler-procedure
             handled-symbol)
  (unless (use-thing-procedure (symbol-replace handler-procedure
                                               'quality
                                               handled-symbol)
                               handled-thing
                               handled-symbol
                               new-value
                               #:pass-symbol #f
                               #:flip-syntax flip-syntax)
    (use-thing-procedure handler-procedure
                         handled-thing
                         handled-symbol
                         new-value
                         #:pass-symbol #f
                         #:flip-syntax flip-syntax)))

(define (thing-has-procedure? queried-thing
                              queried-procedure
                              [skip #f])
  (when (or skip
            (not (use-thing-procedure 'thing-has-procedure?
                                      queried-thing
                                      queried-procedure)))
    (hash-has-key? (thing-procedures queried-thing)
                   queried-procedure)))

(define (thing-procedure queried-thing
                         queried-procedure
                         [skip #f])
  (when (or skip
            (not (use-thing-procedure 'thing-procedure?
                                      queried-thing
                                      queried-procedure)))
    (hash-ref (thing-procedures queried-thing) queried-procedure)))

(define (set-thing-procedure! changed-thing
                              changed-procedure
                              new-value
                              [skip #f])
  (when (or skip
            (not (use-thing-procedure 'set-thing-procedure!
                                      changed-thing
                                      changed-procedure
                                      new-value)))
    (hash-set! (thing-procedures changed-thing)
               changed-procedure
               new-value)))

(define (add-procedures-to-thing! procedures-list
                                  target-thing)
  (unless (and (list? procedures-list)
               (andmap symbol?
                       (map (λ (p) (car p))
                            procedures-list))
               (andmap procedure?
                       (map (λ (p) (cdr p))
                            procedures-list)))
    (raise-argument-error 'add-procedures-to-universe!
                          "listof (symbol? . procedure?)"
                          procedures-list))
  (map (λ (added-procedure)
         (define procedure-key (car added-procedure))
         (unless (thing-has-procedure? target-thing
                                       procedure-key)
           (set-thing-procedure! target-thing
                                 procedure-key
                                 (cdr added-procedure))))
       procedures-list))

(define (thing-has-universe? queried-thing)
  (cond [(universe? (thing-universe queried-thing)) #t]
        [else #f]))

(define (thing-has-quality? queried-thing
                            queried-quality)
  (log-debug (§ "Primary thing-has-quality? procedure has been "
                "called on ~a (for the ~a quality).")
             (thing-name queried-thing)
             queried-quality)
  (unless
      (use-thing-quality-procedure 'thing-has-quality?
                                   queried-thing
                                   queried-quality)
    (log-debug "Using default thing-has-quality? procedure for ~a."
               (thing-name queried-thing))
    (hash-has-key? (thing-qualities queried-thing)
                                  queried-quality)))

(define (thing-quality queried-thing queried-quality)
  (unless (use-thing-procedure 'thing-quality
                               queried-thing
                               queried-quality)
    (log-debug "Using default thing-quality procedure on ~a for ~a."
               (thing-name queried-thing)
               queried-quality)
    (cond
      [(thing-has-quality? queried-thing
                           queried-quality)
       (hash-ref (thing-qualities queried-thing)
                 queried-quality)]
      [else
       (raise-thing-quality-missing-error
        'thing-quality
        queried-thing
        queried-quality)])))

(define (set-thing-quality! changed-thing
                            changed-quality
                            new-value
                            #:skip [skip #f]
                            #:force [force-set #f])
  (log-debug (§ "Primary set-thing-quality! procedure has been "
                "called on ~a (for the ~a quality).")
             (thing-name changed-thing)
             changed-quality)
  (when (or skip
            (not (use-thing-quality-procedure 'set-thing-quality!
                                              changed-thing
                                              changed-quality
                                              new-value)))
    (log-debug "Using default set-thing-quality! procedure for ~a."
               (thing-name changed-thing))
    (cond
      [(or force-set
           (thing-has-quality? changed-thing
                           changed-quality))
       (hash-set! (thing-qualities changed-thing)
                  changed-quality
                  new-value)]
      [else
       (raise-thing-quality-missing-error
        'set-thing-quality!
        changed-thing
        changed-quality)])))

(define (add-quality-to-thing! new-quality
                               changed-thing)
  (unless (use-thing-quality-procedure 'add-quality-to-thing!
                                       changed-thing
                                       new-quality)
    (set-thing-quality! changed-thing
                        new-quality
                        (void)
                        #:skip #t
                        #:force #t)))


(define (add-string-to-thing-quality! input-string
                                      changed-thing
                                      changed-quality)
  (unless (string? input-string)
    (raise-argument-error 'add-string-to-thing-quality!
                          "string?"
                          input-string))
  (unless (use-thing-quality-procedure 'add-string-to-thing-quality!
                                       changed-thing
                                       changed-quality
                                       input-string
                                       #:flip-syntax #t)
    (set-thing-quality! changed-thing
                        changed-quality
                        (string-join
                         (list (thing-quality changed-thing
                                              changed-quality)
                               input-string)))))

(define (element-in-thing-quality? queried-element
                                   queried-thing
                                   queried-quality)
  (unless (use-thing-quality-procedure 'element-in-thing-quality?
                                       queried-thing
                                        queried-quality
                                        queried-element
                                        #:flip-syntax #t)
    (unless (thing-has-quality? queried-thing queried-quality)
      (cond
        [(member (thing-quality queried-thing queried-quality)
                 queried-element)
         #t]
        [else #f]))))

(define (add-element-to-thing-quality! new-element
                                      changed-thing
                                      changed-quality)
  (unless (use-thing-quality-procedure 'add-element-to-thing-quality!
                                       changed-thing
                                       changed-quality
                                       new-element
                                       #:flip-syntax #t)
    (unless (list? (thing-quality changed-quality))
      (raise-argument-error 'add-element-to-thing-quality!
                            "list?"
                            (thing-quality changed-quality)))
    (set-thing-quality! changed-thing
                        changed-quality
                        (append (thing-quality changed-thing
                                               changed-quality)
                                (list new-element)))))

(define (add-elements-to-thing-quality! new-elements
                                       changed-thing
                                       changed-quality)
  (unless (list? new-elements)
    (raise-argument-error 'add-elements-to-thing-quality!
                          "list?" new-elements))
  (unless
      (use-thing-quality-procedure 'add-elements-to-thing-quality!
                                   changed-thing
                                   changed-quality
                                   new-elements)

    (map (λ (new-element)
           (add-element-to-thing-quality! new-element
                                          changed-thing
                                          changed-quality))
         new-elements)))

(define (remove-element-from-thing-quality! removed-element
                                            changed-thing
                                            changed-quality)
  (unless
      (use-thing-quality-procedure
       'remove-element-from-thing-quality!
       changed-thing
       changed-quality
       removed-element
       #:flip-syntax #t)
    (set-thing-quality! changed-thing
                        changed-quality
                        (remove removed-element
                                (thing-quality changed-thing
                                               changed-quality)))))

(define (add-keyvalue-to-thing-quality! new-keyvalue
                                        changed-thing
                                        changed-quality)
  (unless (use-thing-quality-procedure
           'add-keyvalue-to-thing-quality!
           changed-thing
           changed-quality
           new-keyvalue
           #:flip-syntax #f)
    (hash-set! (thing-quality changed-thing changed-quality)
               (car new-keyvalue) (cdr new-keyvalue))))

(define (add-keyvalues-to-thing-quality! new-keyvalues
                                         changed-thing
                                         changed-quality)
  (unless (list? new-keyvalues)
    (raise-argument-error 'add-keyvalues-to-thing-quality!
                          "list?"
                          new-keyvalues))
  (map (λ (new-keyvalue)
	 (add-keyvalue-to-thing-quality! new-keyvalue
                                         changed-thing
                                         changed-quality))
       new-keyvalues))

(define (remove-key-from-thing-quality! removed-key
                                        changed-thing
                                        changed-quality)
  (unless (use-thing-quality-procedure
           'remove-key-from-thing-quality!
           changed-thing
           changed-quality
           removed-key
           #:flip-syntax #f)
    (hash-remove! (thing-quality changed-thing
                                 changed-quality)
                  removed-key)))

(define (thing-quality-key queried-thing
                           queried-quality
                           queried-key)
  (unless (use-thing-quality-procedure
           'thing-quality-key
           queried-thing
           queried-quality
           queried-key)
    (hash-ref (thing-quality queried-thing
                             queried-quality)
              queried-key)))

(define (list-thing-names things)
  (unless (and (list? things)
               (andmap thing? things))
    (raise-argument-error 'list-thing-names
                          "listof thing?"
                          things))
  (oxfordize-list
   (map (λ (this-thing)
          (thing-name this-thing))
        things)))
