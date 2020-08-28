#lang racket

(require "../structs/mud.rkt"
         "raising-errors.rkt")

(provide thing-has-procedure?
         thing-procedure
         thing-has-universe?
         thing-has-quality?
         thing-quality
         set-thing-quality!
         add-string-to-thing-quality!
         add-element-to-thing-quality!
         add-elements-to-thing-quality!
         remove-element-from-thing-quality!
         add-keyvalue-to-thing-quality!
         add-keyvalues-to-thing-quality!
         remove-key-from-thing-quality!
         list-thing-names)

(define (use-thing-procedure handler-procedure
                      handled-thing
                      handled-symbol
                      [new-value #f])
  (unless (symbol? handler-procedure)
    (raise-argument-error 'use-thing-procedure
                          "symbol?" handler-procedure))
  (unless (thing? handled-thing)
    (raise-argument-error handler-procedure
                          "thing?" handled-thing))
  (unless (symbol? handled-symbol)y
    (raise-argument-error handler-procedure
                          "symbol?" handled-symbol))
  (define (do-target-procedure target-procedure
                               handled-thing
                               handled-symbol
                               new-value)
    (cond
      [new-value
       (target-procedure handled-thing
                         handled-symbol
                         new-value)]
      [else
       (target-procedure handled-thing
                         handled-symbol)]))
  (define handled-thing-name (thing-name handled-thing))
  (define handled-thing-universe
    (thing-universe bill))
  (log-debug "Handling ~a"
             handled-thing-name)
  (cond [(thing-has-procedure? handled-thing handler-procedure #t)
          (do-target-procedure
           (thing-procedure handled-thing handler-procedure #t)
           handled-thing handled-symbol new-value)]
        [(and handled-thing-universe
              (universe-has-procedure? handled-thing-universe
                                       handler-procedure))
         (do-target-procedure
          (universe-procedure handler-procedure)
          handled-thing handled-symbol new-value)]
        [else #f]))

(define (use-thing-quality-procedure handler-procedure
                                     handled-thing
                                     handled-symbol
                                     [new-value #f])
  (or (use-thing-procedure (symbol-replace handler-procedure
                                           'quality
                                           handled-symbol)
                           handled-thing
                           handled-symbol
                           new-value)
      (use-thing-procedure handler-procedure
                           handled-thing
                           handled-symbol
                           new-value)))

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

(define (thing-has-universe? queried-thing)
  (cond [(universe? (thing-universe queried-thing)) #t]
        [else #f]))

(define (thing-has-quality? queried-thing
                            queried-quality)
  (unless
      (use-thing-quality-procedure 'thing-has-quality?
                                   queried-thing
                                   queried-quality))
  (hash-has-key? (thing-quality queried-thing)
                 queried-quality))

(define (thing-quality queried-thing queried-quality)
  (unless (use-thing-procedure 'thing-quality
                               queried-thing
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
                            new-value)
  (unless (use-thing-quality-procedure 'set-thing-quality!
                                       changed-thing
                                       changed-quality
                                       new-value))
  (cond
    [(thing-has-quality? changed-thing
                         changed-quality)
     (hash-set! (thing-qualities changed-thing)
                changed-quality
                new-value)]
    [else
     (raise-thing-quality-missing-error
      'set-thing-quality!
      queried-thing
      queried-quality)]))

(define (add-string-to-thing-quality! input-string
                                      changed-thing
                                      changed-quality)
  (unless (string? input-string)
    (raise-argument-error 'add-stirng-to-thing-quality!
                          "string?"
                          input-string))
  (unless (use-thing-quality-procedure 'add-string-to-thing-quality!
                                       changed-thing
                                       changed-quality
                                       input-string)
    (set-thing-quality! changed-thing
                        changed-quality
                        (string-join
                         (thing-quality changed-thing
                                        thing-quality)
                         input-string))))

(define (add-element-to-thing-quality! new-element
                                      changed-thing
                                      changed-quality)
  (unless (use-thing-quality-procedure 'add-element-to-thing-quality!
                                       changed-thing
                                       changed-quality
                                       new-element)
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
       removed-element)

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
           new-keyvalue)
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
           removed-key)
    (hash-remove! (thing-quality changed-thing
                                 changed-quality)
                  removed-key)))

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
