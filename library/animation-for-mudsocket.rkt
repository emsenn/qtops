#lang racket

(require "../engine/main.rkt"
         "containers.rkt")

(provide make-look-mudsocket-command-for-thing
	 make-move-mudsocket-command-for-thing)

(define (make-look-mudsocket-command-for-thing commanding-thing)
  (λ (command-arguments)
    (cond
      [(hash-empty? command-arguments)
       (cond
         [(thing-has-quality? commanding-thing 'container)
          (let ([commanding-thing-container
                 (thing-quality commanding-thing 'container)])
            (add-string-to-thing-quality!
             (format "[    ~a    ]~a~a"
                     (thing-name commanding-thing-container)
                     (cond
                       [(thing-has-quality?
                         commanding-thing-container
                         'description)
                        (format "\n  ~a"
                                (thing-quality
                                 commanding-thing-container
                                 'description))]
                       [else ""])
                     (cond
                       [(thing-has-quality?
                         commanding-thing-container
                         'exits)
                        (format "\n  Area exits: ~a"
                                (oxfordize-list
                                 (hash-keys
                                  (thing-quality
                                   commanding-thing-container
                                   'exits))))]
                       [else ""]))
             commanding-thing 'mudsocket-output-buffer))]
         [else
          (add-string-to-thing-quality!
           "You look around, but you aren't anyplace."
           commanding-thing 'mudsocket-output-buffer)])]
      [(hash-has-key? command-arguments "container")
       (add-string-to-thing-quality!
        "Looking inside things doesn't work yet, sorry."
        commanding-thing 'mudsocket-output-buffer)]
      [(hash-has-key? command-arguments 'line)
       (add-string-to-thing-quality!
        "Looking at things doesn't work yet, sorry."
        commanding-thing 'mudsocket-output-buffer)])))

(define (make-move-mudsocket-command-for-thing commanding-thing)
  (λ (command-arguments)
    (define commanding-thing-container
      (thing-quality commanding-thing 'container))
    (define commanding-thing-container-exits
      (thing-quality commanding-thing-container 'exits))
    (cond
      [(hash-has-key? command-arguments 'line)
       (define command-arguments-line
         (hash-ref command-arguments 'line))
       (cond
         [(hash-has-key? commanding-thing-container-exits
                         command-arguments-line)
          (define destination-area
            (hash-ref commanding-thing-container-exits
                      command-arguments-line))
          (unless (use-thing-procedure
                   'move-thing-into-thing!
                   commanding-thing
                   'null
                   destination-area
                   #:pass-symbol #f)
            (move-thing-into-thing! commanding-thing
                                   destination-area))
          (add-string-to-thing-quality!
           (format "You move; your location is now ~a.~a"
                   (thing-name destination-area)
                   (cond
                     [(thing-has-quality? destination-area
                                          'exits)

                      (format "\n  Area exits: ~a"
                              (oxfordize-list
                               (hash-keys
                                (thing-quality destination-area
                                               'exits))))]
                     [else ""]))
           commanding-thing 'mudsocket-output-buffer)]
         [else
          (add-string-to-thing-quality!
           (format "You failed to move: ~a is an invalid exit."
                   command-arguments-line)
           commanding-thing 'mudsocket-output-buffer)])]
      [else
       (add-string-to-thing-quality!
        (§ "You must use this command with an exit, "
           "try to \"look\" for one.")
        commanding-thing 'mudsocket-output-buffer)])))


(module+ test
  (require rackunit)
  (define qtmud-animation-for-mudsocket-tests
    (test-suite
     "Tests for qtMUD's Animation for MUDSocket library component."
     (run-logger (create-logger 'qtMUD 'debug))
     (test-case
         "No real tests!"
         (check-eq? 1 1)))))
