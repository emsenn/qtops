#lang racket

(require "../engine/main.rkt")

(provide add-container-to-thing!
         set-thing-container!
         add-contents-to-thing!
         set-thing-contents!
         add-element-to-thing-contents!
         remove-element-from-thing-contents!
         move-thing-into-thing!
         move-things-into-thing!
         add-container-procedures-to-thing!
         add-container-procedures-to-universe!)

(define (add-container-to-thing! changed-thing)
  (log-info "Adding the container quality to ~a"
            (thing-name changed-thing))
  (set-thing-quality! changed-thing
                      'container
                      (void)
                      #:skip #t
                      #:force #t))

(define (set-thing-container! changed-thing new-container)
  (unless (thing? new-container)
    (raise-argument-error 'set-thing-container!
                          "thing?"
                          new-container))
  (unless (thing-has-quality? new-container
                              'contents)
    (raise-thing-quality-missing-error 'set-thing-contianer!
                                       new-container
                                       'contents))
  (set-thing-quality! changed-thing
                      'container
                      new-container
                      #:skip #t))

(define (add-contents-to-thing! changed-thing)
  (log-info "Adding the contents quality to ~a"
            (thing-name changed-thing))
    (set-thing-quality! changed-thing
                        'contents
                        '()
                        #:skip #t
                        #:force #t))

(define (set-thing-contents! changed-thing new-contents)
  (unless (and (list? new-contents)
               (andmap thing? new-contents))
    (raise-argument-error 'set-thing-contents!
                          "listof thing?"
                          new-contents))
  (set-thing-quality! changed-thing
                      'contents
                      new-contents
                      #:skip #t))

(define (add-element-to-thing-contents! new-element
                                        changed-thing)
  (unless (thing? new-element)
    (raise-argument-error 'add-element-to-thing-contents!
                          "thing?"
                          new-element))
  (unless (thing-has-quality? new-element 'container)
    (raise-thing-quality-missing-error
     'add-element-to-thing-contents!
     new-element 'container))
  (unless (thing-has-quality? changed-thing 'contents)
    (display (thing-qualities changed-thing))
    (raise-thing-quality-missing-error
     'add-element-to-thing-contents!
     changed-thing 'contents))
  (set-thing-quality! changed-thing
                      'contents
                      (append (thing-quality changed-thing
                                             'contents)
                              (list new-element))))

(define (remove-element-from-thing-contents! removed-element
                                             container-thing)
  (unless (thing? removed-element)
    (raise-argument-error 'remove-element-from-thing-contents
                          "thing?"
                          removed-element))
  (display (format "Removing ~a from ~a"
                   (thing-name removed-element)
                   (thing-name container-thing)))
  (set-thing-quality! container-thing
                      'contents
                      (remove removed-element
                              (thing-quality container-thing
                                             'contents))))

(define (move-thing-into-thing! moved-thing
                                destination-thing)
  (define moved-thing-container
    (thing-quality moved-thing 'container))
  (when (and (thing? moved-thing-container)
             (element-in-thing-quality? moved-thing
                                        moved-thing-container
                                        'contents))
    (use-thing-procedure 'moving-thing-into-thing
                         moved-thing
                         'null
                         destination-thing
                         #:pass-symbol #f)
    (remove-element-from-thing-quality! moved-thing
                                        moved-thing-container
                                        'contents))
  (add-element-to-thing-quality! moved-thing
                                 destination-thing
                                 'contents)
  (set-thing-quality! moved-thing 'container destination-thing)
  (use-thing-procedure 'moved-thing-from-thing
                       moved-thing
                       'null
                       moved-thing-container
                       #:pass-symbol #f))



(define (move-things-into-thing! moved-things changed-thing)
  (map (λ (moved-thing)
         (move-thing-into-thing! moved-thing
                                 changed-thing))
       moved-things))


(define (add-container-procedures-to-x! x)
  (define container-procedures
    (list
     (cons 'add-container-to-thing!
           add-container-to-thing!)
     (cons 'set-thing-container!
           set-thing-container!)
     (cons 'add-contents-to-thing!
           add-contents-to-thing!)
     (cons 'set-thing-contents!
           set-thing-contents!)
     (cons 'add-element-to-thing-contents!
           add-element-to-thing-contents!)
     (cons 'remove-element-from-thing-contents!
           remove-element-from-thing-contents!)
     (cons 'move-thing-into-thing!
           move-thing-into-thing!)
     (cons 'move-things-into-thing!
           move-things-into-thing!)))
  (log-debug "Adding container procedures to ~a."
             (cond [(universe? x)
                    (universe-name x)]
                   [(thing? x)
                    (thing-name x)]))
  (cond
    [(thing? x)
     (add-procedures-to-thing! container-procedures x)]
    [(universe? x)
     (add-procedures-to-universe! container-procedures x)]))

(define (add-container-procedures-to-thing! changed-thing)
  (add-container-procedures-to-x! changed-thing))

(define (add-container-procedures-to-universe! changed-universe)
  (add-container-procedures-to-x! changed-universe))

(define (hack-test)
  (define testverse (create-universe))
  (define create-thing (create-thing-creator-for-universe testverse))
  (add-container-procedures-to-universe! testverse)
  (define bill (create-thing "bill"))
  (define starship (create-thing "starship"))
  (add-quality-to-thing! 'container bill)
  (add-quality-to-thing! 'contents starship)
  (move-thing-into-thing! bill starship)
  (display (format "bills' container is ~a\n starships contents are ~a"
                   (thing-name (thing-quality bill 'container))
                   (thing-name (first (thing-quality starship 'contents))))))

(module+ test
  (require rackunit
           rackunit/text-ui)
  (provide qtmud-container-tests)
  (define (create-test-thing [universe #f])
    (create-thing (§ "Thing-"
                     (generate-simple-id 3))
                  universe))
  (define qtmud-container-tests
    (test-suite
     "Tests for qtMUD's Containers library component."
     (test-case
         (§ "Check that a blueberry with the container "
            "quality added returns void as its container.")
       (define blueberry (create-thing "blueberry"))
       (add-container-to-thing! blueberry)
       (check-pred void? (thing-quality blueberry
                                        'container)))
     (test-case
         (§ "Check that a crate with the contents quality "
            "added to it returns an empty list as its container.")
       (define crate (create-thing "crate"))
       (add-contents-to-thing! crate)
       (check-pred list? (thing-quality crate
                                        'contents)))
     (test-case
         (§ "Check that a banana with the container quality "
            "added to it raises an exception if trying to "
            "set the container to an integer.")
       (define banana (create-thing "banana"))
       (add-container-to-thing! banana)
       (check-exn exn:fail:contract?
                  (λ () (set-thing-container! banana 7))))
     (test-case
         (§ "Check that a sparrow with the container quality "
            "cannot be added to a star, without the contents "
            "quality.")
       (define sparrow (create-thing "sparrow"))
       (add-container-to-thing! sparrow)
       (define star (create-thing "star"))
       (check-exn exn:qtmud:thing:quality:missing?
                  (λ () (set-thing-container! sparrow star)))))))

;; check that a thing with the contents quality added has an empty list for its contents
;; check that...
;; adding a non-thing to a contents doesn't work
;; adding a thing without the container quality fails
;; adding the container quality to a thing works
;; adding a thing with the container quality works
;; adding a the added-thing's container is now the thing we added it to
;; removing a thing from a container removes it from the container's contents
;; removing a thing from a container makes its container voide
;; moving a thing from one container to another removes it from its previous container's contents
;; moving a thing from one container to another adds it to the new container's contents
;; moving a thing from one container to another makes its container the new container.
;; check a universe with container procedures has the add-element procedure
;; and that it's eqv? (?) to the one here
;; check that a thing made into a universe acts correctly when using the default function names
