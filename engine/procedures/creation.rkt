#lang racket

(require "../structs/mud.rkt"
         "utilities.rkt")

(provide create-universe
         create-thing
         create-thing-creator-for-universe)

(define (create-universe [name (§ "Universe-"
				  (generate-simple-id 3))]
			 [events '()])
  (unless (string? name)
    (raise-argument-error 'create-universe
                          "string?" name))
  (unless (and (list? events)
               (andmap procedure? events))
    (raise-argument-error 'create-universe
                          "listof procedure?" events))
  (log-debug "Creating universe ~a" name)
  (universe name 0 events (list) (make-hash)))

(define (create-thing [name (§ "thing-"
                               (generate-simple-id 3))]
                      [chosen-universe #f]
                      #:grammar [grammar #f]
                      #:qualities [qualities #f]
                      #:procedures [procedures #f])
  (log-info "Creating a new thing named ~a~a"
	    name
	    (cond [chosen-universe
		   (format "for ~a."
			   (universe-name chosen-universe))]
		  [else "."]))
  (let ([created-thing (thing name chosen-universe
			      (cond [grammar (make-hash grammar)]
				    [else (make-hash)])
			      (cond [qualities (make-hash qualities)]
				    [else (make-hash)])
			      (cond [procedures (make-hash procedures)]
				    [else (make-hash)]))])
    (when chosen-universe
      (set-universe-things! chosen-universe
                            (append (universe-things chosen-universe)
                                    (list created-thing))))
    created-thing))

(define (create-thing-creator-for-universe target-universe)
  (λ ([name "thing"]
      #:grammar [grammar #f]
      #:qualities [qualities #f]
      #:procedures [procedures #f])
    (create-thing name
                  target-universe
                  #:grammar grammar
                  #:qualities qualities
                  #:procedures procedures)))

(module+ test
  (require rackunit)
  (provide qtmud-creation-tests)
  (define qtmud-creation-tests
    (test-suite
     "Tests for qtMUD's creation procedures."
     (test-case
         "Return of create-universe is a universe."
       (check-pred universe? (create-universe)))
     (test-case
         (§ "Passing invalid argument types to create-universe "
            "raises argument errors.")
       (check-exn exn:fail:contract?
                  (λ () (create-universe 'bar)))
       (check-exn exn:fail:contract?
                  (λ () (create-universe 'bar 'bing)))))))
