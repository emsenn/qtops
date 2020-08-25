#lang racket

(require rackunit
	 "../qtmud.rkt"
	 "container.rkt")

(provide container-tests)

(define container-tests
  (test-suite
   (§ "Tests for the Container qtMUD library component.")

   (test-case
       (§ "A thing that has had container procedures added to it "
          "has the `add-element-to-thing-contents!` procedure.")
     (check-true
      ((λ () (define test-thing (create-thing))
          (add-container-procedures-to-thing! test-thing)
          (thing-has-procedure? test-thing
   			     'add-element-to-thing-contents!)))))
   
   (test-case
       (§ "A universe to which container procedures have been added has the "
          "`add-element-to-thing-contents!` procedure.")
     (check-true ((λ ()
   		(define testverse (create-universe))
   		(add-container-procedures-to-universe! testverse)
   		(universe-has-procedure? testverse
   					 'add-element-to-thing-contents!)))))

   ))
