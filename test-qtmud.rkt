#lang racket

  (require rackunit
	   "qtmud.rkt")

  (provide qtmud-tests)

(define qtmud-tests
  (test-suite
   "Tests for the qtMUD Racket module."

(test-case
    "Return of create-universe without arguments is a universe."
  (check-pred universe?
	      (create-universe)))

(test-case
    (§ "Return of create-universe with single string argument "
       "is universe.")
  (check-pred universe?
	      (create-universe "Testverse-One")))

(test-case
    (§ "Return of create-universe with two arguments, first "
       "string and second list, is universe.")
  (check-pred universe?
	      (create-universe "Testverse-Two" '())))

(test-case
    (§ "Return of create-universe with non-string first "
       "argument is contract failure.")
  (check-exn exn:fail:contract?
	     (λ () (create-universe 'Failverse))))

(test-case
    (§ "Return of create-universe with non-list second "
       "argument is contract failure.")
  (check-exn exn:fail:contract?
	     (λ () (create-universe "Testverse-Null" 'fail))))

))
