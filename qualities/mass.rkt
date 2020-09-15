#lang racket

(provide make-mass-procedures)

(define ((mass t)) 0)
(define ((set-mass! t) m)
  (when (< m 0) (set! m 0))
  (t 'set-procedure! 'mass (λ () m)))
(define ((increment-mass! t) [m 1])
  (t 'set-mass! (+ (t 'mass) m)))

(define (make-mass-procedures t)
  (list
   (cons 'mass (mass t))
   (cons 'set-mass! (set-mass! t))
   (cons 'increment-mass! (increment-mass! t))))

(module+ test
  (require rackunit
           qtmud)
  (provide qtmud-mass-tests)
  (define qtmud-mass-tests
    (test-suite
     "Tests for qtMUD's mass quality. Things with the mass quality will be called \"massive things\"."
     (test-case
         "Adding mass procedures results in a thing having mass, set-mass!, and increment-mass! procedures."
       (define t (create-thing))
       (t 'set-procedures! (make-mass-procedures t))
       (define thp? (t 'procedure 'has-procedure?))
       (check-pred thp? 'mass)
       (check-pred thp? 'set-mass!)
       (check-pred thp? 'increment-mass!))
     (test-case
         "A massive thing's mass is 0 immediately after the mass quality wass added."
       (define t (create-thing))
       (t 'set-procedures! (make-mass-procedures t))
       (check-eq? 0 (t 'mass)))
     (test-case
         "A massive thing's mass is 10 after it has ('set-mass 10) called on it."
       (define t (create-thing))
       (t 'set-procedures! (make-mass-procedures t))
       (t 'set-mass! 10)
       (check-eq? 10 (t 'mass)))
     (test-case
         "a massive thing's mass is 1 after it has had ('increment-mass!) called on it."
       (define t (create-thing))
       (t 'set-procedures! (make-mass-procedures t))
       (t 'increment-mass!)
       (check-eq? 1 (t 'mass)))
     (test-case
         "A massive thing's mass is 5 after it has '(increment-mass! 5) called on it."
       (define t (create-thing))
       (t 'set-procedures! (make-mass-procedures t))
       (t 'increment-mass! 5)
       (check-eq? 5 (t 'mass)))
     (test-case
         "A massive thing with 2 mass has 0 mass after it has ('increment-mass -5) called on it."
       (define t (create-thing))
       (t 'set-procedures! (make-mass-procedures t))
       (t 'set-mass! 2)
       (t 'increment-mass! -5)
       (check-eq? 0 (t 'mass))
     (test-case
         "A massive thing with 10 mass has 5 mass after it has ('increment-mass! -5) called on it."
       (define t (create-thing))
       (t 'set-procedures! (make-mass-procedures t))
       (t 'set-mass! 10)
       (t 'increment-mass! -5)
       (check-eq? 5 (t 'mass)))))))
