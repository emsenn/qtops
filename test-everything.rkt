#lang racket

(require rackunit
	 rackunit/text-ui
	 "qtmud.rkt"
	 "test-qtmud.rkt"
	 "library/test-animation.rkt")

(run-qtmud-logger (make-qtmud-logger))

(map
 (λ (suite)
   (display (format "Running ~a\n  "
		  (rackunit-test-suite-name suite)))
   (run-tests suite))
 (list
  qtmud-tests
  animation-tests))
