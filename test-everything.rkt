#lang racket

(require rackunit
         rackunit/text-ui
         "engine/procedures/logging.rkt")

(require (submod "engine/procedures/creation.rkt" test)
         (submod "engine/procedures/universe.rkt" test)
         (submod "library/containers.rkt" test))

(run-logger (create-logger 'qtMUD 'debug))


(run-tests (test-suite "All qtMUD tests"
                       qtmud-creation-tests
                       qtmud-universe-tests
                       qtmud-container-tests))
