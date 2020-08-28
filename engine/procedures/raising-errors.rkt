#lang racket

(require "../structs/mud.rkt"
         "../structs/exceptions.rkt")


(define (raise-thing-quality-missing-error trigger-procedure
                                           missing-thing
                                           missing-quality)
  (unless (symbol? trigger-procedure)
    ((raise-argument-error 'raise-thing-quality-missing-error
                          "symbol?"
                          trigger-procedure))
  (unless (thing? missing-thing)
    (raise-argument-error 'raise-thing-quality-missing-error
                          "thing?"
                          missing-thing))
  (unless (symbol? missing-quality)
    (raise-argument-error 'raise-thing-quality-missing-error
                          "symbol?"
                          missing-quality))
  (raise (exn:qtmud:thing:quality:missing
          (format "~a tried to use ~a's non-existent ~a quality."
                  trigger-procedure
                  (thing-name missing-thing)
                  missing-quality)
          (current-continuation-marks))))
