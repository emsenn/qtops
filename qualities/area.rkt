#lang racket

(require "description.rkt"
         "contents.rkt"
         "exits.rkt")

(provide <>area)

(define (<>area t
                #:name [name #f]
                #:description [description #f]
                #:contents [contents #f]
                #:exits [exits #f])
  (log-debug "Building area ~a~a"
             (t 'name)
             (if name (format " (to be named ~a)" name)
                 ""))
  (when name (t 'set-name! name))
  (unless (t 'has-procedure? 'description)
    (t 'set-procedures! (>>make-description-procedures t)))
  (when description (t 'set-description! description))
  (unless (t 'has-procedure? 'contents)
    (t 'set-procedures! (>>make-content-procedures t)))
  (when contents (t 'fill-contents! contents))
  (unless (t 'has-procedure? 'exits)
    (t 'set-procedures! (>>make-exit-procedures t)))
  (when exits (t 'set-exits! exits))
  t)
