#lang racket

(provide make-client-procedures)

(define ((output-buffer t)) "")
(define ((set-output-buffer! t) b)
  (log-debug "Setting output-buffer of ~a to ~a" t b)
  (t 'set-procedure! 'output-buffer (λ () b)))
(define ((append-output-buffer! t) a)
  (t 'set-output-buffer!
     (string-join (list (t 'output-buffer) a) "")))
(define ((clear-output-buffer! t))
  (t 'set-output-buffer! ""))
(define ((message! t) m)
  (t 'append-output-buffer! m))

(define (make-client-procedures t)
  (log-debug "Making client procedures for ~a" t)
  (list
   (cons 'output-buffer (output-buffer t))
   (cons 'set-output-buffer! (set-output-buffer! t))
   (cons 'append-output-buffer! (append-output-buffer! t))
   (cons 'clear-output-buffer! (clear-output-buffer! t))
   (cons 'message! (message! t))))
