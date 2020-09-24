#lang racket

(require "region.rkt")

(provide <>metaregion)

(define ((>link-two-areas^! t) k1 d1 d2 k2)
  (when (and (t 'area? (car k1))
             (t 'area? (car k2)))
    (((t 'area (car k1)) 'area (cdr k1))
     'set-exit! d1
     ((t 'area (car k2)) 'area (cdr k2)))))

(define (<>metaregion t)
  (unless (t 'has-procedure? 'areas)
    (t 'set-procedures! (>>make-region-procedures t)))
  (t 'set-procedure! 'link-two-areas^!
     (>link-two-areas^! t)))
