#lang racket

(require "description.rkt"
         "fillable.rkt"
         "region-area.rkt")

(provide <>region
         >>make-region-procedures)

(define (>areas t)
  (define real-areas (make-hash))
  (λ () real-areas))
(define ((>area? t) k) (hash-has-key? (t 'areas) k))
(define ((>area t) k) (hash-ref (t 'areas) k))
(define ((>set-areas! t) A)
  (hash-map
   A
   (λ (k v) (hash-set! (t 'areas) k v))))
(define ((>set-area! t) k a)
  (hash-set! (t 'areas) k a))
(define ((>remove-area! t) a)
  (hash-remove (t 'areas) a))
(define ((>fill-areas! t) a)
  (hash-map
   a
   (λ (k v)
     (log-debug "Filling ~a with ~a ~a"
                (t 'name) k v)
     (define w
       (first
       (t 'fill-quality!
               'areas v
               'region >>make-region-area-procedures)))
     (log-debug "w is ~a" w)
     (log-debug "FOOBAR Adding ~a to areas of ~a as ~a"
                (w 'name) (t 'name) k)
     (t 'set-area! k w)
     (w 'set-region! t)
     (when (t 'has-procedure? 'contents)
       (t 'add-content! w)))))

(define ((>link-two-areas^! t) k1 d1 d2 k2)
  (when (and (t 'area? k1)
             (t 'area? k2))
    ((t 'area k1) 'set-exit! d1 (t 'area k2))
    ((t 'area k2) 'set-exit! d2 (t 'area k1))))

(define ((>link-areas^! t) A)
  (map
   (λ (a)
     (apply (t 'procedure 'link-two-areas^!) a))
   A))

(define (>>make-region-procedures t)
  (define RP
    ((λ ()
      (define rp '())
      (λ ([r #f]) (if r (set! rp (append rp r)) rp)))))
  (unless (t 'has-procedure? 'fill-quality!)
    (RP (>>make-fillable-procedures t)))
  (RP
   (list
    (cons 'areas (>areas t))
    (cons 'set-areas! (>set-areas! t))
    (cons 'set-area! (>set-area! t))
    (cons 'area? (>area? t))
    (cons 'area (>area t))
    (cons 'remove-area! (>remove-area! t))
    (cons 'fill-areas! (>fill-areas! t))
    (cons 'link-two-areas^! (>link-two-areas^! t))
    (cons 'link-areas^! (>link-areas^! t))))
  (RP))

(define (<>region t
                  #:name [name #f]
                  #:description [description #f]
                  #:areas [areas #f]
                  #:links [links #f])
  (when name (t 'set-name! name))
  (unless (t 'has-procedure? 'description)
    (t 'set-procedures! (>>make-description-procedures t)))
  (unless (t 'has-procedure? 'areas)
    (t 'set-procedures! (>>make-region-procedures t)))
  (when areas (t 'fill-areas! (make-hash areas)))
  (when links (t 'link-areas^! links))
  t)
