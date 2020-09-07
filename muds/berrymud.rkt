#lang racket

(require "../qtmud/main.rkt"
         "../qtmud/library/containers.rkt")




(define (harvest-thing harvested-thing)
  (unless (thing-has-quality? harvested-thing
                              'foragable)
    (raise-thing-quality-missing-error 'harvest-thing
                                       harvested-thing
                                       'foragable))
  (define harvested-thing-foragable-count
    (thing-quality-head harvested-thing 'foragable))
  (define harvested-thing-foragable-item
    (thing-quality-tail harvested-thing 'foragable))
  (set-thing-quality! harvested-thing
                      'foragable
                      (cons (- harvested-thing-foragable-count 1)
                            harvested-thing-foragable-item))
  (harvested-thing-foragable-item))

(define berryverse (create-universe "Berryverse"))
(define create-thing (create-thing-creator-for-universe berryverse))
(add-container-procedures-to-universe! berryverse)
(define bush (create-thing "blueberry bush"
                           #:grammar
                           (list (cons 'adjectives '("berry")))))
(add-quality-to-thing! 'foragable bush)
(set-thing-quality!
 bush 'foragable
 (cons 100 (λ ()
             (define blueberry (create-thing "blueberry"))
             (add-quality-to-thing! 'container blueberry)
             blueberry)))
(define basket (create-thing "basket"))
(add-quality-to-thing! 'contents basket)
(move-thing-into-thing! (harvest-thing bush) basket)
(printf "Basket's contents are ~a.\n"
        (list-thing-names (thing-quality basket 'contents)))

(define field (create-thing "field"))
(add-quality-to-thing! 'contents field)
(define stone (create-thing "stone"
                            #:grammar
                            (list
                             (cons 'adjectives
                                   '("large")))))
(set-thing-contents! field
                     (list stone))
(printf "Field's name equals? \"field\"? ~a\n"
        (thing-name=? field "field"))

(add-quality-to-thing! 'container bush)
(move-thing-into-thing! bush field)

(printf "Field contains ~a.\n"
        (list-thing-names (thing-quality field 'contents)))

(printf "Fields contents with the foragable quality are: ~a\n"
        (list-thing-names
         (things-with-quality (thing-quality field
                                             'contents)
                              'foragable)))

(thing-matches-term? bush "blueberry bush")
(thing-name bush)
(thing-matches-term? bush "berry bush")
(thing-grammar bush)
(thing-matches-term? stone "large stone")
(thing-matches-term? stone "big stone")
