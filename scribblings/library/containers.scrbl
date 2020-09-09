#lang scribble/manual

@(require (for-label racket)
          (for-label "../../main.rkt")
	  (for-label "../../library/containers.rkt"))

@title[#:tag "containers"]{Containers}

@author{emsenn}

The @emph{@bold{Containers}} component of the qtMUD Library provides procedures for two qualities: container and contents.

A thing's contents is a list of things, and a things container is a thing: a thing in the contents of another is expected to have that as its container.

@racketblock[
 (define containerverse (create-universe))
 (add-container-procedures-to-universe! containerverse)
 (define create-thing
  (create-thing-creator-for-universe containerverse))

 (define apple (create-thing "apple"))
 (define basket (create-thing "basket"))

 (add-quality-to-thing! 'container apple)
 (add-quality-to-thing! 'contents basket)
 ((universe-procedure containerverse 'move-thing-into-thing)
  apple basket)

 (thing-name (thing-quality apple 'container))
 ;; basket
 (list-thing-names (thing-quality basket 'contents))
 ;; apple
]


@defmodule[qtmud/library/containers]

@defproc[(add-container-to-thing! [containee thing?])
         (void)]{
Adds the @racket[container] quality to the @racket[containee], setting it to @racket[void].
}

@defproc[(set-thing-container! [containee thing?]
                               [container thing?])
	 (void)]

@defproc[(add-contents-to-thing! [container thing?])
         (void)]

@defproc[(set-thing-contents! [container thing?]
			      [contents (listof thing?)])
	 (void)]

@defproc[(add-element-to-thing-contents! [item thing?]
					 [container thing?])
	 (void)]

@defproc[(remove-element-from-thing-contents! [item thing?]
					      [container thing?])
	 (void)]

@defproc[(move-thing-from-thing! [mover thing?]
				 [origin thing?])
	 (void)]

@defproc[(move-thing-into-thing! [mover thing?]
				 [destination thing?])
	 (void)]

@defproc[(move-things-into-things! [movers (listof thing?)]
				   [destination thing?])
	 (void)]

@defproc[(add-container-procedures-to-thing! [changed-thing thing?])
         (void)]

@defproc[(add-container-procedures-to-universe!
          [changed-universe universe?])
         (void)]
