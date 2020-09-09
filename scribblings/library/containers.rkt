#lang scribble/manual

@(require (for-label racket)
          (for-label "../../main.rkt")
	  (for-label "../../library/containers.rkt"))

@title{Containers}

@author{emsenn}

@defproc[(add-container-to-thing! [contained-thing thing?])
         (void)]{
Adds the @racket[container] quality to the @racket[contained-thing], setting it to @racket[void].
}
