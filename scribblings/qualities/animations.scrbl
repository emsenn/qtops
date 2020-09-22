#lang scribble/manual

@(require scribble/eval
	  (for-label racket)
	  (only-in (for-label "../../things.rkt"))
	  (only-in (for-label "../../qualities/animation.rkt")))

@(define qtops-animation-quality-eval (make-base-eval))

@interaction-eval[#:eval qtops-animation-quality-eval
                  (require "./things.rkt")]

@title[#:tag "animation"]{animation}

@defmodule[qtops/qualities/animation]

@defproc[(animations [t procedure?]) procedure?]{
Returns a procedure for @racket[t] which in turn returns a list.
}

@defproc[(set-animations [t procedure?]) procedure?]{
Returns a procedure for @racket[t] which accepts one argument, a @racket[list]. Each item on the list is expected to take the following format:

@racket[(list procedure integer integer)]
}
@examples[
	#:eval qtops-animation-quality-eval
	(define cedar (create-thing "cedar"))
]
