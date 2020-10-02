#lang scribble/manual

@(require scribble/eval
	  (for-label racket)
	  (only-in (for-label "../../things.rkt"))
	  (only-in (for-label "../../qualities/universe.rkt")))

@(define qtops-universe-quality-eval (make-base-eval))
@interaction-eval[#:eval qtops-universe-quality-eval
                  (require "things.rkt"
		  	   "qualities/universe.rkt")]

@title[#:tag "universe"]{Universe}

@defmodule[qtops/qualities/universe]

The @emph{universe} quality provides a thing with, at the moment, one capability:

They can @racket{create-thing!}, and that thing will be added to a list of @racket{things} the universe maintains. @emph{And, those things will have a @racket{create-thing^!} procedure that provides an equivalent operation.}

  @history[#:added "0.1"
           #:changed "0.1.1" @elem{@racket[<>universe] now accepts @emph{things} keyword, which accepts a list of quality-application procedures.}]

@defproc*[([(things [t procedure?]) procedure?]
           [(things) list?])]{
Returns a procedure which follows the second form, where the @racket[list] is a list of @racket[things]. @racket[t] is assumed to be a thing.
}

@examples[
	#:eval qtops-universe-quality-eval

]
