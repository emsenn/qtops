#lang scribble/manual

@(require scribble/eval
	  (for-label racket)
	  (only-in (for-label "../../things.rkt"))
	  (only-in (for-label "../../qualities/universe-thing.rkt")))

@(define qtops-universe-thing-quality-eval (make-base-eval))
@interaction-eval[#:eval qtops-universe-thing-quality-eval
                  (require "things.rkt"
		  	   "qualities/universe-thing.rkt")]

@title[#:tag "universe-thing"]{universe-thing}

@defmodule[qtops/qualities/universe-thing]

The @emph{universe-thing} quality


@examples[
	#:eval qtops-universe-thing-quality-eval

]
