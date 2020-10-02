#lang scribble/manual

@(require scribble/eval
	  (for-label racket)
	  (only-in (for-label "../../things.rkt"))
	  (only-in (for-label "../../qualities/lit.rkt")))

@(define qtops-lit-quality-eval (make-base-eval))
@interaction-eval[#:eval qtops-lit-quality-eval
                  (require "things.rkt"
		  	   "qualities/lit.rkt")]

@title[#:tag "lit"]{Lit}

@defmodule[qtops/qualities/lit]

The @emph{lit} quality


@examples[
	#:eval qtops-lit-quality-eval

]
