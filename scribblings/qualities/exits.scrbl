#lang scribble/manual

@(require scribble/eval
	  (for-label racket)
	  (only-in (for-label "../../things.rkt"))
	  (only-in (for-label "../../qualities/exits.rkt")))

@(define qtops-exits-quality-eval (make-base-eval))
@interaction-eval[#:eval qtops-exits-quality-eval
                  (require "things.rkt"
		  	   "qualities/exits.rkt")]

@title[#:tag "exits"]{Exits}

@defmodule[qtops/qualities/exits]

The @emph{exits} quality


@examples[
	#:eval qtops-exits-quality-eval

]
