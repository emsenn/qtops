#lang scribble/manual

@(require scribble/eval
	  (for-label racket)
	  (only-in (for-label "../../things.rkt"))
	  (only-in (for-label "../../qualities/pronouns.rkt")))

@(define qtops-pronouns-quality-eval (make-base-eval))
@interaction-eval[#:eval qtops-pronouns-quality-eval
                  (require "things.rkt"
		  	   "qualities/pronouns.rkt")]

@title[#:tag "pronouns"]{pronouns}

@defmodule[qtops/qualities/pronouns]

The @emph{pronouns} quality


@examples[
	#:eval qtops-pronouns-quality-eval

]
