#lang scribble/manual

@(require scribble/eval
	  (for-label racket)
	  (only-in (for-label "../../things.rkt"))
	  (only-in (for-label "../../qualities/mass.rkt")))

@(define qtops-mass-quality-eval (make-base-eval))
@interaction-eval[#:eval qtops-mass-quality-eval
                  (require "things.rkt"
		  	   "qualities/mass.rkt")]

@title[#:tag "mass"]{Mass}

@defmodule[qtops/qualities/mass]

The @emph{mass} quality


@examples[
	#:eval qtops-mass-quality-eval

]
