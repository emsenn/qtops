#lang scribble/manual

@(require scribble/eval
	  (for-label racket)
	  (only-in (for-label "../../things.rkt"))
	  (only-in (for-label "../../qualities/radiation.rkt")))

@(define qtops-radiation-quality-eval (make-base-eval))
@interaction-eval[#:eval qtops-radiation-quality-eval
                  (require "things.rkt"
		  	   "qualities/radiation.rkt")]

@title[#:tag "radiation"]{radiation}

@defmodule[qtops/qualities/radiation]

The @emph{radiation} quality


@examples[
	#:eval qtops-radiation-quality-eval

]
