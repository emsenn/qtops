#lang scribble/manual

@(require scribble/eval
	  (for-label racket)
	  (only-in (for-label "../../things.rkt"))
	  (only-in (for-label "../../qualities/description.rkt")))

@(define qtops-description-quality-eval (make-base-eval))
@interaction-eval[#:eval qtops-description-quality-eval
                  (require "things.rkt"
		  	   "qualities/description.rkt")]

@title[#:tag "description"]{Description}

@defmodule[qtops/qualities/description]

The @emph{description} quality


@examples[
	#:eval qtops-description-quality-eval

]
