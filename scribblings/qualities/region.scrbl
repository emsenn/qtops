#lang scribble/manual

@(require scribble/eval
	  (for-label racket)
	  (only-in (for-label "../../things.rkt"))
	  (only-in (for-label "../../qualities/region.rkt")))

@(define qtops-region-quality-eval (make-base-eval))
@interaction-eval[#:eval qtops-region-quality-eval
                  (require "things.rkt"
		  	   "qualities/region.rkt")]

@title[#:tag "region"]{region}

@defmodule[qtops/qualities/region]

The @emph{region} quality


@examples[
	#:eval qtops-region-quality-eval

]
