#lang scribble/manual

@(require scribble/eval
	  (for-label racket)
	  (only-in (for-label "../../things.rkt"))
	  (only-in (for-label "../../qualities/region-noise.rkt")))

@(define qtops-region-noise-quality-eval (make-base-eval))
@interaction-eval[#:eval qtops-region-noise-quality-eval
                  (require "things.rkt"
		  	   "qualities/region-noise.rkt")]

@title[#:tag "region-noise"]{region-noise}

@defmodule[qtops/qualities/region-noise]

The @emph{region-noise} quality


@examples[
	#:eval qtops-region-noise-quality-eval

]
