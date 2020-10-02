#lang scribble/manual

@(require scribble/eval
	  (for-label racket)
	  (only-in (for-label "../../things.rkt"))
	  (only-in (for-label "../../qualities/noise.rkt")))

@(define qtops-noise-quality-eval (make-base-eval))
@interaction-eval[#:eval qtops-noise-quality-eval
                  (require "things.rkt"
		  	   "qualities/noise.rkt")]

@title[#:tag "noise"]{noise}

@defmodule[qtops/qualities/noise]

The @emph{noise} quality


@examples[
	#:eval qtops-noise-quality-eval

]
