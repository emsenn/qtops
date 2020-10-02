#lang scribble/manual

@(require scribble/eval
	  (for-label racket)
	  (only-in (for-label "../../things.rkt"))
	  (only-in (for-label "../../qualities/region-area.rkt")))

@(define qtops-region-area-quality-eval (make-base-eval))
@interaction-eval[#:eval qtops-region-area-quality-eval
                  (require "things.rkt"
		  	   "qualities/region-area.rkt")]

@title[#:tag "region-area"]{region-area}

@defmodule[qtops/qualities/region-area]

The @emph{region-area} quality


@examples[
	#:eval qtops-region-area-quality-eval

]
