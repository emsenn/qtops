#lang scribble/manual

@(require scribble/eval
	  (for-label racket)
	  (only-in (for-label "../../things.rkt"))
	  (only-in (for-label "../../qualities/sight.rkt")))

@(define qtops-sight-quality-eval (make-base-eval))
@interaction-eval[#:eval qtops-sight-quality-eval
                  (require "things.rkt"
		  	   "qualities/sight.rkt")]

@title[#:tag "sight"]{sight}

@defmodule[qtops/qualities/sight]

The @emph{sight} quality


@examples[
	#:eval qtops-sight-quality-eval

]
