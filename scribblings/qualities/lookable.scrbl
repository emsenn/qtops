#lang scribble/manual

@(require scribble/eval
	  (for-label racket)
	  (only-in (for-label "../../things.rkt"))
	  (only-in (for-label "../../qualities/lookable.rkt")))

@(define qtops-lookable-quality-eval (make-base-eval))
@interaction-eval[#:eval qtops-lookable-quality-eval
                  (require "things.rkt"
		  	   "qualities/lookable.rkt")]

@title[#:tag "lookable"]{Lookable}

@defmodule[qtops/qualities/lookable]

The @emph{lookable} quality


@examples[
	#:eval qtops-lookable-quality-eval

]
