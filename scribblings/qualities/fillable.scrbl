#lang scribble/manual

@(require scribble/eval
	  (for-label racket)
	  (only-in (for-label "../../things.rkt"))
	  (only-in (for-label "../../qualities/fillable.rkt")))

@(define qtops-fillable-quality-eval (make-base-eval))
@interaction-eval[#:eval qtops-fillable-quality-eval
                  (require "things.rkt"
		  	   "qualities/fillable.rkt")]

@title[#:tag "fillable"]{Fillable}

@defmodule[qtops/qualities/fillable]

The @emph{fillable} quality


@examples[
	#:eval qtops-fillable-quality-eval

]
