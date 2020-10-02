#lang scribble/manual

@(require scribble/eval
	  (for-label racket)
	  (only-in (for-label "../../things.rkt"))
	  (only-in (for-label "../../qualities/noun.rkt")))

@(define qtops-noun-quality-eval (make-base-eval))
@interaction-eval[#:eval qtops-noun-quality-eval
                  (require "things.rkt"
		  	   "qualities/noun.rkt")]

@title[#:tag "noun"]{noun}

@defmodule[qtops/qualities/noun]

The @emph{noun} quality


@examples[
	#:eval qtops-noun-quality-eval

]
