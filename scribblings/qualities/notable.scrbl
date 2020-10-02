#lang scribble/manual

@(require scribble/eval
	  (for-label racket)
	  (only-in (for-label "../../things.rkt"))
	  (only-in (for-label "../../qualities/notable.rkt")))

@(define qtops-notable-quality-eval (make-base-eval))
@interaction-eval[#:eval qtops-notable-quality-eval
                  (require "things.rkt"
		  	   "qualities/notable.rkt")]

@title[#:tag "notable"]{Notable}

@defmodule[qtops/qualities/notable]

The @emph{notable} quality


@examples[
	#:eval qtops-notable-quality-eval

]
