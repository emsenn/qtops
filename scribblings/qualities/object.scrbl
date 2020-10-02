#lang scribble/manual

@(require scribble/eval
	  (for-label racket)
	  (only-in (for-label "../../things.rkt"))
	  (only-in (for-label "../../qualities/object.rkt")))

@(define qtops-object-quality-eval (make-base-eval))
@interaction-eval[#:eval qtops-object-quality-eval
                  (require "things.rkt"
		  	   "qualities/object.rkt")]

@title[#:tag "object"]{object}

@defmodule[qtops/qualities/object]

The @emph{object} quality


@examples[
	#:eval qtops-object-quality-eval

]
