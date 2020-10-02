#lang scribble/manual

@(require scribble/eval
	  (for-label racket)
	  (only-in (for-label "../../things.rkt"))
	  (only-in (for-label "../../qualities/contents.rkt")))

@(define qtops-contents-quality-eval (make-base-eval))
@interaction-eval[#:eval qtops-contents-quality-eval
                  (require "things.rkt"
		  	   "qualities/contents.rkt")]

@title[#:tag "contents"]{Contents}

@defmodule[qtops/qualities/contents]

The @emph{contents} quality


@examples[
	#:eval qtops-contents-quality-eval

]
