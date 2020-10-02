#lang scribble/manual

@(require scribble/eval
	  (for-label racket)
	  (only-in (for-label "../../things.rkt"))
	  (only-in (for-label "../../qualities/trivia.rkt")))

@(define qtops-trivia-quality-eval (make-base-eval))
@interaction-eval[#:eval qtops-trivia-quality-eval
                  (require "things.rkt"
		  	   "qualities/trivia.rkt")]

@title[#:tag "trivia"]{trivia}

@defmodule[qtops/qualities/trivia]

The @emph{trivia} quality


@examples[
	#:eval qtops-trivia-quality-eval

]
