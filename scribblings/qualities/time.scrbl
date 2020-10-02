#lang scribble/manual

@(require scribble/eval
	  (for-label racket)
	  (only-in (for-label "../../things.rkt"))
	  (only-in (for-label "../../qualities/time.rkt")))

@(define qtops-time-quality-eval (make-base-eval))
@interaction-eval[#:eval qtops-time-quality-eval
                  (require "things.rkt"
		  	   "qualities/time.rkt")]

@title[#:tag "time"]{time}

@defmodule[qtops/qualities/time]

The @emph{time} quality


@examples[
	#:eval qtops-time-quality-eval

]
