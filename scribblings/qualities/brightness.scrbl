#lang scribble/manual

@(require scribble/eval
	  (for-label racket)
	  (only-in (for-label "../../things.rkt"))
	  (only-in (for-label "../../qualities/brightness.rkt")))

@(define qtops-brightness-quality-eval (make-base-eval))
@interaction-eval[#:eval qtops-brightness-quality-eval
                  (require "things.rkt"
		  	   "qualities/brightness.rkt")]

@title[#:tag "brightness"]{Brightness}

@defmodule[qtops/qualities/brightness]

The @emph{animation} quality provides a thing with a measure of how @quote{bright} they are: how much light they put out. This is might be used by things with a relationship to the bright thing to determine how lit they are, for example.

@examples[
	#:eval qtops-brightness-quality-eval

]
