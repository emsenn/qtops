#lang scribble/manual

@(require scribble/eval
	  (for-label racket)
	  (only-in (for-label "../../things.rkt"))
	  (only-in (for-label "../../qualities/mudsocket-trivia.rkt")))

@(define qtops-mudsocket-trivia-quality-eval (make-base-eval))
@interaction-eval[#:eval qtops-mudsocket-trivia-quality-eval
                  (require "things.rkt"
		  	   "qualities/mudsocket-trivia.rkt")]

@title[#:tag "mudsocket-trivia"]{Mudsocket-trivia}

@defmodule[qtops/qualities/mudsocket-trivia]

The @emph{mudsocket-trivia} quality


@examples[
	#:eval qtops-mudsocket-trivia-quality-eval

]
