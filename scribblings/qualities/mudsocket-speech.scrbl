#lang scribble/manual

@(require scribble/eval
	  (for-label racket)
	  (only-in (for-label "../../things.rkt"))
	  (only-in (for-label "../../qualities/mudsocket-speech.rkt")))

@(define qtops-mudsocket-speech-quality-eval (make-base-eval))
@interaction-eval[#:eval qtops-mudsocket-speech-quality-eval
                  (require "things.rkt"
		  	   "qualities/mudsocket-speech.rkt")]

@title[#:tag "mudsocket-speech"]{Mudsocket-speech}

@defmodule[qtops/qualities/mudsocket-speech]

The @emph{mudsocket-speech} quality


@examples[
	#:eval qtops-mudsocket-speech-quality-eval

]
