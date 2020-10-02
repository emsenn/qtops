#lang scribble/manual

@(require scribble/eval
	  (for-label racket)
	  (only-in (for-label "../../things.rkt"))
	  (only-in (for-label "../../qualities/mudsocket-client.rkt")))

@(define qtops-mudsocket-client-quality-eval (make-base-eval))
@interaction-eval[#:eval qtops-mudsocket-client-quality-eval
                  (require "things.rkt"
		  	   "qualities/mudsocket-client.rkt")]

@title[#:tag "mudsocket-client"]{Mudsocket-Client}

@defmodule[qtops/qualities/mudsocket-client]

The @emph{mudsocket-client} quality


@examples[
	#:eval qtops-mudsocket-client-quality-eval

]
